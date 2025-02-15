# ᕦ(ツ)ᕤ
# codegen.py
# author: asnaroo
# zero to anything

from typing import Dict
from symbols import *
from entity import *
from copy import deepcopy
import subprocess
import struct
import array

#--------------------------------------------------------------------------------------------------

# Var is a virtual variable that can be held in a register or spilled to memory
class Var:
    def __init__(self, name: str, type: str):
        self.name = name                    # name in original code
        self.type = type                    # concrete type, eg. "i32", "i64", "f32", "f64" etc
        self.live_range = (-1, -1)          # instruction index of first write and last read
        self.register = None                # physical register (dynamic)
        self.spill_slot = None            # byte-offset in spill-memory (dynamic)
    def __str__(self): return f"{self.name}"
    def __repr__(self): return str(self)

# Const is a constant that can be i/f<nbits>, eg. i32, f32, etc
class Const:
    def __init__(self, value: str, type: str):
        self.value = value                    # value as a string
        self.type = type                      # concrete type, eg. "i32", "i64", "f32", "f64" etc   
    def __str__(self): return f"{self.value}"
    def __repr__(self): return str(self)

# instruction: can be either VM or processor specific
class Instruction:
    def __init__(self, opcode: str, dests: List[Var], sources: List[Var|Const], comment: str = ""):
        self.opcode : str = opcode                      # opcode
        self.dests : List[Var] = dests                  # destination variables
        self.sources : List[Var|Const] = sources        # source variables or constants
        self.comment : str = comment                    # comment
    def __str__(self):
        line = f"{self.opcode} {', '.join([str(var) for var in self.dests])}, {', '.join([str(var) for var in self.sources])}"
        return line
    def __repr__(self): return str(self)
    def show(self) -> str:
        return f"{self.dests[0].name} <= {self.opcode} {', '.join([str(var) for var in self.sources])}"
       
# instruction block : just a list of instructions
class InstructionBlock:
    def __init__(self):
        self.instructions = []
        self.vars = {}              # str => Var
        self.constants = {}         # str => Const
        self.labels = {}            # str => instruction index
        self.max_live = 0           # maximum number of live variables at any point
        self.dbg = False            # set to True to generate debug code
        
    def __str__(self):
        max_length = max(len(var.name) for var in self.vars.values())
        max_length += max((len(var.type) for var in self.vars.values()), default=0)
        max_digits = len(str(len(self.instructions)))
        max_length += max_digits + 26 # log_grey adds some extra chars

        out = ""
        for i, instr in enumerate(self.instructions):
            ii = log_grey(f"{i:{max_digits}}: ")
            type = log_grey(f"{instr.dests[0].type}")
            lhs = f"{ii}{type} {instr.dests[0].name}"
            lhs += " " * (max_length - len(lhs))
            lhs += f"{log_grey("<=")} {instr.opcode} {', '.join([str(var) for var in instr.sources])}\n"
            out += lhs
        out += f"max_live: {self.max_live}\n"
        return out
    
    def __repr__(self):
        return str(self)
    
    # find the index of the last instruction that writes to (var)
    def find_last_write(self, i_instruction, var: Var):
        i = i_instruction -1
        while i >= 0 and var not in self.instructions[i].dests:
            i -= 1
        return i
    
    # find the index of the next instruction that reads (var)
    def find_next_read(self, i_instruction: int, var: Var):
        i = i_instruction + 1
        while i < len(self.instructions) and var not in self.instructions[i].sources:
            i += 1
        return i
    
    # find the index of the last instruction that reads (var)
    def find_last_read(self, var: Var):
        for i in range(len(self.instructions) - 1, -1, -1):
            if var in self.instructions[i].sources:
                return i
        return -1
    
    # add a label at the current index
    def label(self, name: str):
        self.labels[name] = len(self.instructions)

#--------------------------------------------------------------------------------------------------
# configuration options for code production

class CodegenConfig:
    def __init__(self):
        self.type_substitution = {}

    # add mappings from abstract type names to concrete type names, eg. "number" -> "f32"
    def concrete_types(self, type_map: Dict[str, str]):
        self.type_substitution.update(type_map)
        log(f"concrete_types: {self.type_substitution}")

    # get the concrete type name for an abstract type name, eg. "number" -> "f32"
    def get_concrete_type(self, abstract_type_name: str) -> str:
        return self.type_substitution.get(abstract_type_name, None)

#--------------------------------------------------------------------------------------------------
# Generates VM code from AST (methods called by zero.py)

class CodeGenerator:
    def __init__(self):
        self.config = None
        self.st = None
        self.grammar = None
        self.indent = 0
        self.i_var = 0
        self.block = InstructionBlock()

    def setup(self, config: CodegenConfig, st: SymbolTable, grammar: Grammar):
        self.config = config
        self.st = st
        self.grammar = grammar

    # get the first entity of the required type with the given name
    def find_entity(self, key: str, of_type: Any) -> Entity:
        items = self.st.find(key, of_type, None, True)
        return items[0].element if len(items) == 1 else None

    # clear everything out
    def reset(self):
        self.block = InstructionBlock()
        self.i_var = 0
        self.indent = 0
    
    # allocate a new variable, return its unique identifier (index)
    def alloc_var_index(self) -> int:
        result = self.i_var
        self.i_var += 1
        return result
    
    # add a new VM Instruction to the current block
    def output(self, opcode, dests: List[Lex], sources: List[Lex]):   
        dests = [str(var) for var in dests]
        sources = [str(var) for var in sources]
        txt = "    "*self.indent + f"{opcode} {', '.join(dests)}, {', '.join(sources)}\n"
        log(log_green(txt))
        dests = [self.block.vars[var_str] for var_str in dests]
        sources = [(self.block.vars[var_str] if "_" in var_str else Const(var_str, dests[0].type)) for var_str in sources ]
        self.block.instructions.append(Instruction(opcode, dests, sources))

    # add a new VM variable to the current block (name should contain index)
    def add_var(self, var_name, type):
        var_name = str(var_name)
        type = str(type)
        concrete_type_name = self.config.get_concrete_type(str(type))
        log(f"assert_type {var_name} {type} => {concrete_type_name}")
        if var_name in self.block.vars: raise Exception(f"variable {var_name} already exists")
        self.block.vars[var_name] = Var(var_name, concrete_type_name)

    # given a name of format a.b.c, try to replace the first part with a value from the replace map
    def try_replace(self, var, replace):
        if "." in var:
            vars = var.split(".")
            if vars[0] in replace:
                return replace[vars[0]] + "." + ".".join(vars[1:])
        else:
            if var in replace: return replace[var]
        return var

    # show the current block as a string
    def show(self, e):
        return print_code_formatted(e, self.grammar).replace("\n", "↩︎").replace("    ", "")

#--------------------------------------------------------------------------------------------------
# optimiser - works at VM level before backend does its thing

class Optimiser:
    def __init__(self, block: InstructionBlock):
        self.block = block
    
    # apply various optimisations (this will grow over time)
    def optimise(self):
        self.measure_pressure()
        log_clear()
        log("\nbefore optimisation:")
        log(str(self.block))
        self.optimise_movs()
        self.optimise_live_ranges()
        self.optimise_lds()
        self.measure_pressure()
        self.compute_live_ranges()
        return self

    # move instructions back to be as close as possible to where their source operands were written
    def optimise_live_ranges(self):
        safe_count = 0
        while safe_count < 10:
            safe_count += 1
            n_moves = self.try_moves()
            if n_moves == 0: break
        return self
    
    # remove superfluous 'mov' instructions
    def optimise_movs(self):
        to_remove = []
        vars_to_replace = {}
        for i, instruction in enumerate(self.block.instructions):
            if instruction.opcode == "mov":
                i_source = self.block.find_last_write(i, instruction.sources[0])
                self.block.instructions[i_source].dests[0] = instruction.dests[0]
                vars_to_replace[instruction.sources[0]] = instruction.dests[0]
                to_remove.append(i)
        for i in sorted(to_remove, reverse=True):
            del self.block.instructions[i]
        for i, instruction in enumerate(self.block.instructions):
            for j, var in enumerate(instruction.sources):
                if var in vars_to_replace:
                    instruction.sources[j] = vars_to_replace[var]
        return self
    
    # move 'ld' instructions forward to be as close as possible to where their dests are read
    def optimise_lds(self):
        for i in range(len(self.block.instructions) - 1, -1, -1):
            instruction = self.block.instructions[i]
            if instruction.opcode != "const": continue
            i_read = self.block.find_next_read(i, instruction.dests[0])
            if i_read < len(self.block.instructions):
                i_dest = i_read - 1
                if i_dest > i:
                    self.block.instructions = self.block.instructions[:i] + self.block.instructions[i+1:i_dest] + [instruction] + self.block.instructions[i_dest:]
        return self
    
    # compute live ranges for all variables
    def compute_live_ranges(self):
        for i in range(len(self.block.instructions)):
            instruction = self.block.instructions[i]
            for var in instruction.dests:
                i_last_read = self.block.find_last_read(var)
                var.live_range = (i, i_last_read)
    
    #----------------------------------------------------------------------------------------------
    # below the line
    
    # figure out where an instruction can be moved back to, or -1 if not possible
    def try_move(self, i_instruction) -> int:
        this_instruction = self.block.instructions[i_instruction]
        i_instructions = [self.block.find_last_write(i_instruction, var) for var in this_instruction.sources]
        i_can_move = max(i_instructions)
        if i_can_move >= 0 and (i_can_move+1) < i_instruction:
            #log(f"moving {i_instruction}:{this_instruction} to {i_can_move+1}")
            i_move_to = i_can_move + 1
            tmp = self.block.instructions[:i_instruction] + self.block.instructions[i_instruction+1:]
            self.block.instructions = tmp[:i_move_to] + [this_instruction] + tmp[i_move_to:]
            return i_move_to
        return -1
    
    # try and move each instruction back as close to its source operands generators as possible
    def try_moves(self) -> int: # return number of moves made
        n_moves = 0
        for i in range(0, len(self.block.instructions)):
            i_moved_to = self.try_move(i)
            if i_moved_to >= 0: n_moves += 1
        return n_moves
    
    # measure the maximum number of live variables at any point in th block
    def measure_pressure(self):
        max_pressure = 0
        live_vars = set()
        for i in range(len(self.block.instructions)):
            instruction = self.block.instructions[i]
            live_vars.update(instruction.dests)
            for src_var in instruction.sources:
                i_last_read = self.block.find_last_read(src_var)
                if i_last_read == i and src_var in live_vars:
                    live_vars.remove(src_var)
                max_pressure = max(max_pressure, len(live_vars))
            #log(f"live_vars {i}: {live_vars} ({len(live_vars)})")
        self.block.max_live = max_pressure
        return self
    
#--------------------------------------------------------------------------------------------------
# Backend generates actual code for a platform

class Backend:
    def __init__(self, path: str): self.path = path
    def generate(self, block: InstructionBlock) -> str: override_me()
    def run(self) -> str: return ""

#--------------------------------------------------------------------------------------------------
# Python backend runs numpy

class PythonBackend(Backend):
    def generate(self, block: InstructionBlock):
        out = self.header()
        op_map = { "add" : "+", "sub" : "-", "mul" : "*", "div" : "/", "sqrt" : "np.sqrt" }
        type_map = { "f32" : "np.float32", "i32" : "np.int32" }

        for instruction in block.instructions:
            dest = instruction.dest.name.replace(".", "_")
            if instruction.opcode == "const":
                type = type_map[instruction.dest.type]
                out += f"    {dest} = {type}({instruction.sources[0]})\n"
            else:
                opcode = op_map[instruction.opcode]
                operands = [var.name.replace(".", "_") for var in instruction.sources]
                if opcode in "+-*/":
                    out += f"    {dest} = {operands[0]} {opcode} {operands[1]}\n"
                elif len(operands) == 1:
                    out += f"    {dest} = {opcode}({operands[0]})\n"

        for instruction in block.instructions:
            dest = instruction.dest.name.replace(".", "_")
            out += f"    print(f\"{instruction.dest} = {{{dest}}}\")\n"
        out += self.footer()
        out = out.strip()
        log_clear()
        log("----------------------------------------------")
        log(out)
        write_file(self.path.replace(".*", ".py"), out)
    
    def run(self) -> str:
        result = subprocess.run(['python3', self.path], capture_output=True, text=True)
        return result.stdout.strip()
    
    #----------------------------------------------------------------------------------------------
    # below the line

    def header(self) -> str:
        return log_deindent(f"""
            # ᕦ(ツ)ᕤ
            # {self.path}
            # generated by zeta.py
                            
            import numpy as np
                    
            def run():
        """)
    
    def footer(self) -> str:
        return log_deindent("""
            if __name__ == "__main__":
                run()
        """)

#--------------------------------------------------------------------------------------------------
# processor Backend

class Register:
    def __init__(self, name: str, type: str, index: int):
        self.name = name
        self.type = type
        self.index = index
        self.contents: Var|Const = None
    def __str__(self): return f"{self.name}"
    def __repr__(self): return str(self)

class RegisterSet:
    def __init__(self, prefix: str, n_registers: int):
        self.type = type
        self.n_registers = n_registers
        self.registers = [Register(f"{prefix}{i}", type, i) for i in range(n_registers)]
    def __str__(self): return f"{self.type}"
    def __repr__(self): return str(self)
    def restrict(self, n_registers: int):
        dummy_var = Var("dummy", self.type)
        for i in range(n_registers, len(self.registers)):
            self.registers[i].contents = dummy_var
    
class Data:
    def __init__(self, name: str, type: str, size_bytes: int, value: bytes, comment: str = ""):
        self.name = name
        self.type = type
        self.size_bytes = size_bytes
        self.value = value
        self.comment = comment
        self.offset_bytes = None
    def __str__(self): return f"{self.name}"

class DataBlock:
    def __init__(self, name: str):
        self.address = 0
        self.size_bytes = 0
        self.data : List[Data] = []
        self.name_to_data : Dict[str, Data] = {}
    def allocate(self, name: str, type: str, size_bytes: int, value: bytes) -> Data:
        if name in self.name_to_data: return self.name_to_data[name]
        data = Data(name, type, size_bytes, value)
        data.offset_bytes = self.size_bytes
        self.data.append(data)
        self.size_bytes += size_bytes
        self.name_to_data[name] = data
        return data
    
#--------------------------------------------------------------------------------------------------
# implements RISCV32I with M and F extensions

class RISCV32(Backend):
    def __init__(self, path: str, dbg: bool = False):
        self.path = path
        self.block = InstructionBlock()
        self.dbg = dbg
        self.setup()

    def setup(self):
        int_registers = RegisterSet("x", 32)
        fp_registers = RegisterSet("f", 32)
        fp_registers.restrict(2)
        self.register_manager = RegisterManager( { "i" : int_registers, "u" : int_registers, "f" : fp_registers }, self)
        int_registers.registers[0].contents = Const("0", "u32")
        self.load_opcodes = { "u32" : "lw", "i32" : "lw", "f32" : "flw" }
        self.store_opcodes = { "u32" : "sw", "i32" : "sw", "f32" : "fsw" }
        self.arithmetic_opcodes = { "add", "sub", "mul", "div" }
        self.i_prologue = None
        self.i_epilogue = None

    def generate(self, block: InstructionBlock):
        self.in_block = block
        self.out_block = InstructionBlock()
        self.out_block.label("_start")
        self.initialise()
        self.prologue()
        for i in range(len(self.in_block.instructions)):
            self.generate_instruction(i)
        self.epilogue()
        self.finalise()
        self.show(self.out_block)

    def initialise(self):
        self.data = DataBlock("data")
        self.data_var = Var("_data", "u32")
        self.data_var.live_range = (0, len(self.in_block.instructions))
        data_register = self.get_register(self.data_var)
        self.sp_var = Var("_sp", "u32")
        sp_register = self.get_register(self.sp_var)
        sp_register.name = "sp"
        self.emit(Instruction("la", [data_register], ["_constants"], "load address of constants"))
        self.emit(Instruction("la", [sp_register], ["_stack_top"], "load address of stack top"))

    def prologue(self):
        self.out_block.label("_prologue")
        self.i_prologue = len(self.out_block.instructions)
        self.emit(Instruction("addi", [self.sp_var.register], [self.sp_var.register, "-slots"], "allocate stack"))
    
    def epilogue(self):
        self.out_block.label("_epilogue")
        self.i_epilogue = len(self.out_block.instructions)
        self.emit(Instruction("addi", [self.sp_var.register], [self.sp_var.register, "slots"], "deallocate stack"))
    
    def generate_instruction(self, i_instruction):
        self.i_instruction = i_instruction
        self.instruction = self.in_block.instructions[i_instruction]
        self.instruction.comment = self.instruction.show()
        self.handle_constants()
        self.handle_registers()
        self.handle_memory()
        self.emit(self.instruction)

    def finalise(self):
        prologue_instr = self.out_block.instructions[self.i_prologue]
        prologue_instr.sources[1] = -len(self.register_manager.spill_slots)*4

    def show(self, block: InstructionBlock):
        log("----------------------------------------------")
        log("riscv32 assembly:\n")
        log(self.output(block))

    def output(self, block: InstructionBlock) -> str:
        index_to_label = { i : label for label, i in block.labels.items() }
        out = """    .section .text        # Code section
    .globl _start         # Entry point must be global
    .type _start, @function\n\n"""
        for i, instr in enumerate(block.instructions):
            if i in index_to_label:
                out += f"{index_to_label[i]}:\n"
            line = f"    {self.show_instruction(instr)}"
            if instr.comment != "": line += (" "*(25-len(line))) + f" # {instr.comment}"
            out += line + "\n"
        out += """
    .section .data        # data section
    .align 4              # align to 4-byte boundary\n
"""
        out += "_constants:\n"
        for data in self.data.data:
            line = f"    .word 0x{data.value.hex()}"
            line += (" " * (25-len(line))) + f" # {data.name}"
            out += line + "\n"
        out += """
    .section .bss         # uninitialized data section
    .align 4              # align to 16-byte boundary\n
_stack_bottom:            # label for bottom of stack
    .zero 4096            # allocate 4KB for stack
_stack_top:               # label for top of stack
"""
        return out

    def show_instruction(self, instr):
        if instr.opcode[0] in "sl" and len(instr.sources) == 2 and isinstance(instr.sources[1], int):
            return f"{instr.opcode} {instr.dests[0]}, {instr.sources[1]}({instr.sources[0]})"
        else:
            return str(instr)
    #----------------------------------------------------------------------------------------------

    def handle_constants(self):
        instr = self.instruction
        if instr.opcode == "const":
            const_address = self.allocate_constant(instr.sources[0])
            instr.sources.append(const_address)
            instr.sources[0] = self.data_var
            instr.opcode = self.load_opcodes[instr.dests[0].type]
        else:
            instr.opcode = self.map_opcode(instr.opcode, instr.dests[0].type)

    def handle_registers(self):
        instr = self.instruction
        source_registers = [self.get_register(source) for source in instr.sources]
        self.free_eol_vars(instr)
        dest_registers = [self.get_register(dest) for dest in instr.dests]
        for i, dest in enumerate(instr.dests):
            instr.dests[i] = dest_registers[i]
        for i, source in enumerate(instr.sources):
            if isinstance(source, Var): instr.sources[i] = source_registers[i]

    def handle_memory(self):
        for i, source in enumerate(self.instruction.sources):
            if isinstance(source, Data):
                self.instruction.sources[i] = source.offset_bytes

    def emit(self, instr):
        self.out_block.instructions.append(instr)

    #----------------------------------------------------------------------------------------------
    # these vary between different CPU types

    def map_opcode(self, opcode: str, type: str) -> str:
        if type[0] in "iu":
            if opcode == "sqrt": raise Exception(f"sqrt not supported for integer type: {type}")
            if opcode == "div" and type[0] == "u": return "divu"
            return opcode
        else: # "f"
            return f"f{opcode}.s"
        
    def emit_spill(self, var: Var, slot: int):
        self.emit(Instruction("sw", [var.register], [self.sp_var.register, slot*4], f"spill {var} to slot {slot}"))

    def emit_unspill(self, var: Var, slot: int):
        self.emit(Instruction("lw", [var.register], [self.sp_var.register, slot*4], f"unspill {var} from slot {slot}"))
        
    #----------------------------------------------------------------------------------------------
    # these are the same across different CPU types

    def free_eol_vars(self, instr):
        for src in instr.sources:
            if isinstance(src, Var):
                if src.register is not None and src.live_range[1] <= self.i_instruction:
                    self.register_manager.free_register(src.register)

    def get_register(self, var) -> Register:
        if not isinstance(var, Var): return None
        if var.register is not None: return var.register
        return self.register_manager.allocate_register(var)
    
    def allocate_constant(self, c: Const) -> Register:
        name = f"const_{c.type}_{c.value}"  
        data = self.data.allocate(name, c.type, self.size_of_type(c.type), self.pack_constant(c))
        return data
    
    def pack_constant(self, c: Const) -> bytes:
        if c.type == "u64": return struct.pack(">Q", int(c.value))
        if c.type == "u32": return struct.pack("<I", int(c.value))
        if c.type == "i64": return struct.pack("<q", int(c.value))
        if c.type == "i32": return struct.pack("<i", int(c.value))
        if c.type == "f64": return struct.pack("<d", float(c.value))
        if c.type == "f32": return struct.pack("<f", float(c.value))
        raise Exception(f"unsupported constant type: {c.type}")
    
    def size_of_type(self, type: str) -> int:
        return int(type[1:])//8

#----------------------------------------------------------------------------------------------
# register management

class RegisterManager:
    def __init__(self, register_sets: Dict[str, RegisterSet], backend: Backend):
        self.register_sets = register_sets
        self.backend :Backend = backend
        self.spill_slots : List[Var] = []

    def allocate_register(self, var: Var) -> Register:
        register_set = self.register_sets[var.type[0]]
        register = next((r for r in register_set.registers if r.contents is None), None)
        if register is None: register = self.spill(register_set,var.type)
        var.register = register
        register.contents = var
        if var.spill_slot is not None:
            self.unspill(var)
        return register
    
    def free_register(self, register: Register):
        var = register.contents
        if isinstance(var, Var): var.register = None
        register.contents = None

    def spill(self, register_set: RegisterSet, type: str) -> Register:
        this_instruction = self.backend.in_block.instructions[self.backend.i_instruction]
        last_i_read = -1
        victim_var = None
        for register in register_set.registers:
            potential_var = register.contents
            if potential_var is None: continue
            if potential_var.register != register: continue
            if not isinstance(potential_var, Var): continue
            if potential_var in this_instruction.sources or potential_var in this_instruction.dests: continue
            i_next_read = self.backend.in_block.find_next_read(self.backend.i_instruction, potential_var)
            if i_next_read > last_i_read:
                last_i_read = i_next_read
                victim_var = potential_var
        if victim_var is None: raise Exception(f"no spill slot available for {type} in {register_set}")
        slot = next((i for i, s in enumerate(self.spill_slots) if s is None), None)
        if slot is None:
            self.spill_slots.append(victim_var)
            slot = len(self.spill_slots) - 1
        self.spill_slots[slot] = victim_var
        log(f"spilling {victim_var} to slot {slot}")
        self.backend.emit_spill(victim_var, slot)
        victim_var.spill_slot = slot
        victim_register = victim_var.register
        victim_register.contents = None
        victim_var.register = None
        return victim_register
        

        
    
    def unspill(self, var: Var):
        log(f"unspilling {var} from slot {var.spill_slot}")
        self.backend.emit_unspill(var, var.spill_slot)
        self.spill_slots[var.spill_slot] = None
        var.spill_slot = None



    
