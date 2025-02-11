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
    def __str__(self): return f"#{self.value}"
    def __repr__(self): return str(self)

# instruction: can be either VM or processor specific
class Instruction:
    def __init__(self, opcode: str, dests: List[Var], sources: List[Var|Const], comment: str = ""):
        self.opcode : str = opcode                      # opcode
        self.dests : List[Var] = dests                  # destination variables
        self.sources : List[Var|Const] = sources        # source variables or constants
        self.comment : str = comment                    # comment
    def __str__(self): 
        comment = log_grey(f"// {self.comment}" if self.comment else "")
        line = f"{self.opcode} {', '.join([str(var) for var in self.dests])}, {', '.join([str(var) for var in self.sources])}"
        if comment != "": line += " "*(25-len(line)) + comment
        return line
    def __repr__(self): return str(self)
    def show(self) -> str:
        return f"{self.dests[0].type} {self.dests[0].name} <= {self.opcode} {', '.join([str(var) for var in self.sources])}"
       
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
    def __init__(self, type: str, n_registers: int, prefix: str):
        self.type = type
        self.n_registers = n_registers
        self.registers = [Register(f"{prefix}{i}", type, i) for i in range(n_registers)]
    def __str__(self): return f"{self.type}"
    def __repr__(self): return str(self)
    
class Data:
    def __init__(self, name: str, type: str, size_bytes: int, value: bytes):
        self.name = name
        self.type = type
        self.size_bytes = size_bytes
        self.value = value
        self.offset_bytes = None
    def __str__(self): return f"{self.name}"

class DataBlock:
    def __init__(self, name: str):
        self.address = 0
        self.data : List[Data] = []

#--------------------------------------------------------------------------------------------------
# Processor is base class for all architectures

class Processor:
    def __init__(self): pass
    def setup(self, block: InstructionBlock): override_me()
    def prologue(self) -> List[Instruction]: override_me()
    def epilogue(self) -> List[Instruction]: override_me()
    def instruction(self, instruction: Instruction, i_instruction: int) -> List[Instruction]: override_me()

class RegisterProcessor(Processor):
    def __init__(self):
        super().__init__()
        self.max_fp_registers = None
    def init_register_sets(self) -> Dict[str, RegisterSet]: override_me()
    def register_name(self, register_name: str, type: str) -> str: override_me()
    def map_opcode(self, opcode: str, type: str) -> str: override_me()
    def spill(self, register: Register, slot: int, comment: str = ""): override_me()
    def unspill(self, register: Register, slot: int, comment: str = ""): override_me()

    #----------------------------------------------------------------------------------------------
    # setup : call before you start emitting instructions

    def setup(self, block: InstructionBlock):
        self.block = block
        self.register_sets = self.init_register_sets()
        self.spill_slots = []
        if self.max_fp_registers != None:
            self.restrict_free_fp_registers(self.max_fp_registers)

    def restrict_free_fp_registers(self, max_fp_registers: int):
        if max_fp_registers > 0:
            dummy_var = Var("dummy", "f32")
            register_set = self.register_sets["f"]
            for i in range(0, register_set.n_registers):
                if i >= max_fp_registers:
                    register_set.registers[i].contents = dummy_var

    #----------------------------------------------------------------------------------------------
    # emit instruction

    def instruction(self, instruction: Instruction, i_instruction: int) -> List[Instruction]:
        self.out : List[Instruction] = []
        self.i_instruction = i_instruction
        if len(instruction.dests) != 1: raise Exception(f"expected 1 destination, got {len(instruction.dests)}")
        source_registers = [self.assign_register(source) for source in instruction.sources]
        self.free_eol_registers(instruction)
        instruction.comment = instruction.show()
        instruction.opcode = self.map_opcode(instruction.opcode, instruction.dests[0].type)
        for i, dest in enumerate(instruction.dests):
            instruction.dests[i] = self.assign_register(dest)
        for i in range(len(instruction.sources)):
            if source_registers[i] is not None:
                instruction.sources[i] = source_registers[i]
        return self.out + [instruction]
            
    #----------------------------------------------------------------------------------------------
    # register allocation and spill

    def free_eol_registers(self, instruction: Instruction):
        for source in instruction.sources:
            if isinstance(source, Var) and source.register is not None:
                if source.live_range[1] <= self.i_instruction:
                    source.register.contents = None
                    source.register = None

    def assign_register(self, var: Var) -> Register:
        if not isinstance(var, Var): return None
        if var.register is not None: return var.register
        register_set = self.register_sets[var.type[0]]
        register = next((register for register in register_set.registers if register.contents is None), None)
        if register is None: register = self.spill_register(register_set,var.type)
        register.name = self.register_name(register, var.type)
        if isinstance(register.contents, Var): register.contents.register = None
        if var.spill_slot is not None: self.unspill_register(var, register)
        self.set_register(var, register)
        return register
    
    def set_register(self, var: Var, register: Register):
        var.register = register
        register.contents = var
    
    def spill_register(self, register_set: RegisterSet, type: str) -> Register:
        victim_var = self.find_spill_victim(register_set, type)
        slot = self.find_spill_slot(type)   
        victim_var.spill_slot = slot
        self.spill_slots[slot] = victim_var
        self.out += self.spill(victim_var.register, slot, f"spill {victim_var} to slot {slot}")
        return victim_var.register
    
    def unspill_register(self, var: Var, register: Register):
        slot = var.spill_slot
        self.out += self.unspill(register, slot, f"unspill {var} from slot {slot}")
        var.spill_slot = None
        self.spill_slots[slot] = None
    
    def find_spill_victim(self, register_set: RegisterSet, type: str) -> Var:
        this_instruction = self.block.instructions[self.i_instruction]
        i_best_read = self.i_instruction
        best_victim = None
        for register in register_set.registers:
            var = register.contents
            if var is None: return register                 # if register is free, use it
            if not isinstance(var, Var): continue           # if register is not a variable, you can't spill it
            if var.register != register: continue            # if register is not the same as the one we're spilling, you can't spill it
            if var in this_instruction.sources: continue    # if variable is used by this instruction, you can't spill it
            i_last_read = self.block.find_last_read(var)    # next time we need this instruction
            if best_victim is None or i_last_read > i_best_read:
                best_victim = var
                i_best_read = i_last_read
        return best_victim

    def find_spill_slot(self, type: str) -> int:
        i_slot = next((i for i, slot in enumerate(self.spill_slots) if slot is None), None)
        if i_slot is not None: return i_slot
        self.spill_slots.append(None)
        return len(self.spill_slots) - 1

#--------------------------------------------------------------------------------------------------
# just a couple of register-set-based processors, nothing special

class RISCV(RegisterProcessor):
    def init_register_sets(self): return { "i" : RegisterSet("i", 32, "x"), "f" : RegisterSet("f", 32, "f") }
    def prologue(self) -> List[Instruction]: return []
    def epilogue(self) -> List[Instruction]: return []
    def instruction(self, instruction: Instruction): return []
    def spill(self, register: Register, slot: int): return []
    def unspill(self, register: Register, slot: int): return []

class ARM(RegisterProcessor):
    def __init__(self):
        super().__init__()
        self.register_prefixes = { "i32" : "w", "i64" : "x", "f32" : "s", "f64" : "d" }
        self.arithmetic_ops = [ "add", "sub", "mul", "div", "sqrt"]
        self.data = Var("data", "i64")
        self.stack = Var("stack", "i64")
    def init_register_sets(self): 
        return { "i" : RegisterSet("i", 32, "x"), "f" : RegisterSet("f", 32, "d") }
    def register_name(self, register: Register, type: str) -> str:
        return f"{self.register_prefixes[type]}{register.index}"
    def map_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_ops: return opcode if type.startswith("i") else f"f{opcode}"
        elif opcode == "const": return "ldr"
        raise Exception(f"unknown opcode: {opcode}")
    def spill(self, register: Register, slot: int, comment: str = ""): 
        return [Instruction("str", [register], [self.stack_register, f"{slot*4}"], comment=comment)]
    def unspill(self, register: Register, slot: int, comment: str = ""): 
        return [Instruction("ldr", [register], [self.stack_register, f"{slot*4}"], comment=comment)]
    def prologue(self) -> List[Instruction]:
        self.stack_register = self.register_sets["i"].registers[31]
        self.stack_register.name = "sp"
        self.set_register(self.stack, self.stack_register)
        self.data_register = self.register_sets["i"].registers[0]
        self.set_register(self.data, self.data_register)
        return [ Instruction("adr", [self.data_register], ["data"], "load data start address"),
            Instruction("sub", [self.stack_register], [self.stack_register, "spill_size"], "allocate stack space")]
    def epilogue(self) -> List[Instruction]:
        return [Instruction("add", [self.stack_register], [self.stack_register, "spill_size"], "deallocate stack space")]

#--------------------------------------------------------------------------------------------------
# stubs for now: but you see how this can work

class StackProcessor(Processor):
    pass

class WebAsm(StackProcessor):
    pass

#--------------------------------------------------------------------------------------------------

class CPUBackend(Backend):
    def __init__(self, path: str, processor: Processor, dbg: bool):
        self.path = path
        self.processor = processor
        self.dbg = dbg

    # output assembly code for the given VM code
    def generate(self, block: InstructionBlock):
        self.reset(block)
        self.out.instructions += self.processor.prologue()
        for i, instruction in enumerate(block.instructions):
            self.out.instructions += self.processor.instruction(instruction, i)
        self.out.instructions += self.processor.epilogue()
        self.show()
        self.resolve_addresses()
        self.output_assembly()
        self.assemble()
        self.run()

    #----------------------------------------------------------------------------------------------
    
    def reset(self, block: InstructionBlock):
        self.processor.setup(block)
        self.out = InstructionBlock()
        self.data = DataBlock("data")

    def resolve_addresses(self):
        pass

    def output_assembly(self):
        pass

    def assemble(self):
        pass

    def run(self):
        pass

    def show(self):
        for instruction in self.out.instructions:
            log(f"{instruction}")
        



