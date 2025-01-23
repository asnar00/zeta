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
# configuration options for code production

class CodegenConfig:
    def __init__(self):
        self.type_substitution = {}

    def concrete_types(self, type_map: Dict[str, str]):
        self.type_substitution.update(type_map)
        log(f"concrete_types: {self.type_substitution}")

    def get_concrete_type(self, abstract_type_name: str) -> str:
        return self.type_substitution.get(abstract_type_name, None)
    

class Var:
    def __init__(self, name: str, type_name: str):
        self.name = name
        self.type_name = type_name          # concrete type name
        self.live_range = (-1, -1)          # instruction index of first write and last read
        self.register = None                # physical register name (dynamic)
        self.spill_index = None             # index in spill-memory (dynamic)
    def __str__(self): return self.name
    def __repr__(self): return self.name

class Instruction:
    def __init__(self, opcode: str, dest_vars: List[Var], src_vars: List[Var]):
        self.opcode = opcode
        self.dest_vars = dest_vars
        self.src_vars = src_vars

class InstructionBlock:
    def __init__(self):
        self.instructions = []
        self.vars = {}
        self.max_live = 0
        
    def __str__(self):
        max_length = max(len(var.name) for var in self.vars.values())
        max_length += max((len(var.type_name) for var in self.vars.values()), default=0)
        max_digits = len(str(len(self.instructions)))
        max_length += max_digits + 26 # log_grey adds some extra chars

        out = ""
        for i, instr in enumerate(self.instructions):
            ii = log_grey(f"{i:{max_digits}}: ")
            type = log_grey(f"{instr.dest_vars[0].type_name}")
            lhs = f"{ii}{type} {instr.dest_vars[0].name}"
            lhs += " " * (max_length - len(lhs))
            lhs += f"{log_grey("<=")} {instr.opcode} {', '.join([str(var) for var in instr.src_vars])}\n"
            out += lhs
        out += f"max_live: {self.max_live}\n"
        return out
    
    def __repr__(self):
        return str(self)
    
    def find_last_write(self, i_instruction, var: Var):
        i = i_instruction -1
        while i >= 0 and var not in self.instructions[i].dest_vars:
            i -= 1
        return i
    def find_next_read(self, i_instruction: int, var: Var):
        i = i_instruction + 1
        while i < len(self.instructions) and var not in self.instructions[i].src_vars:
            i += 1
        return i
    def find_last_read(self, var: Var):
        for i in range(len(self.instructions) - 1, -1, -1):
            if var in self.instructions[i].src_vars:
                return i
        return -1

#--------------------------------------------------------------------------------------------------

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

    def find_entity(self, key: str, of_type: Any) -> Entity:
        items = self.st.find(key, of_type, None, True)
        return items[0].element if len(items) == 1 else None

    def reset(self):
        self.block = InstructionBlock()
        self.i_var = 0
        self.indent = 0
    
    def alloc_var_index(self) -> int:
        result = self.i_var
        self.i_var += 1
        return result
    
    def output(self, opcode, dest_vars: List[Lex], src_vars: List[Lex]):   
        dest_vars = [str(var) for var in dest_vars]
        src_vars = [str(var) for var in src_vars]
        txt = "    "*self.indent + f"{opcode} {', '.join(dest_vars)}, {', '.join(src_vars)}\n"
        log(log_green(txt))
        dest_vars = [self.block.vars[var_str] for var_str in dest_vars]
        src_vars = [(self.block.vars[var_str] if "_" in var_str else var_str) for var_str in src_vars ]
        self.block.instructions.append(Instruction(opcode, dest_vars, src_vars))

    def add_var(self, var_name, type_name):
        var_name = str(var_name)
        type_name = str(type_name)
        concrete_type_name = self.config.get_concrete_type(str(type_name))
        log(f"assert_type {var_name} {type_name} => {concrete_type_name}")
        if var_name in self.block.vars: raise Exception(f"variable {var_name} already exists")
        self.block.vars[var_name] = Var(var_name, concrete_type_name)


    def show(self, e):
        return print_code_formatted(e, self.grammar).replace("\n", "↩︎").replace("    ", "")
    
#--------------------------------------------------------------------------------------------------
# super below the line (this should really go in zero.py!)

def try_replace(var, replace):
    if "." in var:
        vars = var.split(".")
        if vars[0] in replace:
            return replace[vars[0]] + "." + ".".join(vars[1:])
    else:
        if var in replace: return replace[var]
    return var

#--------------------------------------------------------------------------------------------------
# optimiser - works at VM level before backend does its thing

class Optimiser:
    def __init__(self, block: InstructionBlock):
        self.block = block
    
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
                i_source = self.block.find_last_write(i, instruction.src_vars[0])
                self.block.instructions[i_source].dest_vars[0] = instruction.dest_vars[0]
                vars_to_replace[instruction.src_vars[0]] = instruction.dest_vars[0]
                to_remove.append(i)
        for i in sorted(to_remove, reverse=True):
            del self.block.instructions[i]
        for i, instruction in enumerate(self.block.instructions):
            for j, var in enumerate(instruction.src_vars):
                if var in vars_to_replace:
                    instruction.src_vars[j] = vars_to_replace[var]
        return self
    
    # move 'ld' instructions forward to be as close as possible to where their dest_vars are read
    def optimise_lds(self):
        for i in range(len(self.block.instructions) - 1, -1, -1):
            instruction = self.block.instructions[i]
            if instruction.opcode != "imm": continue
            i_read = self.block.find_next_read(i, instruction.dest_vars[0])
            if i_read < len(self.block.instructions):
                i_dest = i_read - 1
                if i_dest > i:
                    self.block.instructions = self.block.instructions[:i] + self.block.instructions[i+1:i_dest] + [instruction] + self.block.instructions[i_dest:]
        return self
    
    # compute live ranges for all variables
    def compute_live_ranges(self):
        for i in range(len(self.block.instructions)):
            instruction = self.block.instructions[i]
            for var in instruction.dest_vars:
                i_last_read = self.block.find_last_read(var)
                var.live_range = (i, i_last_read)
    
    #----------------------------------------------------------------------------------------------
    # below the line
    
    
    def try_move(self, i_instruction) -> int:  # find earliest index we can move to, or -1 if not possible
        this_instruction = self.block.instructions[i_instruction]
        i_instructions = [self.block.find_last_write(i_instruction, var) for var in this_instruction.src_vars]
        i_can_move = max(i_instructions)
        if i_can_move >= 0 and (i_can_move+1) < i_instruction:
            #log(f"moving {i_instruction}:{this_instruction} to {i_can_move+1}")
            i_move_to = i_can_move + 1
            tmp = self.block.instructions[:i_instruction] + self.block.instructions[i_instruction+1:]
            self.block.instructions = tmp[:i_move_to] + [this_instruction] + tmp[i_move_to:]
            return i_move_to
        return -1
    def try_moves(self) -> int: # return number of moves made
        n_moves = 0
        for i in range(0, len(self.block.instructions)):
            i_moved_to = self.try_move(i)
            if i_moved_to >= 0: n_moves += 1
        return n_moves
    
    def measure_pressure(self): # for each instruction, find the number of live vars
        max_pressure = 0
        live_vars = set()
        for i in range(len(self.block.instructions)):
            instruction = self.block.instructions[i]
            live_vars.update(instruction.dest_vars)
            for src_var in instruction.src_vars:
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
    def generate(self, block: InstructionBlock) -> str: pass
    def run(self) -> str: return ""

#--------------------------------------------------------------------------------------------------
# Python backend runs numpy

class PythonBackend(Backend):
    def generate(self, block: InstructionBlock):
        out = self.header()

        op_map = { "add" : "+", "sub" : "-", "mul" : "*", "div" : "/", "sqrt" : "np.sqrt" }
        type_map = { "f32" : "np.float32", "i32" : "np.int32" }

        for instruction in block.instructions:
            dest_var = instruction.dest_vars[0].name.replace(".", "_")
            if instruction.opcode == "imm":
                type = type_map[instruction.dest_vars[0].type_name]
                out += f"    {dest_var} = {type}({instruction.src_vars[0]})\n"
            else:
                opcode = op_map[instruction.opcode]
                operands = [var.name.replace(".", "_") for var in instruction.src_vars]
                if opcode in "+-*/":
                    out += f"    {dest_var} = {operands[0]} {opcode} {operands[1]}\n"
                elif len(operands) == 1:
                    out += f"    {dest_var} = {opcode}({operands[0]})\n"

        for instruction in block.instructions:
            dest_var = instruction.dest_vars[0].name.replace(".", "_")
            out += f"    print(f\"{instruction.dest_vars[0]} = {{{dest_var}}}\")\n"
        out += self.footer()
        out = out.strip()
        log_clear()
        log("----------------------------------------------")
        log(out)
        write_file(self.path, out)
    
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
# ARM backend runs on the M1

# single register set of a single type (float or int)
class RegisterSet:
    def __init__(self, n_registers: int, name_map: Dict[str, str]):
        self.n_registers = n_registers
        self.name_map = name_map
        self.reset()

    def reset(self):
        self.assigned_vars = [None for i in range(0, self.n_registers)]
        # artificially occupy some registers
        dummy_var = Var("dummy", "f32")
        for i in range(2, self.n_registers): self.assigned_vars[i] = dummy_var

    def allocate(self, var: Var) -> str:
        for i in range(0, self.n_registers):
            if self.assigned_vars[i] == None:
                self.assigned_vars[i] = var
                reg = self.name_map[var.type_name] + str(i)
                var.register = reg
                return reg
        return None
    
    def free(self, var: Var):
        if var.register == None: return
        self.assigned_vars[int(var.register[1:])] = None
        var.register = None

    def transfer(self, to_var: Var, from_var: Var):
        to_var.register = from_var.register
        from_var.register = None
        i_register = int(from_var.register[1:])
        self.assigned_vars[i_register] = to_var

# manages multiple register sets
class RegisterAllocator:
    def __init__(self, register_sets: Dict[str, RegisterSet]):
        self.register_sets = register_sets
        self.reset()

    def reset(self):
        for register_set in self.register_sets.values(): register_set.reset()
        
    def allocate(self, var: Var) -> str:
        register_set = self.register_sets[var.type_name[0]]
        return register_set.allocate(var)
    
    def free(self, var: Var):
        self.register_sets[var.type_name[0]].free(var)
        var.register = None

# manages spill slots for a single size
class SpillManager:
    def __init__(self, n_bytes: int):
        self.n_bytes = n_bytes
        self.reset()

    def reset(self):
        self.spilled_vars = []
        self.max_spills = 0

    def assign_spill_index(self, var: Var) -> int:      # return byte offset to store to
        index = next((i for i, var in enumerate(self.spilled_vars) if var is None), None)
        if index is None:
            index = len(self.spilled_vars)
            self.spilled_vars.append(None)
            self.max_spills = max(self.max_spills, len(self.spilled_vars))
        self.spilled_vars[index] = var
        var.spill_index = index
        return index * self.n_bytes

    def unspill_var(self, var: Var) -> int:     # return byte offset to load from
        index = var.spill_index
        self.spilled_vars[index] = None
        var.spill_index = None
        return index * self.n_bytes

# holds a register, two offsets (store, and load)
class RegisterResult:
    def __init__(self, register: str, store_offset: int, spill_var: Var, load_offset: int):
        self.register = register
        self.store_offset = store_offset
        self.spill_var = spill_var
        self.load_offset = load_offset

# manages register allocation and splitting automatically
class RegisterManager:
    def __init__(self, register_sets: Dict[str, RegisterSet]):
        self.register_allocator = RegisterAllocator(register_sets)
        self.spill_manager_32 = SpillManager(4)
        self.spill_manager_64 = SpillManager(8)
        self.spill_managers = { "i32" : self.spill_manager_32, "i64" : self.spill_manager_64, "f32" : self.spill_manager_32, "f64" : self.spill_manager_64 }
    
    def reset(self, block: InstructionBlock):
        self.block : InstructionBlock = block
        self.register_allocator.reset()
        for spill_manager in self.spill_managers.values(): spill_manager.reset()
    
    def find_register(self, var: Var, for_write: bool, i_instruction: int) -> RegisterResult:        # spills / unspills as necessary
        if var.register: return RegisterResult(var.register)    # we already have a register, nothing to do
        # some spillage
        spill_manager = self.spill_managers[var.type_name]
        if for_write: # we have to spill something
            reg = self.register_allocator.allocate(var)
            if reg: return RegisterResult(reg, None, None, None)
            store_offset, victim_var = self.spill_something(var, spill_manager, i_instruction)
            reg = self.register_allocator.allocate(var)
            return RegisterResult(reg, store_offset, victim_var, None)
        else: # we're unspilling this var
            load_offset = spill_manager.unspill_var(var)
            reg = self.register_allocator.allocate(var) # if this works we're fine
            if reg: return RegisterResult(reg, None, None, load_offset)
            store_offset, victim_var = self.spill_something(var, spill_manager, i_instruction)
            reg = self.register_allocator.allocate(var)
            return RegisterResult(reg, store_offset, victim_var, load_offset)
        
    def free(self, var: Var):
        self.register_allocator.free(var)
        
    def spill_something(self, var: Var, spill_manager: SpillManager, i_instruction: int) -> Tuple[int, Var]:
        victim_var = self.select_victim(var.type_name, i_instruction)
        store_index = spill_manager.assign_spill_index(victim_var)
        self.register_allocator.free(victim_var)
        return store_index, victim_var

    def select_victim(self, type_name: str, i_instruction: int) -> Var:
        potentials = [var for var in self.block.vars.values() if var.register and var.type_name == type_name]
        this_instruction = self.block.instructions[i_instruction]
        potentials = [var for var in potentials if var not in this_instruction.dest_vars and var not in this_instruction.src_vars]
        if len(potentials) == 0: log_exit("no victims found")
        if len(potentials) == 1: return potentials[0]
        # find the one with the latest next-read instruction
        i_best_read = -1
        best_victim = None
        for var in potentials:
            i_next_read = self.block.find_next_read(i_instruction, var)
            if i_next_read > i_best_read:
                i_best_read = i_next_read
                best_victim = var
        return best_victim
    
    def total_spill_bytes(self) -> int:
        return self.spill_manager_32.max_spills * 4 + self.spill_manager_64.max_spills * 8
    
#--------------------------------------------------------------------------------------------------
# ARM backend outputs ARM assembly/binary

# ARMCode holds both text assembly and binary executable code
class ARMCode:
    def __init__(self):
        self.text = ""
        self.data = array.array('I')
    def write_text(self, line, comment):
        spaces = " "*(32-len(line))
        self.text += line + spaces + f"@ {comment}\n"
    def write_data(self, instruction):
        self.data.append(instruction)
    def __str__(self):
        return self.text
    def __repr__(self):
        return str(self)
    def join(self, other_arm_block):
        self.text += other_arm_block.text
        self.data.extend(other_arm_block.data)
    def emit_float_op(self, opcode, dest_reg, src_regs, comment):
        opcode_map = { "fadd": 0b00011110001, "fsub": 0b00011110011, "fmul": 0b00011110101, "fsqrt": 0b00011110000 }
        if not opcode in opcode_map: log_exit(f"unknown opcode {opcode}")
        self.write_text(f"    {opcode} {dest_reg}, {', '.join(src_regs)}", comment)
        opcode_bits = opcode_map[opcode]
        if dest_reg[0] == "d": opcode_bits |= (1 << 4)
        rd = int(dest_reg[1:])
        rn = int(src_regs[0][1:])
        rm = int(src_regs[1][1:]) if len(src_regs) > 1 else 0
        instruction = ((opcode_bits << 21) | (rm << 16) | (rn << 5) | rd)
        self.write_data(instruction)
    def emit_int_op(self, opcode, dest_reg, src_regs, comment):
        self.write_text(f"    {opcode} {dest_reg}, {', '.join(src_regs)}", comment)
    def emit_ldr(self, dest_reg, mem_reg, offset, comment):
        self.write_text(f"    ldr {dest_reg}, [{mem_reg}, #{offset}]", comment)
    def emit_str(self, dest_reg, mem_reg, offset, comment):
        self.write_text(f"    str {dest_reg}, [{mem_reg}, #{offset}]", comment)
    def emit_stp(self, dest_reg, mem_reg, offset, comment):
        self.write_text(f"    stp {dest_reg}, {mem_reg}, [sp, #{offset}]!", comment)
    def emit_adr(self, dest_reg, mem_reg, comment):
        self.write_text(f"    adr {dest_reg}, {mem_reg}", comment)
    def emit_mov(self, dest_reg, src_reg, comment):
        self.write_text(f"    mov {dest_reg}, {src_reg}", comment)
    def emit_ret(self):
        self.write_text("    ret", "return")
    def emit_section(self, section_name: str, comment: str):
        self.write_text(f".section {section_name}", comment)
    def emit_label(self, label: str, comment: str):
        self.write_text(f"{label}:", comment)
    def emit_word(self, word: str, comment: str):
        self.write_text(f"    .word {word}", comment)
    def align(self, n_bytes: int):
        self.write_text(f".align {n_bytes}", "align to {n_bytes} bytes")
        n_current = len(self.data)
        n_padding = (n_bytes - n_current) % n_bytes
        self.data.extend([0] * n_padding)

class ARMBackend(Backend):
    def __init__(self, path: str):
        super().__init__(path)
        self.int_registers = RegisterSet(32, { "i32" : "w", "i64" : "x" })
        self.fp_registers = RegisterSet(32, { "f32" : "s", "f64" : "d", "f32x4" : "v" })
        self.register_sets = { "i" : self.int_registers, "f" : self.fp_registers }
        self.register_manager = RegisterManager(self.register_sets)
        self.arithmetic_ops = ["add", "sub", "mul", "div", "sqrt"]
        self.opcode_map_float = { "add" : "fadd", "sub" : "fsub", "mul" : "fmul", "div" : "fdiv", "sqrt" : "fsqrt" }
        self.opcode_map_int = { "add" : "add", "sub" : "sub", "mul" : "mul", "div" : "div", "sqrt" : "sqrt" }
        self.constants = {}
        self.constant_memory_size = 0
        self.constant_memory = Var("constant_memory_adr", "i64")
        self.i_instruction = None
        self.out = ARMCode()

    def generate(self, block: InstructionBlock):
        self.block = block
        #log_clear()
        self.reset()
        self.find_register(self.constant_memory, for_write=True)
        
        for i, instruction in enumerate(block.instructions):
            self.i_instruction = i
            self.emit_instruction(instruction)
           
        self.emit_dealloc_spill(self.out)
        output_block = self.emit_prelude()
        output_block.join(self.out)
        output_block.emit_ret()
        log(output_block)
        write_file(self.path, output_block)

    def emit_instruction(self, instruction):
        if instruction.opcode == "imm": return self.emit_immediate(instruction)
        elif instruction.opcode in self.arithmetic_ops: return self.emit_arithmetic_op(instruction)
        else: log_exit(f"unknown opcode {instruction.opcode}")

    def emit_arithmetic_op(self, instruction):
        src_regs = []
        src_types = []
        src_regs = [self.find_register(var, for_write=False) for var in instruction.src_vars]
        src_types = [var.type_name for var in instruction.src_vars]
        self.free_unused_registers(instruction.src_vars)
        dest_reg = self.find_register(instruction.dest_vars[0], for_write=True)
        dest_type = instruction.dest_vars[0].type_name
        out_opcode = self.find_opcode(instruction.opcode, dest_type, src_types)
        comment = f"{instruction.dest_vars[0]} <= {instruction.opcode}{str(instruction.src_vars).replace("[", " ").replace("]", "")}"
        self.out.emit_float_op(out_opcode, dest_reg, src_regs, comment)

    def emit_immediate(self, instruction):
        immediate = instruction.src_vars[0]
        dest_reg = self.find_register(instruction.dest_vars[0], for_write=True)
        dest_type = instruction.dest_vars[0].type_name
        offset = self.constants.get(immediate, None)
        if offset is None:
            n_bytes = int(dest_type[1:])/8
            offset = int(self.constant_memory_size)  # todo: align if necessary
            self.constant_memory_size += n_bytes
            self.constants[immediate] = offset
        self.out.emit_ldr(dest_reg, self.constant_memory.register, offset, f"{instruction.dest_vars[0]} <= f32({immediate})")

    def emit_prelude(self) -> ARMCode:
        prelude = ARMCode()
        self.output_memory_section(prelude)
        prelude.write_text(".global run", "make 'run' callable from outside")
        prelude.emit_label("run", "entry point")
        prelude.emit_stp("x29", "x30", "-16", "save frame pointer and return address")
        prelude.emit_mov("x29", "sp", "set up frame pointer")
        self.emit_alloc_spill(prelude)
        prelude.emit_adr(self.constant_memory.register, "f32_constants", "load constant memory address")
        return prelude

    #-------------------------------------------------------------------------


    def output_memory_section(self, out_block: ARMCode):
        def float_to_hex(f: float) -> str:
            # Pack the float into 4 bytes (single-precision)
            packed = struct.pack('>f', f)  # '<f' is little-endian single-precision
            # Convert the packed bytes to a hexadecimal string
            return packed.hex()
        out_block.emit_section(".rodata", "constant data section")
        out_block.align(4)
        out_block.emit_label("f32_constants", "label for constant memory")
        for i, (const, offset) in enumerate(self.constants.items()):
            out_block.emit_word(f"0x{float_to_hex(float(const))}", f"f32({const})")
    
    def emit_alloc_spill(self, output_block: ARMCode) -> str:
        n_spill_bytes = self.register_manager.total_spill_bytes()
        if n_spill_bytes > 0:
            output_block.emit_int_op("sub", "sp", ["sp", f"#{n_spill_bytes}"], "allocate spill space")
    
    def emit_dealloc_spill(self, output_block: ARMCode) -> str:
        n_spill_bytes = self.register_manager.total_spill_bytes()
        if n_spill_bytes > 0:
            output_block.emit_int_op("add", "sp", ["sp", f"#{n_spill_bytes}"], "deallocate spill space")
            

    #-------------------------------------------------------------------------
    # opcode selection based on type

    def find_opcode(self, opcode, dest_type, src_types):
        if dest_type[0] == "f" and all(src_type[0] == "f" for src_type in src_types):
            return self.opcode_map_float[opcode]
        elif dest_type[0] == "i" and all(src_type[0] == "i" for src_type in src_types):
            return self.opcode_map_int[opcode]
        else:
            raise Exception(f"no opcode found for {opcode} {dest_type} {src_types}")
    
    #-------------------------------------------------------------------------
    # register allocation stuff

    def reset(self):
        self.register_manager.reset(self.block)
    
    # find a register for a variable; if there isn't one, generate load instruction
    @log_suppress
    def find_register(self, var: Var, for_write: bool) -> str:
        log(f"i{self.i_instruction}: find_register {var.name} {"for write" if for_write else "for read"}")
        if var.register:
            return var.register
        reg_result = self.register_manager.find_register(var, for_write, self.i_instruction)
        if reg_result.store_offset != None:
            self.out.emit_str(reg_result.register, "sp", reg_result.store_offset, f"spill {reg_result.spill_var.name} to make room for {var.name}")
        if reg_result.load_offset != None:
            self.out.emit_ldr(reg_result.register, "sp", reg_result.load_offset, f"unspill {var.name}")
        return reg_result.register
    
    # if any variables are at the end of their live range, free their registers
    def free_unused_registers(self, vars: List[Var]):
        for var in vars:
            if var.live_range[1] <= self.i_instruction:
                self.register_manager.free(var)

    


    
    
        



        
        
        


