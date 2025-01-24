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
        self.dbg = False            # set to True to generate debug code
        
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
        n_bytes = self.spill_manager_32.max_spills * 4 + self.spill_manager_64.max_spills * 8
        return (n_bytes + 15) // 16 * 16
    
#--------------------------------------------------------------------------------------------------
# ARM backend outputs ARM assembly/binary

# ARMCode holds both text assembly and binary executable code
class ARMCode:
    def __init__(self):
        self.text = ""
        self.lines = []
        self.data = array.array('I')
        self.descriptors = []                   # array of arrays of bit-indices
        self.labels = {}                        # name => offset from start in bytes
        self.sections = {}                      # name => offset from start in bytes
        self.globals = {}                       # name => offset from start in bytes
        self.pending_relative_addresses = {}    # name => offset from start in bytes

    def write_instruction(self, text, comment, instruction, descriptor):
        spaces = " "*(32-len(text))
        line = text + spaces + f"// {comment}"
        self.text += line + "\n"
        self.lines.append(line)
        self.data.append(instruction)
        self.descriptors.append(descriptor)

    def __str__(self):
        return self.text
    def __repr__(self):
        return str(self)
    
    def join(self, other_arm_block):
        self.text = self.text + other_arm_block.text
        self.lines.extend(other_arm_block.lines)
        self.data.extend(other_arm_block.data)
        self.descriptors.extend(other_arm_block.descriptors)

    def emit_float_op(self, opcode, dest_reg, src_regs, comment):
        opcode_map = { "fadd": 0b00011110001, "fsub": 0b00011110011, "fmul": 0b00011110101, "fsqrt": 0b00011110000 }
        if not opcode in opcode_map: log_exit(f"unknown opcode {opcode}")
        opcode_bits = opcode_map[opcode]
        if dest_reg[0] == "d": opcode_bits |= (1 << 4)
        rd = int(dest_reg[1:])
        rn = int(src_regs[0][1:])
        rm = int(src_regs[1][1:]) if len(src_regs) > 1 else 0
        instruction = ((opcode_bits << 21) | (rm << 16) | (rn << 5) | rd)
        self.write_instruction(f"    {opcode} {dest_reg}, {', '.join(src_regs)}", comment, instruction, [21, 16, 5])

    def emit_int_op(self, opcode, dest_reg, src_regs, comment):
        opcode_map = {
            # Basic arithmetic
            "add": 0b10001011,
            "sub": 0b11001011,
            "mul": 0b10011011,
            "sdiv": 0b10011010,
            "udiv": 0b10011010,
            # Bitwise
            "and": 0b10001010,
            "orr": 0b10101010,
            "eor": 0b11001010,
            # Compare
            "cmp": 0b11001011
        }
        if not opcode in opcode_map:
            log_exit(f"unknown opcode {opcode}")
        rd = 31 if dest_reg == "sp" else int(dest_reg[1:])
        rn = 31 if src_regs[0] == "sp" else int(src_regs[0][1:])
        descriptor = []
        if opcode in ["sdiv", "udiv"]:
            rm = int(src_regs[1][1:])
            div_op = 0b001 if opcode == "sdiv" else 0b010
            instruction = (opcode_map[opcode] << 24) | (div_op << 21) | (rm << 16) | (rn << 5) | rd
            descriptor = [24, 21, 16, 5]
        elif opcode == "cmp":
            if src_regs[1].startswith('#'):
                imm = int(src_regs[1][1:])
                instruction = (opcode_map[opcode] << 24) | (0b00 << 22) | (imm << 10) | (rn << 5) | 31  # rd = 31 (discard)
                descriptor = [24, 22, 10, 5]
            else:
                rm = int(src_regs[1][1:])
                instruction = (opcode_map[opcode] << 24) | (rm << 16) | (rn << 5) | 31
                descriptor = [24, 16, 5]
        elif len(src_regs) > 1 and src_regs[1].startswith('#'):
            imm = int(src_regs[1][1:])
            if imm > 0xFFF:
                log_exit(f"immediate {imm} too large")
            instruction = (opcode_map[opcode] << 24) | (0b00 << 22) | (imm << 10) | (rn << 5) | rd
            descriptor = [24, 22, 10, 5]
        else:
            rm = int(src_regs[1][1:])
            instruction = (opcode_map[opcode] << 24) | (rm << 16) | (rn << 5) | rd
            descriptor = [24, 16, 5]
        self.write_instruction(f"    {opcode} {dest_reg}, {', '.join(src_regs)}", comment, instruction, descriptor)

    def emit_ldr(self, dest_reg, mem_reg, offset, comment):
        rd = int(dest_reg[1:])
        rn = 31 if mem_reg == "sp" else int(mem_reg[1:])
        imm12 = offset & 0xFFF
        instruction = 0b10111000101 << 21 | (imm12 << 10) | (rn << 5) | rd
        self.write_instruction(f"    ldr {dest_reg}, [{mem_reg}, #{offset}]", comment, instruction, [21, 10, 5])

    def emit_str(self, dest_reg, mem_reg, offset, comment):
        rd = int(dest_reg[1:])
        rn = 31 if mem_reg == "sp" else int(mem_reg[1:])
        imm12 = offset & 0xFFF
        instruction = 0b10111000100 << 21 | (imm12 << 10) | (rn << 5) | rd
        self.write_instruction(f"    str {dest_reg}, [{mem_reg}, #{offset}]", comment, instruction, [21, 10, 5])

    def emit_stp(self, dest_reg, mem_reg, offset, comment):
        rt1 = int(dest_reg[1:])
        rt2 = int(mem_reg[1:])
        imm7 = (offset // 8) & 0x7F  # Scale by 8 and take 7 bits
        instruction = 0b10100110 << 24 | (imm7 << 15) | (rt2 << 10) | (31 << 5) | rt1  # 31 = sp
        self.write_instruction(f"    stp {dest_reg}, {mem_reg}, [sp, #{offset}]!", comment, instruction, [24, 15, 10, 5])

    def emit_adr(self, dest_reg, mem_address, comment):
        rd = int(dest_reg[1:])
        relative_address = self.find_relative_offset(mem_address)
        if relative_address is None: 
            self.add_pending_relative_address(mem_address, self.current_address())
            relative_address = 0
        immlo = (relative_address & 0b11) << 29
        immhi = ((relative_address >> 2) & 0x7FFFF) << 5
        instruction = 0b00010000 << 24 | immlo | immhi | rd
        self.write_instruction(f"    adr {dest_reg}, {mem_address}", comment, instruction, [29, 24, 5])

    def emit_mov(self, dest_reg, src_reg, comment):
        rd = 31 if dest_reg == "sp" else int(dest_reg[1:])
        rn = 31 if src_reg == "sp" else int(src_reg[1:])
        instruction = 0b10101010000 << 21 | (rn << 5) | rd    # MOV is actually an alias for ORR with zero register
        self.write_instruction(f"    mov {dest_reg}, {src_reg}", comment, instruction, [21, 5])
   
    def emit_ret(self):
        instruction = 0b11010110010111110000001111000000  # Fixed encoding for RET
        self.write_instruction("    ret", "return", instruction, None)

    def emit_section(self, section_name: str, comment: str):
        if section_name in self.sections: log_exit("section {section_name} already exists")
        self.sections[section_name] = self.current_address()
        self.text += f".section {section_name}\n"

    def emit_global(self, name: str):
        self.text += f".global {name}\n"
        self.globals[name] = self.current_address()

    def emit_label(self, label: str, comment: str):
        if label in self.labels: log_exit(f"label {label} already exists")
        self.labels[label] = self.current_address()
        self.text += f"{label}:\n"
        if label in self.pending_relative_addresses:
            #log(f"label '{label}' is pending relative address {self.pending_relative_addresses[label]}")
            offsets = self.pending_relative_addresses[label]
            for offset in offsets:
                self.poke_relative_address(self.current_address(), offset)
            del self.pending_relative_addresses[label]

    def emit_word(self, word: str, comment: str):
        self.write_instruction(f"    .word {word}", comment, int(word, 16), None)

    def emit_align(self, n_bytes: int):
        n_padding = (n_bytes - self.current_address()) % n_bytes
        n_words = int(n_padding / 4)
        for i in range(0, n_words):
            self.write_instruction("    .word 0x00000000", "padding", 0, None)

    def find_relative_offset(self, mem_address: str) -> int:
        if mem_address in self.labels:
            return (self.labels[mem_address] - self.current_address()) * 4 # bytes not instructions
        return None

    def add_pending_relative_address(self, mem_address: str, offset: int):
        if mem_address not in self.pending_relative_addresses:
            self.pending_relative_addresses[mem_address] = [offset]
        else:
            self.pending_relative_addresses[mem_address].append(offset)
        #log(f"added pending relative address '{mem_address}' => {offset}")

    def poke_relative_address(self, dest_address:int, offset:int):
        relative_address = dest_address - offset
        #log(f"dest_address {hex(dest_address)} relative_address {hex(relative_address)} at {hex(offset)}")
        immlo = (relative_address & 0b11) << 29
        immhi = ((relative_address >> 2) & 0x7FFFF) << 5
        i_word = int(offset/4)
        existing = self.data[i_word] # int
        new_instruction = (existing & (0xff00001f)) | immlo | immhi
        self.data[i_word] = new_instruction
    
    def current_address(self) -> int: return len(self.data) * 4

    def show_binary(self) -> str:
        out = ""
        for i in range(0, len(self.data)):
            instr = self.data[i]
            line = self.lines[i]
            line = line.split('//')[0].strip()
            hex_i = f"{i*4:02x}"
            out += f"{hex_i:2}: {line}" + " " * (27-len(line))
            out += f"{self.show_instruction(instr, self.descriptors[i])}\n"
        return out
    
    def show_instruction(self, instr: int, descriptor: List[int]) -> str:
        if descriptor==None: descriptor = []
        descriptor = [32] + descriptor + [0]
        out = ""
        highlight = False
        for i_desc in range(0, len(descriptor)-1):
            i_bit = descriptor[i_desc+1]
            n_bits = descriptor[i_desc] - i_bit
            val = (instr >> i_bit) & ((1 << n_bits) - 1)
            val = bin(val)[2:]  # Convert to binary, remove the '0b' prefix, and add leading zeros up to n_bits
            spaces = n_bits - len(val)
            before = spaces
            after = spaces - before
            val = "0" * before + val + " " * after
            if highlight == False: val = log_grey(val)
            out += val
            highlight = not highlight
        return out

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
        self.scratch = {}
        self.scratch_initials = {}
        self.scratch_comments = {}
        self.scratch_memory_size = 0
        self.scratch_memory_adr = Var("scratch_memory_adr", "i64")
        self.i_instruction = None
        self.out = ARMCode()

    def generate(self, block: InstructionBlock):
        #block.dbg = True        # write temp vars into scratch
        self.block = block
        self.reset()
        self.add_dbg_taps()
        self.find_register(self.scratch_memory_adr, for_write=True)
        for i, instruction in enumerate(block.instructions):
            self.i_instruction = i
            self.emit_instruction(instruction)
        self.emit_dealloc_spill(self.out)
        output_block = self.emit_prelude()
        output_block.join(self.out)
        output_block.emit_ret()
        self.output_memory_section(output_block)
        log("----------------------------------------------")
        log("assembly:")
        log(output_block.text)
        log("----------------------------------------------")
        log("binary:")
        log(output_block.show_binary())
        log("----------------------------------------------")
        write_file(self.path.replace(".*", ".s"), output_block)
        with open(self.path.replace(".*", ".bin"), "wb") as f:
            for i in range(0, len(output_block.data)):
                f.write(struct.pack('<I', output_block.data[i]))

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
        self.emit_dbg_tap(instruction.dest_vars[0])

    def emit_immediate(self, instruction):
        immediate = instruction.src_vars[0]
        dest_reg = self.find_register(instruction.dest_vars[0], for_write=True)
        dest_type = instruction.dest_vars[0].type_name
        initial_value = f"0x{float_to_hex(float(immediate))}"
        offset = self.alloc_scratch_offset(immediate, dest_type, initial_value, f"constant {dest_type} {immediate}")
        self.out.emit_ldr(dest_reg, self.scratch_memory_adr.register, offset, f"{instruction.dest_vars[0]} <= f32({immediate})")
        self.emit_dbg_tap(instruction.dest_vars[0])

    def emit_prelude(self) -> ARMCode:
        prelude = ARMCode()
        prelude.emit_global("run")
        prelude.emit_label("run", "entry point")
        prelude.emit_stp("x29", "x30", -16, "save frame pointer and return adr")
        prelude.emit_mov("x29", "sp", "set up frame pointer")
        self.emit_alloc_spill(prelude)
        prelude.emit_adr(self.scratch_memory_adr.register, "scratch", "load constant memory address")
        return prelude

    #-------------------------------------------------------------------------

    # returns byte index from start of scratch memory
    def alloc_scratch_offset(self, key: str, type_name: str, initial_val: int, comment: str) -> int:
        offset = self.scratch.get(key, None)
        if offset is None:
            n_bytes = int(type_name[1:])/8
            offset = int(self.scratch_memory_size)  # todo: align if necessary
            self.scratch_memory_size += n_bytes
            self.scratch[key] = offset
            self.scratch_initials[key] = initial_val
            self.scratch_comments[key] = comment
        return offset
    
    # finds byte index from start of memory
    def get_scratch_offset(self, key: str) -> int:
        offset = self.scratch.get(key, None)
        return offset
    
    # outputs the scratch memory section to assembly
    def output_memory_section(self, out_block: ARMCode):
        out_block.emit_align(16)
        out_block.emit_section(".rodata", "constant data section")
        out_block.emit_label("scratch", "label for constant memory")
        for i, (key, offset) in enumerate(self.scratch.items()):
            out_block.emit_word(self.scratch_initials[key], self.scratch_comments[key])
    
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

    #-------------------------------------------------------------------------
    # debugging

    def add_dbg_taps(self):
        if not self.block.dbg: return
        for var in self.block.vars.values():
            self.alloc_scratch_offset(var.name, var.type_name, "0x00000000", f"tap {var.name}")

    def emit_dbg_tap(self, var: Var):
        if not self.block.dbg: return
        offset = self.get_scratch_offset(var.name)
        if offset is not None:
            self.out.emit_str(var.register, self.scratch_memory_adr.register, offset, f"tap {var.name}")


    
    
        



        
        
        


