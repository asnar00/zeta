# ᕦ(ツ)ᕤ
# codegen.py
# author: asnaroo
# zero to anything

from typing import Dict
from src.symbols import *
from src.ast import *
from copy import deepcopy
import subprocess

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
                i_source = self.find_assignment_reverse(i, instruction.src_vars[0])
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
            i_read = self.find_read_forward(i, instruction.dest_vars[0])
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
                i_last_read = self.find_last_read(var)
                var.live_range = (i, i_last_read)
    
    #----------------------------------------------------------------------------------------------
    # below the line
    
    def find_assignment_reverse(self, i_instruction, var: Var):
        i = i_instruction -1
        while i >= 0 and var not in self.block.instructions[i].dest_vars:
            i -= 1
        return i
    def find_read_forward(self, i_instruction, var: Var):
        i = i_instruction + 1
        while i < len(self.block.instructions) and var not in self.block.instructions[i].src_vars:
            i += 1
        return i
    def find_last_read(self, var: Var):
        for i in range(len(self.block.instructions) - 1, -1, -1):
            if var in self.block.instructions[i].src_vars:
                return i
        return -1
    def try_move(self, i_instruction) -> int:  # find earliest index we can move to, or -1 if not possible
        this_instruction = self.block.instructions[i_instruction]
        i_instructions = [self.find_assignment_reverse(i_instruction, var) for var in this_instruction.src_vars]
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
                i_last_read = self.find_last_read(src_var)
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
    def run(self, path: str) -> str: return ""

#--------------------------------------------------------------------------------------------------
# Python backend runs numpy

class PythonBackend(Backend):
    def generate(self, block: InstructionBlock):
        out = self.header()

        op_map = { "add" : "+", "sub" : "-", "mul" : "*", "div" : "/", "sqrt" : "np.sqrt" }
        type_map = { "f32" : "np.float32", "i32" : "np.int32" }

        for instruction in block.instructions:
            dest_var = instruction.dest_vars[0].replace(".", "_")
            if instruction.opcode == "imm":
                type = type_map[block.type_map[instruction.dest_vars[0]]]
                out += f"    {dest_var} = {type}({instruction.src_vars[0]})\n"
            else:
                opcode = op_map[instruction.opcode]
                operands = [var.replace(".", "_") for var in instruction.src_vars]
                if opcode in "+-*/":
                    out += f"    {dest_var} = {operands[0]} {opcode} {operands[1]}\n"
                elif len(operands) == 1:
                    out += f"    {dest_var} = {opcode}({operands[0]})\n"

        for instruction in block.instructions:
            dest_var = instruction.dest_vars[0].replace(".", "_")
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
# ARM backends runs on the M1

class RegisterSet:
    def __init__(self, n_registers: int, name_map: Dict[str, str]):
        self.n_registers = n_registers
        self.name_map = name_map
        self.reset()

    def reset(self):
        self.free_registers = set()
        for i in range(0, self.n_registers): self.free_registers.add(i)

    def alloc_register(self, type_name: str) -> str:
        if self.free_registers:
            reg = self.free_registers.pop()
            return f"{self.name_map[type_name]}{reg}"
        return None
    
    def free_register(self, reg: str):
        i_reg = int(reg[1:])
        if i_reg in self.free_registers:
            raise Exception(f"register {reg} is already free")
        self.free_registers.add(i_reg)

class ARMBackend(Backend):
    def __init__(self, path: str):
        super().__init__(path)
        self.int_registers = RegisterSet(32, { "i32" : "w", "i64" : "x" })
        self.fp_registers = RegisterSet(32, { "f32" : "s", "f64" : "d", "f32x4" : "v" })
        self.register_sets = { "i" : self.int_registers, "f" : self.fp_registers }
        self.var_to_reg = {}         # Map variable names to physical registers
        self.reg_to_var = {}         # Reverse map to track register usage
        self.spills = []             # Spilled variables for debugging or further handling
    
    def generate(self, block: InstructionBlock):
        self.reset()
        pass


    #-------------------------------------------------------------------------
    # below the line

    def reset(self):
        for register_set in self.register_sets.values(): register_set.reset()
        self.var_to_reg = {} 
        self.reg_to_var = {}
        self.spills = []

    def var_to_register(self, var_name: str, type_name: str) -> str:
        register_set = self.register_sets[type_name[0]]
        reg = register_set.alloc_register(type_name)
        if not reg:
            reg = self.spill_register(register_set, type_name)
        self.var_to_reg[var_name] = reg
        self.reg_to_var[reg] = var_name
        return reg
    
    def spill_register(self, register_set: RegisterSet, type_name: str) -> str:
        pass



        
        
        


