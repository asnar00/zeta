# ᕦ(ツ)ᕤ
# codegen.py
# author: asnaroo
# zero to anything

from typing import Dict
from src.symbols import *
from src.entity import *
from copy import deepcopy

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
    
class Instruction:
    def __init__(self, opcode: str, dest_vars: List[str], src_vars: List[str]):
        self.opcode = opcode
        self.dest_vars = dest_vars
        self.src_vars = src_vars

class InstructionBlock:
    def __init__(self):
        self.instructions = []
        self.type_map = {} # map variable name => concretetype name

    def __str__(self):
        max_length = max(len(var) for var in self.type_map.keys()) if self.type_map else 0
        max_length += max((len(type_name) for type_name in self.type_map.values()), default=0)
        max_digits = len(str(len(self.instructions)))
        max_length += max_digits + 26

        out = ""
        for i, instr in enumerate(self.instructions):
            ii = log_grey(f"{i:{max_digits}}: ")
            type = log_grey(f"{self.type_map[instr.dest_vars[0]]}")
            lhs = f"{ii}{type} {instr.dest_vars[0]}"
            lhs += " " * (max_length - len(lhs))
            lhs += f"{log_grey("<=")} {instr.opcode} {', '.join(instr.src_vars)}\n"
            out += lhs
        return out
    
    def __repr__(self):
        return str(self)
    
    def optimise(self):
        log_clear()
        log("\nbefore optimisation:")
        log(str(self))
        self.measure_pressure()
        self.optimise_live_ranges()
        self.optimise_movs()
        self.optimise_lds()
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
        for i, instruction in enumerate(self.instructions):
            if instruction.opcode == "mov":
                i_source = self.find_assignment_reverse(i, instruction.src_vars[0])
                self.instructions[i_source].dest_vars[0] = instruction.dest_vars[0]
                vars_to_replace[instruction.src_vars[0]] = instruction.dest_vars[0]
                to_remove.append(i)
        for i in sorted(to_remove, reverse=True):
            del self.instructions[i]
        for i, instruction in enumerate(self.instructions):
            for j, var in enumerate(instruction.src_vars):
                if var in vars_to_replace:
                    instruction.src_vars[j] = vars_to_replace[var]
        return self
    
    # move 'ld' instructions forward to be as close as possible to where their dest_vars are read
    def optimise_lds(self):
        for i in range(len(self.instructions) - 1, -1, -1):
            instruction = self.instructions[i]
            if instruction.opcode != "imm": continue
            i_read = self.find_read_forward(i, instruction.dest_vars[0])
            if i_read < len(self.instructions):
                i_dest = i_read - 1
                if i_dest > i:
                    self.instructions = self.instructions[:i] + self.instructions[i+1:i_dest] + [instruction] + self.instructions[i_dest:]
        return self
    
    #----------------------------------------------------------------------------------------------
    # below the line
    
    def find_assignment_reverse(self, i_instruction, var_name):
        i = i_instruction -1
        while i >= 0 and var_name not in self.instructions[i].dest_vars:
            i -= 1
        return i
    def find_read_forward(self, i_instruction, var_name):
        i = i_instruction + 1
        while i < len(self.instructions) and var_name not in self.instructions[i].src_vars:
            i += 1
        return i
    def find_last_read(self, var_name):
        for i in range(len(self.instructions) - 1, -1, -1):
            if var_name in self.instructions[i].src_vars:
                return i
        return -1
    def try_move(self, i_instruction) -> int:  # find earliest index we can move to, or -1 if not possible
        this_instruction = self.instructions[i_instruction]
        i_instructions = [self.find_assignment_reverse(i_instruction, var) for var in this_instruction.src_vars]
        i_can_move = max(i_instructions)
        if i_can_move >= 0 and (i_can_move+1) < i_instruction:
            #log(f"moving {i_instruction}:{this_instruction} to {i_can_move+1}")
            i_move_to = i_can_move + 1
            tmp = self.instructions[:i_instruction] + self.instructions[i_instruction+1:]
            self.instructions = tmp[:i_move_to] + [this_instruction] + tmp[i_move_to:]
            return i_move_to
        return -1
    def try_moves(self) -> int: # return number of moves made
        n_moves = 0
        for i in range(0, len(self.instructions)):
            i_moved_to = self.try_move(i)
            if i_moved_to >= 0: n_moves += 1
        return n_moves
    
    def measure_pressure(self): # for each instruction, find the number of live vars
        max_pressure = 0
        live_vars = set()
        for i in range(len(self.instructions)):
            instruction = self.instructions[i]
            live_vars.update(instruction.dest_vars)
            for src_var in instruction.src_vars:
                if not ("_" in src_var): continue
                i_last_read = self.find_last_read(src_var)
                if i_last_read == i and src_var in live_vars:
                    live_vars.remove(src_var)
                max_pressure = max(max_pressure, len(live_vars))
            #log(f"live_vars {i}: {live_vars} ({len(live_vars)})")
        log(f"max_live: {max_pressure}")
        return self


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
    
    def add_var(self) -> int:
        result = self.i_var
        self.i_var += 1
        return result
    
    def output(self, opcode, dest_vars: List[str], src_vars: List[str]):    
        txt = "    "*self.indent + f"{opcode} {', '.join(dest_vars)}, {', '.join(src_vars)}\n"
        log(log_green(txt))
        self.block.instructions.append(Instruction(opcode, dest_vars, src_vars))

    def assert_type(self, var_name, type_name):
        concrete_type_name = self.config.get_concrete_type(str(type_name))
        log(f"assert_type {var_name} {type_name} => {concrete_type_name}")
        self.block.type_map[var_name] = concrete_type_name

    def show(self, e):
        return print_code_formatted(e, self.grammar).replace("\n", "↩︎").replace("    ", "")
    

    
#--------------------------------------------------------------------------------------------------
# super below the line

def try_replace(var, replace):
    if "." in var:
        vars = var.split(".")
        if vars[0] in replace:
            return replace[vars[0]] + "." + ".".join(vars[1:])
    else:
        if var in replace: return replace[var]
    return var