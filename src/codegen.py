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

    def setup_types(self, type_map: Dict[str, str]):
        self.type_substitution.update(type_map)

    def get_concrete_type(self, abstract_type_name: str) -> str:
        return self.type_substitution.get(abstract_type_name, abstract_type_name)
    
class Instruction:
    def __init__(self, opcode: str, dest_vars: List[str], src_vars: List[str]):
        self.opcode = opcode
        self.dest_vars = dest_vars
        self.src_vars = src_vars
    def __str__(self):
        return f"{self.opcode} {', '.join(self.dest_vars)}, {', '.join(self.src_vars)}"
    def __repr__(self):
        return str(self)

class InstructionBlock:
    def __init__(self):
        self.instructions = []
    def __str__(self):
        out = ""
        for i, instruction in enumerate(self.instructions):
            ii = log_grey(f"{i:3}: ")
            out += f"{ii}{str(instruction)}\n"
        return out
    def __repr__(self):
        return str(self)
    def optimise(self):
        log_clear()
        self.optimise_live_ranges()
        self.optimise_movs()
        self.optimise_lds()
        return self

    # move instructions back to be as close as possible to where their source operands were written
    def optimise_live_ranges(self):
        safe_count = 0
        while safe_count < 10:
            log(str(self))
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
            if instruction.opcode != "ld": continue
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