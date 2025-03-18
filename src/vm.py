# ᕦ(ツ)ᕤ
# vm.py
# author: asnaroo
# zero to anything

from typing import Dict
from symbols import *
from entity import *
from copy import deepcopy
import subprocess

#--------------------------------------------------------------------------------------------------

# VMVar is a virtual variable that can be held in a register or spilled to memory
class VMVar:
    def __init__(self, name: str, type: str):
        self.name = name                    # name in original code
        self.type = type                    # concrete type, eg. "i32", "i64", "f32", "f64" etc
        self.live_range = (-1, -1)          # instruction index of first write and last read
        self.register = None                # physical register (dynamic)
        self.spill_slot = None              # byte-offset in spill-memory (dynamic)
    def __str__(self): return f"{self.name}"
    def __repr__(self): return str(self)

# VMConst is a constant that can be i/f<nbits>, eg. i32, f32, etc
class VMConst:
    def __init__(self, value: str, type: str):
        self.value = value                    # value as a string
        self.type = type                      # concrete type, eg. "i32", "i64", "f32", "f64" etc   
    def __str__(self): return f"{self.value}"
    def __repr__(self): return str(self)

# VMConstArray is a fixed-size array of some type (u8, i32, etc)
class VMConstArray:
    def __init__(self, values: List[str], type: str):
        self.values = values
        self.type = type
    def __str__(self): return f"{self.values}"
    def __repr__(self): return str(self)

# instruction: can be either VM or processor specific
class VMInstruction:
    def __init__(self, opcode: str, dest, sources: List[VMVar|VMConst], comment: str = ""):
        self.opcode : str = opcode                      # opcode
        self.dest : VMVar = dest                  # destination variables
        self.sources : List[VMVar|VMConst] = sources        # source variables or constants
        self.comment : str = comment                    # comment
    def __str__(self):
        line = f"{self.opcode}"
        if self.dest is not None: line += f" {str(self.dest)}"
        if len(self.sources) > 0: line += f", {', '.join([str(var) for var in self.sources])}"
        return line
    def __repr__(self): return str(self)
    def show(self) -> str:
        return f"{self.dest.name} <= {self.opcode} {', '.join([str(var) for var in self.sources])}"
       
# instruction block : just a list of instructions
class VMBlock:
    def __init__(self):
        self.instructions = []
        self.vars = {}              # str => VMVar
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
            type = log_grey(f"{instr.dest.type}")
            lhs = f"{ii}{type} {instr.dest.name}"
            lhs += " " * (max_length - len(lhs))
            lhs += f"{log_grey("<=")} {instr.opcode} {', '.join([str(var) for var in instr.sources])}\n"
            out += lhs
        out += f"max_live: {self.max_live}\n"
        return out
    
    def __repr__(self):
        return str(self)
    
    # find the index of the last instruction that writes to (var)
    def find_last_write(self, i_instruction, var: VMVar):
        i = i_instruction -1
        while i >= 0 and var != self.instructions[i].dest:
            i -= 1
        return i
    
    # find the index of the next instruction that reads (var)
    def find_next_read(self, i_instruction: int, var: VMVar):
        i = i_instruction + 1
        while i < len(self.instructions) and var not in self.instructions[i].sources:
            i += 1
        return i
    
    # find the index of the last instruction that reads (var)
    def find_last_read(self, var: VMVar):
        for i in range(len(self.instructions) - 1, -1, -1):
            if var in self.instructions[i].sources:
                return i
        return -1

# VMProgram is a collection of VMBlocks
class VMProgram:
    def __init__(self):
        self.blocks = []            # basic blocks
        self.constants = {}         # str => VMConst|VMConstArray
    

#--------------------------------------------------------------------------------------------------
# configuration options for code production

class VMCodegenConfig:
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

class VMCodeGenerator:
    def __init__(self):
        self.config = None
        self.st = None
        self.grammar = None
        self.indent = 0
        self.i_var = 0
        self.program = VMProgram()
        self.block = VMBlock()
        self.program.blocks.append(self.block)

    def setup(self, config: VMCodegenConfig, st: SymbolTable, grammar: Grammar):
        self.config = config
        self.st = st
        self.grammar = grammar

    # get the first entity of the required type with the given name
    def find_entity(self, key: str, of_type: Any) -> Entity:
        items = self.st.find(key, of_type, None, True)
        return items[0].element if len(items) == 1 else None

    # clear everything out
    def reset(self):
        self.block = VMBlock()
        self.i_var = 0
        self.indent = 0
    
    # allocate a new variable, return its unique identifier (index)
    def alloc_var_index(self) -> int:
        result = self.i_var
        self.i_var += 1
        return result
    
    # add a new VM VMInstruction to the current block
    def output(self, opcode, dest: Lex, sources: List[Lex]):   
        dest = str(dest)
        sources = [str(var) for var in sources]
        txt = "    "*self.indent + f"{opcode} {dest}, {', '.join(sources)}\n"
        log(log_green(txt))
        dest = self.block.vars[dest]
        sources = [(self.block.vars[var_str] if "_" in var_str else VMConst(var_str, dest.type)) for var_str in sources ]
        self.block.instructions.append(VMInstruction(opcode, dest, sources))

    # add a new VM variable to the current block (name should contain index)
    def add_var(self, var_name, type):
        var_name = str(var_name)
        type = str(type)
        concrete_type_name = self.config.get_concrete_type(str(type))
        log(f"assert_type {var_name} {type} => {concrete_type_name}")
        if var_name in self.block.vars: raise Exception(f"variable {var_name} already exists")
        self.block.vars[var_name] = VMVar(var_name, concrete_type_name)

    # add a string constant
    def add_string_constant(self, value: str): pass


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
    def __init__(self, block: VMBlock):
        self.block = block
    
    # apply various optimisations (this will grow over time)
    def optimise(self):
        self.measure_pressure()
        log_clear()
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
                self.block.instructions[i_source].dest = instruction.dest
                vars_to_replace[instruction.sources[0]] = instruction.dest
                to_remove.append(i)
        for i in sorted(to_remove, reverse=True):
            del self.block.instructions[i]
        for i, instruction in enumerate(self.block.instructions):
            for j, var in enumerate(instruction.sources):
                if var in vars_to_replace:
                    instruction.sources[j] = vars_to_replace[var]
        return self
    
    # move 'ld' instructions forward to be as close as possible to where their destination variable is read
    def optimise_lds(self):
        for i in range(len(self.block.instructions) - 1, -1, -1):
            instruction = self.block.instructions[i]
            if instruction.opcode != "const": continue
            i_read = self.block.find_next_read(i, instruction.dest)
            if i_read < len(self.block.instructions):
                i_dest = i_read - 1
                if i_dest > i:
                    self.block.instructions = self.block.instructions[:i] + self.block.instructions[i+1:i_dest] + [instruction] + self.block.instructions[i_dest:]
        return self
    
    # compute live ranges for all variables
    def compute_live_ranges(self):
        for i in range(len(self.block.instructions)):
            instruction = self.block.instructions[i]
            if instruction.dest is not None:
                i_last_read = self.block.find_last_read(instruction.dest)
                instruction.dest.live_range = (i, i_last_read)
    
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
            if instruction.dest is not None:
                live_vars.add(instruction.dest)
            for src_var in instruction.sources:
                i_last_read = self.block.find_last_read(src_var)
                if i_last_read == i and src_var in live_vars:
                    live_vars.remove(src_var)
                max_pressure = max(max_pressure, len(live_vars))
            #log(f"live_vars {i}: {live_vars} ({len(live_vars)})")
        self.block.max_live = max_pressure
        return self
    
