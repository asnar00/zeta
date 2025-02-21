 
# Virtual Machine variable
class VmVar:
    def __init__(self, name: str, type_name: str):
        self.name = name
        self.type_name = type_name          # concrete type name
        self.live_range = (-1, -1)          # instruction index of first write and last read
        self.register = None                # physical register name (dynamic)
        self.spill_index = None             # index in spill-memory (dynamic)
    def __str__(self): return self.name
    def __repr__(self): return self.name
    def size_bytes(self) -> int:
        return int(self.type_name[1:]) // 8

# Virtual Machine instruction
class VmInstruction:
    def __init__(self, opcode: str, dest_vars: List[VmVar], src_vars: List[VmVar]):
        self.opcode = opcode         # opcode
        self.dest_vars = dest_vars   # destination variables
        self.src_vars = src_vars     # source variables or constants

# Virtual Machine instruction block
class VmBlock:
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
    
    # find the index of the last instruction that writes to (var)
    def find_last_write(self, i_instruction, var: VmVar):
        i = i_instruction -1
        while i >= 0 and var not in self.instructions[i].dest_vars:
            i -= 1
        return i
    
    # find the index of the next instruction that reads (var)
    def find_next_read(self, i_instruction: int, var: VmVar):
        i = i_instruction + 1
        while i < len(self.instructions) and var not in self.instructions[i].src_vars:
            i += 1
        return i
    
    # find the index of the last instruction that reads (var)
    def find_last_read(self, var: VmVar):
        for i in range(len(self.instructions) - 1, -1, -1):
            if var in self.instructions[i].src_vars:
                return i
        return -1

#--------------------------------------------------------------------------------------------------
# VM Code Generator

class VmCodeGenerator:
    def __init__(self):
        self.config = None
        self.st = None
        self.grammar = None
        self.indent = 0
        self.i_var = 0
        self.block = VmBlock()

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
        self.block = VmBlock()
        self.i_var = 0
        self.indent = 0
    
    # allocate a new variable, return its unique identifier (index)
    def alloc_var_index(self) -> int:
        result = self.i_var
        self.i_var += 1
        return result
    
    # add a new VM Instruction to the current block
    def output(self, opcode, dest_vars: List[Lex], src_vars: List[Lex]):   
        dest_vars = [str(var) for var in dest_vars]
        src_vars = [str(var) for var in src_vars]
        txt = "    "*self.indent + f"{opcode} {', '.join(dest_vars)}, {', '.join(src_vars)}\n"
        log(log_green(txt))
        dest_vars = [self.block.vars[var_str] for var_str in dest_vars]
        src_vars = [(self.block.vars[var_str] if "_" in var_str else var_str) for var_str in src_vars ]
        self.block.instructions.append(VmInstruction(opcode, dest_vars, src_vars))

    # add a new VM variable to the current block (name should contain index)
    def add_var(self, var_name, type_name):
        var_name = str(var_name)
        type_name = str(type_name)
        concrete_type_name = self.config.get_concrete_type(str(type_name))
        log(f"assert_type {var_name} {type_name} => {concrete_type_name}")
        if var_name in self.block.vars: raise Exception(f"variable {var_name} already exists")
        self.block.vars[var_name] = VmVar(var_name, concrete_type_name)

    # show the current block as a string
    def show(self, e):
        return print_code_formatted(e, self.grammar).replace("\n", "↩︎").replace("    ", "")
    
#--------------------------------------------------------------------------------------------------
# super below the line (this should really go in zero.py!)

# given a name of format a.b.c, try to replace the first part with a value from the replace map
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

class VmOptimiser:
    def __init__(self, block: VmBlock):
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
    
    # figure out where an instruction can be moved back to, or -1 if not possible
    def try_move(self, i_instruction) -> int:
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
    def generate(self, block: VmBlock) -> str: pass
    def run(self) -> str: return ""

#--------------------------------------------------------------------------------------------------
# Python backend runs numpy

class PythonBackend(Backend):
    def generate(self, block: VmBlock):
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
# CPU backend

class CPUBackend(Backend):
    # set up with path and target ISA
    def __init__(self, path: str, isa: 'ISA', dbg: bool):
        super().__init__(path)
        self.isa = isa
        self.out = ISAInstructionBlock()
        self.arithmetic_ops = ["add", "sub", "mul", "div", "sqrt"]
        self.data_adr = VmVar("data_adr", "i64")
        self.dbg = dbg

    # generate code for the target ISA
    def generate(self, block: VmBlock):
        self.block = block
        self.reset()
        self.out.label("start")
        self.generate_header()
        for i, instruction in enumerate(block.instructions):
            self.i_instruction = i
            self.emit_instruction(instruction)
        self.generate_footer()
        self.out.finalise()
        self.out.verify(self.out)
        log(self.out.show())
        self.out.write_to_file(self.path)
        return self
    
    #----------------------------------------------------------------------------------------------

    # reset
    def reset(self):
        self.i_instruction = 0
        self.isa.register_manager.reset(self.block)
        self.find_register(self.data_adr, for_write=True)
        self.add_dbg_taps()

    # generate start-of-function stuff (params, stack  management, etc)
    def generate_header(self):
        for instruction in self.isa.function_prologue():
            self.out.emit(instruction)

    # generate end-of-function stuff (return, etc)
    def generate_footer(self):
        for instruction in self.isa.function_epilogue():
            self.out.emit(instruction)

    # generate a single instruction, plus any spills / unspills
    def emit_instruction(self, instruction: VmInstruction):
        if instruction.opcode == "imm":
            return self.emit_immediate(instruction)
        elif instruction.opcode in self.arithmetic_ops:
            return self.emit_arithmetic_op(instruction)
        else: raise Exception(f"unsupported VM opcode {instruction.opcode}")

    #----------------------------------------------------------------------------------------------

    def emit_arithmetic_op(self, instruction):
        src_regs = [self.find_register(src_var, for_write=False) for src_var in instruction.src_vars]
        self.free_eol_registers(instruction.src_vars)
        dest_reg = self.find_register(instruction.dest_vars[0], for_write=True)
        comment = f"{instruction.dest_vars[0]} <= {instruction.opcode}{str(instruction.src_vars).replace('[', ' ').replace(']', '')}"
        self.emit(instruction.opcode, dest_reg, src_regs, comment)
        self.emit_dbg_tap(instruction.dest_vars[0])

    def emit_immediate(self, instruction):
        immediate = instruction.src_vars[0]
        dest_reg = self.find_register(instruction.dest_vars[0], for_write=True)
        dest_type = instruction.dest_vars[0].type_name
        dest_size = int(dest_type[1:]) // 8
        initial_value = pack_float(float(immediate))
        constant_name = f"constant_{dest_type}_{immediate}"
        offset = self.out.symbol(constant_name, dest_size, initial_value)
        self.emit("ldr", dest_reg, [self.data_adr.register, offset], f"load {dest_type} {immediate}")
        self.emit_dbg_tap(instruction.dest_vars[0])

    def emit(self, opcode: str, dest_reg: str, operands: List[str], comment: str):
        isa_instruction = self.isa.encode(opcode, dest_reg, operands, comment)
        self.out.emit(isa_instruction)

    #----------------------------------------------------------------------------------------------

    # find a register for a variable, and emit spills / unspills if necessary
    def find_register(self, var: VmVar, for_write: bool) -> str:
        if isinstance(var, str): return var
        #log(f"i{self.i_instruction}: find_register {var.name} {"for write" if for_write else "for read"}")
        if var.register:
            return var.register
        reg_result = self.isa.register_manager.find_register(var, for_write, self.i_instruction)
        if reg_result.store_offset != None:
            self.emit("str", reg_result.register, ["sp", reg_result.store_offset], f"spill {var.name}")
        if reg_result.load_offset != None:
            self.emit("ldr", reg_result.register, ["sp", reg_result.load_offset], f"unspill {var.name}")
        return reg_result.register
    
    # if any variables are at the end of their live range, free their registers
    def free_eol_registers(self, vars: List[VmVar|str]):
        for var in vars:
            if isinstance(var, str): continue
            if var.live_range[1] <= self.i_instruction:
                self.isa.register_manager.free(var)

    #----------------------------------------------------------------------------------------------

    # add debug taps for all variables, if dbg is enabled
    def add_dbg_taps(self):
        if not self.block.dbg: return
        for var in self.block.vars.values():
            self.out.symbol(var.name, var.size_bytes(), None)

    # emit a debug tap for a variable
    def emit_dbg_tap(self, var: VmVar):
        if not self.block.dbg: return
        offset = self.out.find_symbol(var.name)
        self.emit("str", var.register, [self.data_adr.register, offset], f"tap {var.name}")

#--------------------------------------------------------------------------------------------------
# CPU Instruction Sets and all that fun stuff

# ISA is the base class for all CPU instruction sets
class ISA:
    def __init__(self): self.register_manager = RegisterManager()
    def encode(self, opcode, dest, operands, comment) -> 'ISAInstruction': pass
    def function_prologue(self) -> List['ISAInstruction']: return []
    def function_epilogue(self) -> List['ISAInstruction']: return []

# represents a single instruction in target ISA format
class ISAInstruction:
    def __init__(self, opcode: str, dest: str, operands: List[str], comment: str, instruction: int, descriptor: List[int]):
        self.opcode = opcode
        self.dest = dest
        self.operands = operands
        self.comment = comment
        self.instruction = instruction
        self.descriptor = descriptor
    def __str__(self): 
        operand_strs = [str(operand) for operand in self.operands]
        return f"{self.opcode} {self.dest} {', '.join(operand_strs)}"
    def __repr__(self): return str(self)

# represents a data variable in executable code
class ISAData:
    def __init__(self, name: str, offset_bytes: int, size_bytes: int, alloc_bytes: int, initial_value: bytes|None):
        self.name = name
        self.offset_bytes = offset_bytes
        self.size_bytes = size_bytes
        self.alloc_bytes = alloc_bytes
        self.initial_value = initial_value
    def __str__(self): return f"{self.name} {self.size_bytes} {self.initial_value}"
    def __repr__(self): return str(self)

# a block of instructions and memory in target ISA format
class ISAInstructionBlock:
    def __init__(self):
        self.instructions : List[ISAInstruction] = []   # instructions with all metadata
        self.data : List[ISAData] = []                  # data variables
        self.data_bytes = 0                             # total size of data variables including padding
        self.binary = array.array('B')                  # binary instructions and data
        self.labels = {}                                # name => address relative to start of code
        self.offset_to_label = {}                       # address => name
        self.offset_to_data = {}                        # address => ISAData
        self.symbols = {}                               # name => ISAData

    def emit(self, instruction: ISAInstruction):
        self.binary.extend(instruction.instruction.to_bytes(4, byteorder='little'))
        self.instructions.append(instruction)

    def label(self, name: str):
        self.labels[name] = len(self.binary)
        self.offset_to_label[len(self.binary)] = name

    def symbol(self, name: str, size_bytes: int, initial_value: bytes|None) -> int:
        alloc_size = ((size_bytes + 3) // 4) * 4    # minimum alloc-size of 4 bytes
        offset = ((self.data_bytes + (alloc_size-1)) // alloc_size) * alloc_size # align
        isa_data = ISAData(name, offset, size_bytes, alloc_size, initial_value)
        self.data.append(isa_data)
        self.data_bytes += alloc_size
        self.symbols[name] = isa_data
        self.offset_to_data[offset] = isa_data
        return offset
    
    # return offset of symbol in bytes
    def find_symbol(self, name: str) -> int: 
        return self.symbols[name][0]
    
    def extend(self, block: 'ISAInstructionBlock'):
        self.instructions.extend(block.instructions)
        self.data.extend(block.data)
        self.data_bytes += block.data_bytes
        self.binary.extend(block.binary)
        self.labels.update(block.labels)
        self.offset_to_label.update(block.offset_to_label)
        self.offset_to_data.update(block.offset_to_data)
        self.symbols.update(block.symbols)
    
    # add the data section on to the end of the binary
    def finalise(self):
        self.label("data")
        for isa_data in self.data:
            offset = len(self.binary)
            align = isa_data.alloc_bytes
            padding = (align - (offset % align)) % align
            if padding > 0: 
                self.binary.extend(bytearray(padding))  # pad to align start of data
            self.binary.extend(isa_data.initial_value)
            padding = isa_data.alloc_bytes - isa_data.size_bytes
            if padding > 0:
                self.binary.extend(bytearray(padding))  # pad if alloc > size

    def verify(self, isa: 'ISA'):
        pass
    
    def write_to_file(self, path: str):
        with open(path.replace(".*", ".bin"), "wb") as f:
            f.write(self.binary)

    def show(self)-> str:
        result = ""
        for offset in range(0, len(self.binary), 4):
            out = ""
            if offset in self.offset_to_label:
                out += f"{self.offset_to_label[offset]}:"
                result += out + "\n"
                out = ""
            index = log_grey(f"{offset:03x}:")
            out += f"  {index} "
            for i in range(0, 4):
                out += f"{self.binary[offset+i]:02x}"
            out += "  "
            i_instruction = offset // 4
            if i_instruction < len(self.instructions):
                instruction = self.instructions[i_instruction]
                out += str(instruction)
                out += " " * (45-len(out))
                out += log_grey("// " + instruction.comment)
            else:
                data_offset = offset - (len(self.instructions)*4)
                if data_offset in self.offset_to_data:
                    isa_data = self.offset_to_data[data_offset]
                    out += f".word"
                    out += " " * (45-len(out))
                    out += log_grey(f"// {isa_data.name}")
            result += out + "\n"
        return result
#--------------------------------------------------------------------------------------------------
# register stuff

# single register set of a single type (float or int)
class RegisterSet:
    def __init__(self, n_registers: int, name_map: Dict[str, str]):
        self.n_registers = n_registers
        self.name_map = name_map
        self.reset()

    def reset(self):
        self.assigned_vars = [None for i in range(0, self.n_registers)]
        # artificially occupy some registers
        dummy_var = VmVar("dummy", "f32")
        for i in range(2, self.n_registers): self.assigned_vars[i] = dummy_var

    def allocate(self, var: VmVar) -> str:
        for i in range(0, self.n_registers):
            if self.assigned_vars[i] == None:
                self.assigned_vars[i] = var
                reg = self.name_map[var.type_name] + str(i)
                var.register = reg
                return reg
        return None
    
    def free(self, var: VmVar):
        if var.register == None: return
        self.assigned_vars[int(var.register[1:])] = None
        var.register = None

    def transfer(self, to_var: VmVar, from_var: VmVar):
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
        
    def allocate(self, var: VmVar) -> str:
        register_set = self.register_sets[var.type_name[0]]
        return register_set.allocate(var)
    
    def free(self, var: VmVar):
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

    def assign_spill_index(self, var: VmVar) -> int:      # return byte offset to store to
        index = next((i for i, var in enumerate(self.spilled_vars) if var is None), None)
        if index is None:
            index = len(self.spilled_vars)
            self.spilled_vars.append(None)
            self.max_spills = max(self.max_spills, len(self.spilled_vars))
        self.spilled_vars[index] = var
        var.spill_index = index
        return index * self.n_bytes

    def unspill_var(self, var: VmVar) -> int:     # return byte offset to load from
        index = var.spill_index
        self.spilled_vars[index] = None
        var.spill_index = None
        return index * self.n_bytes

# holds a register, two offsets (store, and load)
class RegisterResult:
    def __init__(self, register: str, store_offset: int, spill_var: VmVar, load_offset: int):
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
    
    def reset(self, block: VmBlock):
        self.block : VmBlock = block
        self.register_allocator.reset()
        for spill_manager in self.spill_managers.values(): spill_manager.reset()
    
    def find_register(self, var: VmVar, for_write: bool, i_instruction: int) -> RegisterResult:        # spills / unspills as necessary
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
        
    def free(self, var: VmVar):
        self.register_allocator.free(var)
        
    def spill_something(self, var: VmVar, spill_manager: SpillManager, i_instruction: int) -> Tuple[int, VmVar]:
        victim_var = self.select_victim(var.type_name, i_instruction)
        store_index = spill_manager.assign_spill_index(victim_var)
        self.register_allocator.free(victim_var)
        return store_index, victim_var

    def select_victim(self, type_name: str, i_instruction: int) -> VmVar:
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
# ARM ISA

class ARMISA(ISA):
    # initialise: set up up register sets with types and name prefixes as commonly used
    def __init__(self):
        self.int_registers = RegisterSet(32, { "i32" : "w", "i64" : "x" })
        self.fp_registers = RegisterSet(32, { "f32" : "s", "f64" : "d", "f32x4" : "v" })
        self.register_sets = { "i" : self.int_registers, "f" : self.fp_registers }
        self.register_manager = RegisterManager(self.register_sets)
        self.arithmetic_ops = ["add", "sub", "mul", "div", "sqrt"]
        self.load_store_ops = ["ldr", "str", "adr"]

    # encode an instruction (operation, dest-register name, operands names (register names or immediates) to a 32-bit integer
    def encode(self, op, dest, operands, comment) -> ISAInstruction:
        arm_op, instruction, descriptor = None, None, None
        type, size, rd = self.parse_operand(dest)
        type1, size1, rn = self.parse_operand(operands[0])
        type2, size2, rm = self.parse_operand(operands[1]) if len(operands) > 1 else (None, None, None)
        if type == "float": 
            arm_op, instruction, descriptor = self.encode_float(op, rd, rn, rm, size, [type1, type2], comment)
        else:
            arm_op, instruction, descriptor = self.encode_int(op, rd, rn, rm, size, [type1, type2], comment)
        return ISAInstruction(arm_op, dest, operands, comment, instruction, descriptor)
    
    # function prologue
    def function_prologue(self) -> List['ISAInstruction']: return []
    #----------------------------------------------------------------------------------------------
    # these are internal functions - reorganise for other versions if necessary

    # encode a float instruction
    def encode_float(self, op, rd, rn, rm, size, types, comment) -> Tuple[str, int, List[int]]:
        if op in self.arithmetic_ops:
            return self.encode_float_op(op, rd, rn, rm, size)
        elif op in self.load_store_ops:
            return self.encode_float_ldr_str(op, rd, rn, rm, size)
        raise Exception(f"unsupported opcode {op}")
    
    # encode a float arithmetic instruction - return arm opcode, instruction, and descriptor
    def encode_float_op(self, op, rd, rn, rm, size) -> Tuple[str, int, List[int]]:
        arm_opcode = "f" + op
        opcode_map = { "fadd": 0b00011110001, "fsub": 0b00011110001, "fmul": 0b00011110001, "fsqrt": 0b00011110001 }
        op_map = { "fadd" : 0b001010, "fsub" : 0b001110, "fmul" : 0b000010, "fsqrt" : 0b110000 }
        if not arm_opcode in opcode_map: raise Exception(f"unsupported float-arithmetic op {arm_opcode}")
        opcode_bits = opcode_map[arm_opcode]
        op_bits = op_map[arm_opcode]
        if size==8: opcode_bits |= (1 << 4)
        if rm is None: rm = 1 # funky sqrt thing for arm
        instruction = ((opcode_bits << 21) | (rm << 16) | (op_bits << 10) |(rn << 5) | rd)
        descriptor = [21, 16, 10, 5]
        return arm_opcode, instruction, descriptor

    # encode a load or store instruction to/from a float register
    def encode_float_ldr_str(self, op, rd, rn, offset, size) -> Tuple[str, int, List[int]]:
        arm_op = op
        arm_opcodes = { "ldr" : 0b10111101010, "str" : 0b10111101000 }
        if not arm_op in arm_opcodes: raise Exception(f"unsupported float-ldr-str op {arm_op}")
        arm_opcode = arm_opcodes[arm_op]
        imm = (offset // size) & 0b11111111111
        instruction = arm_opcode << 21 | (imm << 10) | (rn << 5) | rd
        return arm_op, instruction, [21, 10, 5]
    
    #----------------------------------------------------------------------------------------------
    
    # encode an integer instruction
    def encode_int(self, op, rd, rn, rm, size, types, comment) -> Tuple[str, int, List[int]]:
        if op in self.arithmetic_ops: raise Exception(f"unsupported int-arithmetic op {op}")
        elif op in self.load_store_ops: return self.encode_int_ldr_str_adr(op, rd, rn, rm, size)
        raise Exception(f"unsupported int op {op}")
    
    # encode a load or store instruction to/from an integer register
    def encode_int_ldr_str_adr(self, op, rd, rn, offset, size) -> Tuple[str, int, List[int]]:
        arm_op = op
        if op != "adr": raise Exception("unsupported int-op ldr/str") # only supporting adr for now
        immlo = (offset & 0b11) << 29
        immhi = ((offset >> 2) & 0x7FFFF) << 5
        instruction =  immlo | (0b00010000 << 24) |immhi | rd
        descriptor = [29, 24, 5]
        return arm_op, instruction, descriptor

    #----------------------------------------------------------------------------------------------
    
    # from register name, return type (int/float/imm), size (4/8) bytes, and index
    def parse_operand(self, register: str) -> Tuple[str, int, int]:
        if isinstance(register, int): return "imm", 4, register
        if register[0] in "-0123456789": return "imm", 4, int(register)
        if register == "sp": return "int", 8, 31
        type = "float" if register[0] in "sd" else "int"
        size = 8 if register[0] in "xd" else 4
        index = int(register[1:])
        return type, size, index
