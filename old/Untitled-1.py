
#--------------------------------------------------------------------------------------------------
# CPU backend

class CPUBackend(Backend):
    # set up with path and target ISA
    def __init__(self, path: str, isa: 'ISA', dbg: bool):
        super().__init__(path)
        self.isa = isa
        self.out = ISAInstructionBlock()
        self.arithmetic_ops = ["add", "sub", "mul", "div", "sqrt"]
        self.data_adr = Var("data_adr", "i64")
        self.dbg = dbg

    # generate code for the target ISA
    def generate(self, block: Block):
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
    def emit_instruction(self, instruction: Instruction):
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
    def find_register(self, var: Var, for_write: bool) -> str:
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
    def free_eol_registers(self, vars: List[Var|str]):
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
    def emit_dbg_tap(self, var: Var):
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
    
    def reset(self, block: Block):
        self.block : Block = block
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
