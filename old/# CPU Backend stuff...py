# CPU Backend stuff... helpers

# PhysicalRegister is a physical register
class PhysicalRegister:
    def __init__(self, index: int, in_set: 'RegisterSet'):
        self.index = index
        self.in_set = in_set
        self.contents : Var|Const = None

# LogicalRegister is a sub-part of a PhysicalRegister (bottom n_bits)
class LogicalRegister:
    def __init__(self, physical_register: PhysicalRegister, n_bits: int):
        self.physical_register = physical_register
        self.n_bits = n_bits

# RegisterSet is a set of registers of the same type
class RegisterSet:
    def __init__(self, type: str, n_registers: int):
        self.type = type # "i" or "f"
        self.registers : List[PhysicalRegister] = [PhysicalRegister(i, self) for i in range(n_registers)]

# RegisterManager is a group of RegisterSets
class RegisterManager:
    def __init__(self, register_sets: Dict[str, RegisterSet]):
        self.register_sets : Dict[str, RegisterSet] = register_sets

# MemoryLocation is a named memory location somewhere in a memory block
class MemoryLocation:
    def __init__(self, name: str, type: str, size_bytes: int):
        self.name = name
        self.type = type
        self.size_bytes = size_bytes
        self.offset = None

# MemoryBlock is a named block containing a list of MemoryLocations
class MemoryBlock:
    def __init__(self, name: str):
        self.locations = {}
        self.offset_to_location = {}
        self.name = name
        self.size_bytes = 0
    def add_location(self, loc: MemoryLocation):
        alloc_bytes = ((loc.size_bytes + 3)//4)*4
        padding = alloc_bytes -(loc.size_bytes % alloc_bytes)
        self.size_bytes += padding
        loc.offset = self.size_bytes
        self.locations[loc.name] = loc
        self.offset_to_location[loc.offset] = loc
        self.size_bytes += alloc_bytes

# ProcessorInstruction is a processor-specific instruction
class ProcessorInstruction:
    def __init__(self, opcode: str, dest: LogicalRegister, sources: List[LogicalRegister|Const|MemoryLocation]):
        self.opcode = opcode
        self.dest = dest
        self.sources = sources
        self.comment = ""

# ProcessorInstructionBlock is a list of processor instructions and labels
class ProcessorInstructionBlock:
    def __init__(self):
        self.instructions = []
        self.labels = {}
        self.offset_to_label = {}

    def add_instruction(self, instruction: ProcessorInstruction, comment: str):
        instruction.comment = comment
        self.instructions.append(instruction)

    def add_label(self, label: str):
        self.labels[label] = len(self.instructions)
        self.offset_to_label[len(self.instructions)] = label

# Processor is base class for Processor type
class Processor:
    def __init__(self): self.register_manager = None                                                # subclass has to set this
    def adr(self, dest: LogicalRegister, relative_address: int) -> ProcessorInstruction: pass       # return adr instruction
    def load(self, dest: LogicalRegister, source: MemoryLocation) -> ProcessorInstruction: pass     # return load-register instruction
    def opcode(self, vm_opcode: str, type: str) -> str: pass                                        # convert VM opcode to processor code
    def prologue(self) -> ProcessorInstructionBlock: pass                                           # return prologue block           
    def epilogue(self) -> ProcessorInstructionBlock: pass                                           # return epilogue block
    def register_name(self, register: LogicalRegister) -> str: pass                                 # return register name as string
    def get_register(self, name: str) -> LogicalRegister: pass                                      # return register from name

#--------------------------------------------------------------------------------------------------
# CPUBackend

class CPUBackend(Backend):
    def __init__(self, path: str, target: 'Processor', dbg: bool):
        self.path = path
        self.target = target
        self.dbg = dbg
        self.arithmetic_ops = [ "add", "sub", "mul", "div", "sqrt" ]

    def generate(self, block: InstructionBlock):
        self.block = block
        self.code : ProcessorInstructionBlock = ProcessorInstructionBlock()
        self.data = MemoryBlock("data")
        prologue : ProcessorInstructionBlock = self.prologue()
        for i_instruction, instruction in enumerate(block.instructions):
            self.i_instruction = i_instruction
            self.emit(instruction)
        epilogue : ProcessorInstructionBlock = self.epilogue()
        output = self.join([prologue, self.code, epilogue])
        code_binary : bytes = self.output_binary(output)
        data_binary : bytes = self.output_binary_data(self.data)
        final_binary : bytes = code_binary + data_binary
        write_file(self.path.replace(".*", ".bin"), final_binary)


    #----------------------------------------------------------------------------------------------

    def prologue(self) -> ProcessorInstructionBlock:
        out = ProcessorInstructionBlock()
        # set up data register
        self.memory_register = self.find_register(Var("_data", "i64"), for_write=True)
        out.add_instruction(self.target.adr(self.memory_register, self.data), "set up data register")
        return out



    #----------------------------------------------------------------------------------------------

    def emit(self, instruction: Instruction):
        if instruction.opcode == "const": self.emit_const(instruction)
        elif instruction.opcode in self.arithmetic_ops: self.emit_arithmetic(instruction)
        else: raise Exception(f"unsupported opcode {instruction.opcode}")

    def emit_const(self, instruction: Instruction):
        dest_register = self.find_register(instruction.dests[0], for_write=True)
        source_location = self.find_constant_location(instruction.sources[0])
        processor_instruction = self.target.load(dest_register, source_location)
        comment = f"{instruction.dests[0]} <= {instruction.sources[0]}"
        self.code.add_instruction(processor_instruction, comment)
        self.output_dbg_tap(instruction, dest_register)

    def emit_arithmetic(self, instruction: Instruction):
        source_registers = [self.find_register(source, for_write=False) for source in instruction.sources]
        self.free_eol_registers(self, instruction.sources)
        dest_register = self.find_register(instruction.dests[0], for_write=True)
        opcode = self.target.opcode(instruction.opcode, instruction.dest.type)
        processor_instruction = ProcessorInstruction(opcode, dest_register, source_registers)
        self.code.add_instruction(processor_instruction, f"{instruction.dests[0]} <= {instruction.opcode} {', '.join([str(src) for src in instruction.sources])}")
        self.output_dbg_tap(instruction, dest_register)

    #----------------------------------------------------------------------------------------------

    def find_register(self, var: Var, for_write: bool) -> LogicalRegister:
        if var.register: return var.register
        register_set = self.target.register_manager.register_sets[var.type[0]]
        register = next((reg for reg in register_set.registers if reg.contents is None), None) # first register whose contents are None
        if register is None: register = self.spill_register(register_set)
        return self.assign_register(var, register)

    def free_eol_registers(self, instruction: Instruction):
        for src in instruction.sources:
            if not isinstance(src, Var): continue
            i_last_read = self.block.find_last_read(src, self.i_instruction)
            if i_last_read == self.i_instruction:
                self.free_register(src)

    def assign_register(self, var: Var, register: PhysicalRegister) -> LogicalRegister:
        register.contents = var
        register.n_bits = int(var.type[1:])
        logical_register = LogicalRegister(register, int(var.type[1:]))
        var.register = logical_register
        return logical_register

    def free_register(self, var: Var):
        if var.register:
            physical_register = var.register.physical_register
            physical_register.contents = None
            var.register = None

    #----------------------------------------------------------------------------------------------

    def find_constant_location(self, constant: Const) -> MemoryLocation:
        if self.data.locations.get(constant.value): return self.data.locations[constant.value]
        loc = MemoryLocation(constant.value, constant.type, int(constant.type[1:])//8)
        self.data.add_location(loc)
        return loc
    
    #----------------------------------------------------------------------------------------------

    def output_dbg_tap(self, instruction: Instruction, dest_register: LogicalRegister):
        pass

    def output_binary(self, output: ProcessorInstructionBlock) -> bytes:
        pass

    def output_binary_data(self, data: MemoryBlock) -> bytes:
        pass

    def join(self, blocks: List[ProcessorInstructionBlock]) -> ProcessorInstructionBlock:
        pass

        
        

    def run(self) -> str:
        pass

#--------------------------------------------------------------------------------------------------
# ARM

class ARMProcessor(Processor):
    def __init__(self):
        self.int_registers = RegisterSet("i", 32)
        self.fp_registers = RegisterSet("f", 32)
        self.register_manager = RegisterManager({ "i" : self.int_registers, "f" : self.fp_registers })
        self.int_opcodes = { "add" : "add", "sub" : "sub", "mul" : "mul", "div" : "sdiv", "sqrt" : "sqrts" }
        self.fp_opcodes = { "add" : "fadd", "sub" : "fsub", "mul" : "fmul", "div" : "fdiv", "sqrt" : "fsqrt" }

    def adr(self, dest: LogicalRegister, relative_address: int) -> ProcessorInstruction: 
        return ProcessorInstruction("adr", dest, [relative_address])
    
    def load(self, dest: LogicalRegister, source: MemoryLocation) -> ProcessorInstruction: 
        return ProcessorInstruction("ldr", dest, [source.base_register(), source.offset])
    
    #----------------------------------------------------------------------------------------------
    # register naming conventions: x0-31 are i64, w0-31 are i32; d0-31 are f64, s0-31 are f32

    def opcode(self, vm_opcode: str, type: str) -> str:
        type = type[0] # "i" or "f"
        if type == "i" and vm_opcode in self.int_opcodes: return self.int_opcodes[vm_opcode]
        elif type == "f" and vm_opcode in self.fp_opcodes: return self.fp_opcodes[vm_opcode]
        else: raise Exception(f"unknown opcode {vm_opcode} for type {type}")

    def register_name(self, register: LogicalRegister) -> str:
        if register.n_bits == 64 and register.index == 31 and register.physical_register.in_set.type == "i": return "sp"
        if register.physical_register.in_set.type == "i":
            return f"x{register.index}" if register.n_bits == 64 else f"w{register.index}"
        else:
            return f"d{register.index}" if register.n_bits == 64 else f"s{register.index}"
        
    def get_register(self, name: str) -> LogicalRegister:
        if name == "sp": return LogicalRegister(self.int_registers.registers[31], 64)
        prefix = name[0]
        index = int(name[1:])
        size = 64 if prefix in "xd" else 32
        register_set = self.int_registers if prefix in "xw" else self.fp_registers
        register = register_set.registers[index]
        return LogicalRegister(register, size)
        


#--------------------------------------------------------------------------------------------------
# RISC-V

class RiscVProcessor(Processor):
    def __init__(self):
        self.int_registers = RegisterSet("i", 32)
        self.fp_registers = RegisterSet("f", 32)
        self.register_manager = RegisterManager({ "i" : self.int_registers, "f" : self.fp_registers })
        self.int_opcodes = { "add" : "add", "sub" : "sub", "mul" : "mul", "div" : "div", "sqrt" : "sqrt" }
        self.fp_opcodes = { "add" : "fadd", "sub" : "fsub", "mul" : "fmul", "div" : "fdiv", "sqrt" : "fsqrt" }


    #----------------------------------------------------------------------------------------------
    # register naming conventions: x0-31 are i32/64, f0-31 are f32/64; opcodes encode bit-size

    def opcode(self, vm_opcode: str, type: str) -> str:
        n_bits = int(type[1:])
        type = type[0]
        opcode = ""
        if type == "i" and vm_opcode in self.int_opcodes: 
            opcode = self.int_opcodes[vm_opcode] + ".w" if n_bits == 32 else ""
        elif type == "f" and vm_opcode in self.fp_opcodes: 
            opcode = self.fp_opcodes[vm_opcode] + ".d" if n_bits == 64 else ".s"
        else: raise Exception(f"unknown opcode {vm_opcode} for type {type}")
        return f"{opcode}.{n_bits}"
    
    def register_name(self, register: LogicalRegister) -> str:
        if register.physical_register.in_set.type == "i": return f"x{register.physical_register.index}"
        else: return f"f{register.physical_register.index}"

    def get_register(self, name: str) -> LogicalRegister:
        if name[0] == "x": return LogicalRegister(self.int_registers.registers[int(name[1:])], 64)
        else: return LogicalRegister(self.fp_registers.registers[int(name[1:])], 64)

