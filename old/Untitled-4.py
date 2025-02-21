
#--------------------------------------------------------------------------------------------------
# CPUBackend

class CPUBackend(Backend):
    def __init__(self, path: str, target: 'CPU', dbg: bool = False):
        super().__init__(path)
        self.target : 'CPU' = target
        self.dbg = dbg
        self.arithmetic_ops = [ "add", "sub","mul", "div", "sqrt" ]
        self.memory_ops = ["ldr", "str", "adr"]
        self.data_var = Var("_data", "i64")

    # convert a VM instruction block to a CPU instruction block
    def generate(self, block: InstructionBlock):
        self.block = block
        self.reset()
        self.emit_prologue()
        for i, instruction in enumerate(block.instructions):
            self.i_instruction = i
            self.emit_instruction(instruction)
        self.emit_epilogue()
        self.finalise()
        log(self.show())

    #----------------------------------------------------------------------------------------------

    # set everything up before we start emitting code
    def reset(self):
        for set in self.target.register_sets.values(): set.reset()
        for instruction in self.block.instructions:
            for var in instruction.dests + instruction.sources:
                if isinstance(var, Var): 
                    var.register = None
                    var.spill_slot = None
        self.out = CPUBlock()
        self.data = DataBlock()
        self.data_var = Var("_data", "i64")
        self.defer_list : List[Tuple[int, Callable]] = []
        self.spill_managers = { "64" : SpillManager(64), "32" : SpillManager(32) }
        self.sp = self.target.sp()

    def finalise(self):
        self.data.start_address = self.out.pc()
        self.handle_deferred_emits()


    # emit instructions that must preceed the actual code
    def emit_prologue(self):
        self.defer(lambda: self.emit_address(self.data_var, self.data.start_address))                    # load data address into that register
        self.defer(lambda: self.emit("sub", self.sp, [self.sp, self.spill_size_bytes()], "allocate spill space")) # allocate spill space  

        
    # emit instructions that must follow the actual code
    def emit_epilogue(self):
        self.free_eol_registers(self.block.instructions[-1].sources)
        self.emit("add", self.sp, [self.sp, self.spill_size_bytes()], "deallocate spill space")

    #----------------------------------------------------------------------------------------------

    # emit one or more CPUInstructions for a VM instruction
    def emit_instruction(self, instruction: Instruction):
        if instruction.opcode == "const": self.emit_const(instruction)
        elif instruction.opcode in self.arithmetic_ops: return self.emit_arithmetic(instruction)
        else: raise Exception(f"unsupported vm instruction {instruction.opcode}")

    # emit instructions to load a constant into a register
    def emit_const(self, instruction: Instruction):
        type = instruction.dests[0].type
        dest_register = self.find_register(instruction.dests[0])
        self.defer(lambda:
            self.emit_load(dest_register, self.data_var.register, self.find_const_address(instruction.sources[0]), type))

    # emit arithmetic instruction
    def emit_arithmetic(self, instruction: Instruction):
        type = instruction.dests[0].type
        dest_register = self.find_register(instruction.dests[0])
        self.free_eol_registers(instruction.sources)
        src_registers = [self.find_register(src) for src in instruction.sources]
        opcode = self.find_target_opcode(instruction.opcode, type)
        self.emit(opcode, dest_register, src_registers, f"{instruction.dests[0]} = {instruction.opcode} {instruction.sources}")

    #----------------------------------------------------------------------------------------------

    # emit an instruction to load a value from memory into a register
    def emit_load(self, dest_register: 'Register', base_register: 'Register', item: 'MemoryItem', type: str):
        relative_address = (item.offset + self.data.start_address) - self.out.pc()
        self.emit(self.find_target_opcode("ldr", type), dest_register, [base_register, relative_address], f"load {type} from {base_register} + {item.offset}")

    # emit an instruction to store a value from a register to memory
    def emit_store(self, source_register: 'Register', base_register: 'Register', offset: int, type: str):
        self.emit(self.find_target_opcode("str", type), source_register, [base_register, offset], f"store {type} to {base_register} + {offset}")

    # emit an address
    def emit_address(self, dest_register: 'Register', address: int):
        relative_address = address - self.out.pc()
        self.emit(self.find_target_opcode("adr", "i64"), dest_register, [relative_address], f"{dest_register} holds address {hex(address)}")

    # build a CPUInstruction and emit it
    def emit(self, opcode: str, dest: 'Register', sources: List['Register|Const|MemoryItem'], comment: str):
        log(f"emitting {opcode} {dest} {sources} {comment}")
        self.out.emit(CPUInstruction(opcode, dest, sources, comment))

    #----------------------------------------------------------------------------------------------

    # find the register that (var) lives in; generate all spill/management code behind the scenes
    def find_register(self, var: Var) -> 'Register':
        if var.register: return var.register
        register = self.find_free_register_for(var)
        var.register = register
        register.var = var
        return register
    
    # search for a free register; if you can't find one, spill a victim and reuse their register
    def find_free_register_for(self, var: Var) -> 'Register':
        register_set = self.target.register_set(var.type)
        for register in register_set.registers:
            if register.var is None: return register
        victim = self.find_spill_victim(var)
        self.emit_spill(victim)
        if var.spill_slot is not None: self.emit_unspill(var, victim)
        return victim
    
    # free registers that are no longer needed at the end of the current instruction
    def free_eol_registers(self, sources: List[Var|Const]):
        for source in sources:
            if not isinstance(source, Var): continue
            if source in self.block.instructions[self.i_instruction].sources: continue
            last_use_index = source.live_range[1]
            if last_use_index <= self.i_instruction:
                source.register.var = None
                source.register = None
                source.spill_slot = None
                
    #----------------------------------------------------------------------------------------------
    
    # find a 'victim' variable to spill; the one that we'll need the furthest in the future
    def find_spill_victim(self, var: Var) -> 'Register':
        register_set = self.target.register_set(var.type)
        this_instruction = self.block.instructions[self.i_instruction]
        last_index = -1
        victim_index = -1
        for index, register in enumerate(register_set.registers):
            var = register.var
            if var is None: return register                 # we should never get here if there's a free register, but anyway
            if var in this_instruction.sources: continue    # don't spill a register that's being used by this instruction
            i_next_read = self.block.find_next_read(self.i_instruction, var)       # find the next instruction that reads this variable
            if i_next_read >= last_index:
                last_index = i_next_read
                victim_index = index
        victim_register = register_set.registers[victim_index]
        return victim_register

    # output a CPU instruction to spill a variable from register to memory
    def emit_spill(self, register: 'Register'):
        var = register.var
        spill_manager = self.spill_managers[var.type[1:]]
        slot = spill_manager.find_slot()
        self.emit_store(register, self.data_var.register,(slot * spill_manager.n_bytes_per_slot))
        var.spill_slot = slot
        var.register = None
        register.var = None
        
    # output a CPU instruction to unspill a variable from memory to register
    def emit_unspill(self, var: Var, dest_register: 'Register'):
        slot = var.spill_slot
        self.emit_load(dest_register, self.data_var.register, slot)
        self.spill_slots[slot] = None
        var.spill_slot = None

    # sum of 64-bit and 32-bit spill slots, in bytes, rounded up to nearest multiple of 16
    def spill_size_bytes(self) -> int:
        total = self.spill_managers["64"].size_bytes() + self.spill_managers["32"].size_bytes()
        return ((total + 15) // 16)*16
    
    #----------------------------------------------------------------------------------------------

    # get a memory item for a constant
    def find_const_address(self, const: Const) -> 'MemoryItem':
        key = f"const_{const.value}"
        if key in self.data.labels: return self.data.labels[key]
        n_bytes = int(const.type[1:])//8
        item = MemoryItem(key, n_bytes, self.encode_const(const))
        self.data.emit(item)
        return item
    
    # encode a constant into a bytes object
    def encode_const(self, const: Const) -> bytes:
        if const.type == "i64": return struct.pack("q", int(const.value))
        elif const.type == "i32": return struct.pack("i", int(const.value))
        elif const.type == "u64": return struct.pack("Q", int(const.value))
        elif const.type == "u32": return struct.pack("I", int(const.value))
        elif const.type == "f64": return struct.pack("d", float(const.value))
        elif const.type == "f32": return struct.pack("f", float(const.value))
        else: raise Exception(f"unknown constant type {const.type}")

    # translates a VM opcode to the opcode for the target CPU
    def find_target_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_ops: return self.target.arithmetic_op(opcode, type)
        elif opcode in self.memory_ops: return self.target.memory_op(opcode, type)
        else: raise Exception(f"unknown opcode {opcode}")

    #----------------------------------------------------------------------------------------------

    # defer building a CPUInstruction until later (post epilogue)
    def defer(self, func: Callable):
        i_instruction = self.out.pc() // 4
        self.defer_list.append((i_instruction, func))
        self.out.emit(CPUInstruction("nop", None, [], "awaiting deferred emit"))

    # emit all deferred CPUInstructions
    def handle_deferred_emits(self):
        log(f"handling {len(self.defer_list)} deferred emits")
        for i_instruction, func in self.defer_list:
            self.out.insert_index = i_instruction
            func()
        self.out.insert_index = None

    #----------------------------------------------------------------------------------------------
    # debug readout

    def show(self) -> str:
        out = ""
        for i, instruction in enumerate(self.out.instructions):
            out += f"{i:3}: {instruction.opcode} {instruction.dest}, {", ".join([str(source) for source in instruction.sources])}\n"
        return out

#--------------------------------------------------------------------------------------------------

# CPU is base class for all CPUs
class CPU:
    def __init__(self): pass
    def sp(self) -> 'Register': pass                                # get the register to use as the stack pointer
    def register_set(self, type: str) -> 'RegisterSet': pass        # get the register set for a type ("i64", "f32", ..)
    def arithmetic_op(self, opcode: str, type: str) -> str: pass    # add, sub, mul, div, sqrt
    def memory_op(self, opcode: str, type: str) -> str: pass        # ldr, str, adr

# Register is a CPU register, possibly holding a Var
class Register:
    def __init__(self, register_set: 'RegisterSet', index: int):
        self.register_set = register_set
        self.index = index
        self.var = None
    def __str__(self): return f"{self.register_set.prefix(self.var)}{self.index}"
    def __repr__(self): return str(self)

# RegisterSet is a physical bank of registers
class RegisterSet:
    def __init__(self, n_registers: int, prefixes: Dict[str, str]):
        self.n_registers = n_registers                                      # number of registers
        self.registers = [Register(self, i) for i in range(n_registers)]    # list of registers
        self.prefixes = prefixes                                            # type => prefix
    def prefix(self, var: Var|None) -> str:
        if var is None: return next(iter(self.prefixes.values()))
        else: return self.prefixes[var.type]
    def reset(self):
        for register in self.registers: register.var = None

# SpillManager manages spill slots for a given size
class SpillManager:
    def __init__(self, n_bits: int):
        self.n_bytes_per_slot = n_bits//8
        self.slots : List[Var] = []
    def find_slot(self) -> int:
        for i, slot in enumerate(self.slots):
            if self.slots[i] is None: return i
        self.slots.append(None)
        return len(self.slots) - 1
    def free_slot(self, slot: int):
        self.slots[slot] = None
    def size_bytes(self) -> int: return len(self.slots) * self.n_bytes_per_slot

# MemoryItem notes that we're storing a variable or constant in memory
class MemoryItem:
    def __init__(self, name: str, size_bytes: int, value: bytes):
        self.name = name
        self.offset = 0
        self.size_bytes = size_bytes
        self.value = value

# A CPUInstruction represents a single machine instruction
class CPUInstruction:
    def __init__(self, opcode: str, dest: 'Register', sources: List['Register|int|MemoryItem'], comment: str):
        self.opcode = opcode
        self.dest = dest
        self.sources = sources
        self.comment = comment

# CPUBlock is a list of CPUInstructions, and labels
class CPUBlock:
    def __init__(self):
        self.instructions = []
        self.labels = {}
        self.insert_index = None
    def emit(self, instruction: CPUInstruction):
        if self.insert_index is None:
            self.instructions.append(instruction)
        else:
            self.instructions[self.insert_index] = instruction
    def pc(self) -> int:
        return len(self.instructions) * 4       # assuming 32-bit instructions for the moment
    def emit_label(self, name: str):
        self.labels[name] = self.pc()

# DataBlock is a list of MemoryItems arranged in a contiguous block of memory
class DataBlock:
    def __init__(self):
        self.items = []
        self.labels = {}
        self.size_bytes = 0
        self.start_address = 0
    def emit(self, item: MemoryItem) -> int:
        offset = self.size_bytes
        self.items.append(item)
        self.labels[item.name] = item
        self.size_bytes += item.size_bytes
        item.offset = offset
        return offset
    
#--------------------------------------------------------------------------------------------------

# ARM is the ARM CPU
class ARM(CPU):
    def __init__(self):
        self.int_registers = RegisterSet(32, { "i64" : "x", "u64" : "x", "i32" : "w", "u32" : "w" })
        self.float_registers = RegisterSet(32, { "f64" : "d", "f32" : "s" })
        self.register_sets = { "i" : self.int_registers, "u" : self.int_registers, "f" : self.float_registers }
    def sp(self) -> 'Register': return self.int_registers.registers[31]
    def register_set(self, type: str) -> RegisterSet: return self.register_sets[type[0]]
    # on ARM, the opcode stays the same for all sizes, just changes for float/int
    def arithmetic_op(self, vm_opcode: str, type: str) -> str:
        if "f" in type: return f"f{vm_opcode}"
        else: return f"w{vm_opcode}"
    # on ARM, the memory opcode stays the same for all types/sizes
    def memory_op(self, vm_opcode: str, type: str) -> str:
        return vm_opcode

#--------------------------------------------------------------------------------------------------

# RISCV is the RISC-V CPU
class RISCV(CPU):
    def __init__(self):
        self.int_registers = RegisterSet(32, { "i64" : "x", "u64" : "x", "i32" : "x", "u32" : "x" })
        self.float_registers = RegisterSet("f", { "f64" : "f", "f32" : "f" })
        self.register_sets = { "i" : self.int_registers, "u" : self.int_registers, "f" : self.float_registers }
        self.width_map = { "64" : "", "32" : "w", "16" : "h", "8" : "b" }
    def sp(self) -> 'Register': return self.int_registers.registers[2]
    def register_set(self, type: str) -> 'RegisterSet': return self.register_sets[type[0]]
    # on RISC-V, the opcode changes depending on the type 
    def arithmetic_op(self, vm_opcode: str, type: str) -> str:
        if type[0] in "iu":
            return f"{vm_opcode}w" if "32" in type else vm_opcode
        elif type[0] == "f":
            return f"f{vm_opcode}.s" if "32" in type else f"f{vm_opcode}.d"
        else: raise Exception(f"unknown arithmetic operation {vm_opcode} for type {type}")
    # on RISC-V, the memory operation changes depending on the type
    def memory_op(self, vm_opcode: str, type: str) -> str:
        if type[0] in "iu":
            out = "l" if vm_opcode == "ldr" else "s"
            out += self.width_map[type[1:]]
            if not ("64" in type) and type[0] == "u": out += "u"
            return out
        elif type[0] == "f":
            out = "fl" if vm_opcode == "ldr" else "fs"
            out += "w" if "32" in type else "d"
            return out
        raise Exception(f"unknown memory operation {vm_opcode} for type {type}")


