# Processor is base class for all architectures

class Processor:
    def __init__(self): pass
    def setup(self, block: InstructionBlock, data: DataBlock): override_me()
    def prologue(self) -> List[Instruction]: override_me()
    def epilogue(self) -> List[Instruction]: override_me()
    def instruction(self, instruction: Instruction, i_instruction: int) -> List[Instruction]: override_me()

class RegisterProcessor(Processor):
    def __init__(self):
        super().__init__()
        self.max_fp_registers = None
    def init_register_sets(self) -> Dict[str, RegisterSet]: override_me()
    def register_name(self, register_name: str, type: str) -> str: override_me()
    def emit(self, instruction: Instruction) -> List[Instruction]: override_me()
    def spill(self, register: Register, slot: int, comment: str = "") -> List[Instruction]: override_me()
    def unspill(self, register: Register, slot: int, comment: str = "") -> List[Instruction]: override_me()

    #----------------------------------------------------------------------------------------------
    # setup : call before you start emitting instructions

    def setup(self, block: InstructionBlock, data: DataBlock):
        self.block = block
        self.data : DataBlock= data
        self.register_sets = self.init_register_sets()
        self.spill_slots = []
        if self.max_fp_registers != None:
            self.restrict_free_fp_registers(self.max_fp_registers)

    def restrict_free_fp_registers(self, max_fp_registers: int):
        if max_fp_registers > 0:
            dummy_var = Var("dummy", "f32")
            register_set = self.register_sets["f"]
            for i in range(0, register_set.n_registers):
                if i >= max_fp_registers:
                    register_set.registers[i].contents = dummy_var

    #----------------------------------------------------------------------------------------------
    # emit instruction

    def instruction(self, instruction: Instruction, i_instruction: int) -> List[Instruction]:
        self.out : List[Instruction] = []
        self.i_instruction = i_instruction
        if len(instruction.dests) != 1: raise Exception(f"expected 1 destination, got {len(instruction.dests)}")
        source_registers = [self.assign_register(source) for source in instruction.sources]
        self.free_eol_registers(instruction)
        instruction.comment = instruction.show()
        for i, dest in enumerate(instruction.dests):
            instruction.dests[i] = self.assign_register(dest)
        for i in range(len(instruction.sources)):
            if source_registers[i] is not None:
                instruction.sources[i] = source_registers[i]
        return self.out + self.emit(instruction)
            
    #----------------------------------------------------------------------------------------------
    # register allocation and spill

    def free_eol_registers(self, instruction: Instruction):
        for source in instruction.sources:
            if isinstance(source, Var) and source.register is not None:
                if source.live_range[1] <= self.i_instruction:
                    source.register.contents = None
                    source.register = None

    def assign_register(self, var: Var) -> Register:
        if not isinstance(var, Var): return None
        if var.register is not None: return var.register
        register_set = self.register_sets[var.type[0]]
        register = next((register for register in register_set.registers if register.contents is None), None)
        if register is None: register = self.spill_register(register_set,var.type)
        register.name = self.register_name(register, var.type)
        if isinstance(register.contents, Var): register.contents.register = None
        if var.spill_slot is not None: self.unspill_register(var, register)
        self.set_register(var, register)
        return register
    
    def set_register(self, var: Var, register: Register):
        var.register = register
        register.contents = var
    
    def spill_register(self, register_set: RegisterSet, type: str) -> Register:
        victim_var = self.find_spill_victim(register_set, type)
        slot = self.find_spill_slot(type)   
        victim_var.spill_slot = slot
        self.spill_slots[slot] = victim_var
        self.out += self.spill(victim_var.register, slot, f"spill {victim_var} to slot {slot}")
        return victim_var.register
    
    def unspill_register(self, var: Var, register: Register):
        slot = var.spill_slot
        self.out += self.unspill(register, slot, f"unspill {var} from slot {slot}")
        var.spill_slot = None
        self.spill_slots[slot] = None
    
    def find_spill_victim(self, register_set: RegisterSet, type: str) -> Var:
        this_instruction = self.block.instructions[self.i_instruction]
        i_best_read = self.i_instruction
        best_victim = None
        for register in register_set.registers:
            var = register.contents
            if var is None: return register                 # if register is free, use it
            if not isinstance(var, Var): continue           # if register is not a variable, you can't spill it
            if var.register != register: continue            # if register is not the same as the one we're spilling, you can't spill it
            if var in this_instruction.sources: continue    # if variable is used by this instruction, you can't spill it
            i_last_read = self.block.find_last_read(var)    # next time we need this instruction
            if best_victim is None or i_last_read > i_best_read:
                best_victim = var
                i_best_read = i_last_read
        return best_victim

    def find_spill_slot(self, type: str) -> int:
        i_slot = next((i for i, slot in enumerate(self.spill_slots) if slot is None), None)
        if i_slot is not None: return i_slot
        self.spill_slots.append(None)
        return len(self.spill_slots) - 1

#--------------------------------------------------------------------------------------------------
# just a couple of register-set-based processors, nothing special

class RISCV(RegisterProcessor):
    def init_register_sets(self): return { "i" : RegisterSet("i", 32, "x"), "f" : RegisterSet("f", 32, "f") }
    def prologue(self) -> List[Instruction]: return []
    def epilogue(self) -> List[Instruction]: return []
    def emit(self, instruction: Instruction): return []
    def spill(self, register: Register, slot: int): return []
    def unspill(self, register: Register, slot: int): return []

class ARM(RegisterProcessor):
    def __init__(self):
        super().__init__()
        self.register_prefixes = { "i32" : "w", "i64" : "x", "f32" : "s", "f64" : "d" }
        self.arithmetic_ops = [ "add", "sub", "mul", "div", "sqrt"]
        self.data = Var("data", "i64")
        self.stack = Var("stack", "i64")
    def init_register_sets(self): 
        return { "i" : RegisterSet("i", 32, "x"), "f" : RegisterSet("f", 32, "d") }
    def register_name(self, register: Register, type: str) -> str:
        return f"{self.register_prefixes[type]}{register.index}"
    
    def spill(self, register: Register, slot: int, comment: str = ""): 
        return [Instruction("str", [register], [self.stack_register, f"{slot*4}"], comment=comment)]
    def unspill(self, register: Register, slot: int, comment: str = ""): 
        return [Instruction("ldr", [register], [self.stack_register, f"{slot*4}"], comment=comment)]
    
    def prologue(self) -> List[Instruction]:
        self.stack_register = self.register_sets["i"].registers[31]
        self.stack_register.name = "sp"
        self.set_register(self.stack, self.stack_register)
        self.data_register = self.register_sets["i"].registers[0]
        self.set_register(self.data, self.data_register)
        return [ Instruction("adr", [self.data_register], ["data"], "load data start address"),
            Instruction("sub", [self.stack_register], [self.stack_register, "spill_size"], "allocate stack space")]

    def emit(self, instruction: Instruction):
        if instruction.opcode == "const":
            type = instruction.dests[0].contents.type
            value = instruction.sources[0].value
            name = f"const_{type}_{value}"
            n_bytes = int(type[1:])//8
            packed_value = self.pack_value(type, value)
            const_adr = self.data.allocate(name, type, n_bytes, packed_value)
            return [Instruction("ldr", [instruction.dests[0]], [self.data_register, const_adr.offset_bytes], f"load constant {type} {value}")]
        else:
            instruction.opcode = self.map_opcode(instruction.opcode, instruction.dests[0].type)
            return [instruction]
    
    def epilogue(self) -> List[Instruction]:
        return [Instruction("add", [self.stack_register], [self.stack_register, "spill_size"], "deallocate stack space")]

    #----------------------------------------------------------------------------------------------

    def map_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_ops: return opcode if type.startswith("i") else f"f{opcode}"
        elif opcode == "const": return "ldr"
        raise Exception(f"unknown opcode: {opcode}")
    
    def pack_value(self, type: str, value: str) -> bytes:
        if type == "f32": return struct.pack(">f", float(value))
        elif type == "f64": return struct.pack(">d", float(value))
        elif type == "i32": return struct.pack(">i", int(value))
        elif type == "i64": return struct.pack(">q", int(value))
        raise Exception(f"unsupported type: {type}")

#--------------------------------------------------------------------------------------------------
# stubs for now: but you see how this can work

class StackProcessor(Processor):
    pass

class WebAsm(StackProcessor):
    pass

#--------------------------------------------------------------------------------------------------

class CPUBackend(Backend):
    def __init__(self, path: str, processor: Processor, dbg: bool):
        self.path = path
        self.processor = processor
        self.dbg = dbg

    # output assembly code for the given VM code
    def generate(self, block: InstructionBlock):
        self.reset(block)
        self.out.instructions += self.processor.prologue()
        for i, instruction in enumerate(block.instructions):
            self.out.instructions += self.processor.instruction(instruction, i)
        self.out.instructions += self.processor.epilogue()
        self.show()
        self.resolve_addresses()
        self.output_assembly()
        self.assemble()
        self.run()

    #----------------------------------------------------------------------------------------------
    
    def reset(self, block: InstructionBlock):
        self.out = InstructionBlock()
        self.data = DataBlock("data")
        self.processor.setup(block, self.data)

    def resolve_addresses(self):
        pass

    def output_assembly(self):
        pass

    def assemble(self):
        pass

    def run(self):
        pass

    def show(self):
        for instruction in self.out.instructions:
            log(f"{instruction}")
        



