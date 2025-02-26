# ᕦ(ツ)ᕤ
# compiler.py
# author: asnaroo
# zero to anything

from vm import *
from isa import *

#--------------------------------------------------------------------------------------------------
# Backend generates actual code for a platform

class Backend:
    def __init__(self, path: str): self.path = path
    def generate(self, block: VMInstructionBlock) -> str: override_me()
    def run(self) -> str: return ""

#--------------------------------------------------------------------------------------------------
# Python backend runs numpy

class PythonBackend(Backend):
    def generate(self, block: VMInstructionBlock):
        out = self.header()
        op_map = { "add" : "+", "sub" : "-", "mul" : "*", "div" : "/", "sqrt" : "np.sqrt" }
        type_map = { "f32" : "np.float32", "i32" : "np.int32" }

        for instruction in block.instructions:
            dest = instruction.dest.name.replace(".", "_")
            if instruction.opcode == "const":
                type = type_map[instruction.dest.type]
                out += f"    {dest} = {type}({instruction.sources[0]})\n"
            else:
                opcode = op_map[instruction.opcode]
                operands = [var.name.replace(".", "_") for var in instruction.sources]
                if opcode in "+-*/":
                    out += f"    {dest} = {operands[0]} {opcode} {operands[1]}\n"
                elif len(operands) == 1:
                    out += f"    {dest} = {opcode}({operands[0]})\n"

        for instruction in block.instructions:
            dest = instruction.dest.name.replace(".", "_")
            out += f"    print(f\"{instruction.dest} = {{{dest}}}\")\n"
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
# CPUBackend generates code for register-based CPUs

class CPUBackend(Backend):
    def __init__(self, path: str, isa: 'ISA', dbg: bool):
        self.path = path
        self.isa = isa
        self.dbg = dbg
        self.rm = self.isa.init_register_manager()

    def generate(self, vm_block: VMInstructionBlock):
        log(self.isa.__class__.__name__)
        log("------------------------------------------------------")
        self.vm_block = vm_block
        self.initialise()
        self.generate_start()
        self.generate_prologue()
        self.generate_instructions()

        self.arrange_memory()
        self.handle_deferred()

        self.generate_epilogue()
        self.generate_end()
        self.finalise()

    def run(self) -> str:
        # start QEMU
        # grab the output and memory dump
        # check debug values
        pass

    #----------------------------------------------------------------------------------------------

    def initialise(self):
        self.register_manager = self.isa.init_register_manager()
        self.code : List[Instruction] = []                  # generated code
        self.labels : Dict[str, int] = {}                   # label to instruction index 
        self.constants : Dict[str, Constant] = {}           # constant name to constant
        self.variables : Dict[str, Variable] = {}           # variable name to variable
        self.deferred : Dict[int, Callable] = {}            # deferred call to instruction index
        self.pc = 0                                         # current instruction index
        self.data_address = 0                               # will get updated later
        self.stack_address = 0                              # will get updated later
        self.out : List[Instruction] = []                   # output code
        self.spill_slots : List[VMVar] = []                 # spill slots  
        self.reset_variables()

    def reset_variables(self):
        for vm_instr in self.vm_block.instructions:
            self.reset_variable(vm_instr.dest)
            for source in vm_instr.sources:
                if isinstance(source, VMVar): self.reset_variable(source)

    def reset_variable(self, var: VMVar):
        var.register = None

    def finalise(self):
        self.show_code()
        self.encode_all()
        self.write_elf()
        self.check_disassembly()
        pass

    def generate_start(self):
        self.data_register = self.fixed_pointer("_data", self.isa.data_register(self.register_manager))
        self.stack_register = self.fixed_pointer("_stack", self.isa.stack_register(self.register_manager))
        self.defer(lambda: self.emit_rel_address(self.data_register, self.data_address, "load data pointer"))
        self.defer(lambda: self.emit_rel_address(self.stack_register, self.stack_address, "load stack pointer"))
        pass
    
    def generate_prologue(self):
        # allocate spill space on the stack
        self.defer(lambda: self.emit_stack_alloc())
        pass

    def generate_instructions(self):
        for i, vm_instr in enumerate(self.vm_block.instructions):
            self.i_instruction = i
            self.generate_instruction(vm_instr)
    
    def generate_epilogue(self):
        # deallocate spill space on the stack
        self.defer(lambda: self.emit_stack_dealloc())
        pass

    def generate_end(self):
        # generate shutdown instructions
        pass

    #----------------------------------------------------------------------------------------------

    def generate_instruction(self, vm_instr: VMInstruction):
        if vm_instr.opcode == "const": return self.generate_const(vm_instr)
        # assign a register to each source operand
        source_registers = [(self.get_register(s) if isinstance(s, VMVar) else s) for s in vm_instr.sources]
        # free any source registers that won't be used again
        self.free_eol_registers(source_registers)
        # assign a register to the destination
        dest_register = self.get_register(vm_instr.dest)
        opcode = self.isa.get_opcode(vm_instr.opcode, vm_instr.dest.type)
        comment = vm_instr.show()
        self.instruction(Instruction(opcode, dest_register, source_registers), comment)

    def generate_const(self, vm_instr: VMInstruction):
        register = self.get_register(vm_instr.dest)
        type = vm_instr.dest.type
        if type[0] == "f": # more efficient to load from read-only memory
            value_str = vm_instr.sources[0].value
            value_float = float(value_str)
            value_bytes = struct.pack("f", value_float)
            constant = self.constant(value_str, value_bytes, type)
            self.defer(lambda: self.emit_load_fp(register, constant.offset, f"load {type} {value_float}"))
        else: # more efficient to construct immediate directly in register
            self.instructions(self.isa.load_int_immediate(register, int(vm_instr.sources[0].value)), f"load int {vm_instr.sources[0].value}")
    
    def size_of(self, type: str) -> int:
        return int(type[1:]) // 8

    #----------------------------------------------------------------------------------------------

    def emit_rel_address(self, register: Register, address: int, comment: str):
        self.instructions(self.isa.load_rel_address(register, address, self.pc*4), comment)

    def emit_load_fp(self, register: Register, offset: int, comment: str):
        self.instructions(self.isa.load_float_from_memory(register, self.data_register, offset), comment)

    def emit_stack_alloc(self):
        pass

    def emit_stack_dealloc(self):
        pass

    #----------------------------------------------------------------------------------------------
    # business end

    def instructions(self, instructions: List[Instruction], comment: str):
        for i, instr in enumerate(instructions): 
            self.instruction(instr, comment)

    def instruction(self, instr: Instruction, comment: str):
        instr.comment = comment
        if self.pc == len(self.code):
            self.code.append(instr)
        else: 
            self.code[self.pc] = instr
        self.pc += 1

    def label(self, name: str):
        if name in self.labels: raise Exception(f"label {name} already exists")
        self.labels[name] = self.pc

    def constant(self, name: str, value: bytes, type: str) -> str:
        label = f"const_{name}"
        if label not in self.constants:
            self.constants[label] = Constant(value, type)
        return self.constants[label]

    def variable(self, name: str, type: str, count: int=1):
        label = f"var_{name}"
        if label not in self.variables: 
            self.variables[label] = Variable(label, type, count)
        return self.variables[label]

    #----------------------------------------------------------------------------------------------
    # arrange constant and variable memory

    def arrange_memory(self):
        code_size = len(self.code) * 4
        self.data_address = self.align(code_size, 16)

        # sort constants by size, largest first
        sorted_constants = sorted(self.constants.values(), key=lambda c: len(c.value), reverse=True)
        offset = 0
        for c in sorted_constants:
            offset = self.align(offset, 4)
            c.offset = offset
            size_bytes = len(c.value)
            offset += size_bytes

        # sort variables by size, largest first
        sorted_variables = sorted(self.variables.values(), key=lambda v: v.count, reverse=True)
        for v in sorted_variables:
            offset = self.align(offset, 4)
            v.offset = offset
            size_bytes = v.count * self.size_of(v.type)
            offset += size_bytes

        stack_start = self.align(self.data_address +offset, 16)
        stack_size = 1024       # todo: set this properly
        self.stack_address = stack_start + stack_size

    def align(self, value: int, align: int) -> int:
        return ((value + (align - 1)) // align) * align

    #----------------------------------------------------------------------------------------------
    # deferred call

    # add the deferred call tagged to current output pc, so we can revisit later; also, call now
    def defer(self, fn: Callable):      
        self.deferred[self.pc] = fn
        fn()

    # handle deferred calls
    def handle_deferred(self):
        for pc, fn in self.deferred.items():
            self.pc = pc
            fn()

    #----------------------------------------------------------------------------------------------
    # fixed registers

    # create a fixed VMVar of max lifespan, and assign it to a specific register
    def fixed_pointer(self, name: str, register: Register) -> Register:
        var = VMVar(name, self.isa.pointer_type())
        var.live_range = (0, len(self.vm_block.instructions))
        self.assign_register(register, var)
        var.register = None # this stops register being spilled or reused; todo: make this less hacky
        return register
    
    #----------------------------------------------------------------------------------------------
    # register management

    def assign_register(self, register: Register, var: VMVar):
        register.name = self.isa.register_prefix(var.type) + register.defined_name[1:]
        register.var = var
        var.register = register

    def get_register(self, var: VMVar) -> Register:
        if var.register is not None:  return var.register
        register_set = self.register_manager.int_registers if var.type[0] in "iu" else self.register_manager.fp_registers
        register = next((r for r in register_set.registers if r.var is None), None)
        if register is None: register = self.spill_register(register_set, var.type)
        self.assign_register(register, var)
        return register
    
    def free_eol_registers(self, registers: List[Register|None]):
        for register in registers:
            if register is None: continue
            var = register.var
            if var is None: continue
            if var.live_range[1] <= self.i_instruction:
                self.free_register(register)

    def free_register(sef, register: Register):
        register.var.register = None
        register.var = None

    def spill_register(self, register_set: RegisterSet, type: str) -> Register:
        victim_var = self.find_spill_victim(register_set, type)
        spill_slot = self.find_spill_slot(victim_var)
        self.spill_slots[spill_slot] = victim_var
        victim_var.spill_slot = spill_slot
        register = victim_var.register
        self.free_register(register)
        return register

    def find_spill_victim(self, register_set: RegisterSet, type: str) -> VMVar:
        this_instr = self.code[self.i_instruction]
        latest_use = -1
        victim_var = None
        for register in register_set.registers:
            var = register.var
            if var is None or var.register is None: continue
            if var == this_instr.dest or var in this_instr.sources: continue
            i_next_use = self.vm_block.find_next_read(self.i_instruction, var)
            if i_next_use is None: return var
            if i_next_use > latest_use:
                latest_use = i_next_use
                victim_var = var
        return victim_var
    
    def find_spill_slot(self, var: VMVar) -> int:
        for slot in self.spill_slots:
            if self.spill_slots[slot] == None: return slot
        self.spill_slots.append(None)
        return len(self.spill_slots) - 1
    
    #----------------------------------------------------------------------------------------------
    # show code

    def show_code(self):
        for i, instr in enumerate(self.code):
            out = f"{(i*4):03x}: {instr}"
            out += (" " * (30-len(out))) + log_grey("# " + instr.comment)
            log(out)
    #----------------------------------------------------------------------------------------------
    # binary / elf stuff

    def encode_all(self):
        pass

    def write_elf(self):
        pass

    def check_disassembly(self):
        pass




