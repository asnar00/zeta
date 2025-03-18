# ᕦ(ツ)ᕤ
# compiler.py
# author: asnaroo
# zero to anything

from vm import *
from isa import *
from qemu import *

#--------------------------------------------------------------------------------------------------
# Backend generates actual code for a platform

class Backend:
    def __init__(self, path: str): 
        self.path = path
        os.makedirs(os.path.dirname(path), exist_ok=True)
    def generate(self, block: VMBlock) -> str: override_me()
    def run(self) -> str: return ""

#--------------------------------------------------------------------------------------------------
# Python backend runs numpy

class PythonBackend(Backend):
    def generate(self, block: VMBlock):
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
    def __init__(self, path: str, isa: 'ISA', debug: bool, restrict: bool):
        super().__init__(path)
        self.isa = isa
        self.debug = debug
        self.restrict = restrict
        self.rm = self.isa.init_register_manager()

    def generate(self, vm_block: VMBlock):
        log(self.isa.__class__.__name__)
        log("------------------------------------------------------")
        self.vm_block = vm_block
        self.initialise()

        self.generate_start()
        self.generate_prologue()
        self.generate_instructions()
        self.generate_epilogue()
        self.generate_end()

        self.arrange_memory()
        self.handle_deferred()

        self.finalise()

    def run(self) -> str:
        if self.isa.__class__.__name__ != "RISCV32":
            return ""
        # start QEMU
        log("running " + self.path.replace(".*", ".elf"))
        log_flush()
        if not run_qemu(self.path.replace(".*", ".elf")):
            log(log_red("qemu_run failed"))
            log_exit()
        else:
            log(log_green("qemu_run passed"))
        log_flush()
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
        if self.restrict: self.fill_registers()             # artificially fill registers to test spill

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
        self.check_disassembly()
        write_elf(self.isa.__class__.__name__, self.path.replace(".*", ".elf"), self.elf_code, self.rodata_bytes, self.rwdata_size_bytes)
        log("wrote " + self.path.replace(".*", ".elf"))
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
        self.instructions(self.isa.shutdown(), "shutdown")
        pass

    #----------------------------------------------------------------------------------------------

    def generate_test_instruction(self):
        self.emit_load(Register("x8", "i32"), 0, "test load")

    def generate_instruction(self, vm_instr: VMInstruction):
        if vm_instr.opcode == "const": return self.generate_const(vm_instr)
        source_registers = [(self.get_register(s) if isinstance(s, VMVar) else s) for s in vm_instr.sources]
        self.free_eol_registers(source_registers)
        dest_register = self.get_register(vm_instr.dest)
        opcode = self.isa.get_opcode(vm_instr.opcode, vm_instr.dest.type)
        self.instruction(Instruction(opcode, dest_register, source_registers), vm_instr.show())
        if self.debug: self.emit_debug(vm_instr.dest, dest_register)

    def generate_const(self, vm_instr: VMInstruction):
        register = self.get_register(vm_instr.dest)
        type = vm_instr.dest.type
        if type[0] == "f": # more efficient to load from read-only memory
            value_str = vm_instr.sources[0].value
            value_float = float(value_str)
            value_bytes = struct.pack("<f", value_float)
            constant = self.constant(value_str, value_bytes, type)
            self.defer(lambda: self.emit_load(register, constant.offset, f"load {type} {value_float}"))
        else: # more efficient to construct immediate directly in register
            self.instructions(self.isa.load_int_immediate(register, int(vm_instr.sources[0].value)), f"load int {vm_instr.sources[0].value}")
    
    def size_of(self, type: str) -> int:
        return int(type[1:]) // 8

    #----------------------------------------------------------------------------------------------

    def emit_rel_address(self, register: Register, address: int, comment: str):
        self.instructions(self.isa.load_rel_address(register, address, self.pc*4), comment)

    def emit_load(self, register: Register, offset: int, comment: str):
        self.instructions(self.isa.load(register, self.data_register, offset), comment)

    def emit_store(self, register: Register, data_register: Register, offset: int, comment: str):
        self.instructions(self.isa.store(register, data_register, offset), comment)

    def emit_stack_alloc(self):
        pass

    def emit_stack_dealloc(self):
        pass

    def emit_spill(self, var: VMVar,register: Register, offset: int):
        type = var.type
        self.instructions(self.isa.store(register, self.stack_register, offset*4), f"spill {var}")

    def emit_unspill(self, var: VMVar, register: Register, offset: int):
        type = var.type
        self.instructions(self.isa.load(register, self.stack_register, offset*4), f"unspill {var}")

    #----------------------------------------------------------------------------------------------
    # business end

    def instructions(self, instructions: List[Instruction], comment: str):
        for i, instr in enumerate(instructions): 
            self.instruction(instr, comment)

    def instruction(self, instr: Instruction, comment: str):
        instr.comment = comment
        if self.pc == len(self.code):
            self.code.append(deepcopy(instr))
        else: 
            self.code[self.pc] = deepcopy(instr)
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
    # debug

    def emit_debug(self, var: VMVar, register: Register):
        variable = self.variable(var.name, var.type)
        self.defer(lambda: self.emit_store(register, self.data_register, variable.offset, f"debug store {register.var} to {variable.offset}"))

    #----------------------------------------------------------------------------------------------
    # arrange constant and variable memory

    def arrange_memory(self):
        code_size = len(self.code) * 4
        self.data_address = self.align(code_size, 4)

        # sort constants by size, largest first
        sorted_constants = sorted(self.constants.values(), key=lambda c: len(c.value), reverse=True)
        offset = 0
        for c in sorted_constants:
            offset = self.align(offset, 4)
            c.offset = offset
            size_bytes = len(c.value)
            offset += size_bytes

        # create read-only data segment
        offset = self.align(offset, 4)
        self.rodata_bytes = bytearray(offset)
        for i in range(len(self.rodata_bytes)): self.rodata_bytes[i] = 0
        for c in sorted_constants:
            for i in range(len(c.value)):
                self.rodata_bytes[c.offset + i] = c.value[i]


        # sort variables by size, largest first
        start_offset = offset
        sorted_variables = sorted(self.variables.values(), key=lambda v: v.count, reverse=True)
        for v in sorted_variables:
            offset = self.align(offset, 4)
            v.offset = offset
            size_bytes = v.count * self.size_of(v.type)
            offset += size_bytes
        self.rwdata_size_bytes = offset - start_offset

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

    # note that register stores variable
    def assign_register(self, register: Register, var: VMVar):
        register.name = self.isa.register_prefix(var.type) + register.defined_name[1:]
        register.type = var.type
        register.var = var
        var.register = register

    # return the register assigned to a variable; handles allocation and spills 
    def get_register(self, var: VMVar) -> Register:
        if var.register is not None:  return var.register
        register_set = self.register_manager.int_registers if var.type[0] in "iu" else self.register_manager.fp_registers
        register = next((r for r in register_set.registers if r.var is None), None)
        if register is None: register = self.spill(register_set, var)
        self.assign_register(register, var)
        if var.spill_slot is not None: self.unspill(var)
        return register
    
    # free any registers whose variables won't be needed again
    def free_eol_registers(self, registers: List[Register|None]):
        for register in registers:
            if register is None: continue
            var = register.var
            if var is None: continue
            if var.live_range[1] <= self.i_instruction:
                self.free_register(register)

    # free a specific register
    def free_register(self, register: Register):
        register.var.register = None
        register.var = None

    # find a victim var to spill, spill it, and unspill for_var; return the register
    def spill(self, register_set: RegisterSet, for_var: VMVar) -> Register:
        victim_var = self.find_spill_victim(register_set, for_var.type)
        spill_slot = self.find_spill_slot(victim_var)
        self.spill_slots[spill_slot] = victim_var
        victim_var.spill_slot = spill_slot
        register = victim_var.register
        self.free_register(register)
        self.emit_spill(victim_var,register, spill_slot)
        return register

    # find the variable with the latest next use
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
    
    # find a vacant spill slot, or add a new one
    def find_spill_slot(self, var: VMVar) -> int:
        for i, slot in enumerate(self.spill_slots):
            if slot == None: return i
        self.spill_slots.append(None)
        return len(self.spill_slots) - 1
    
    def unspill(self, var: VMVar):
        self.emit_unspill(var, var.register, var.spill_slot)
        self.spill_slots[var.spill_slot] = None
        var.spill_slot = None
    
    # to test spill, artificially fill registers
    def fill_registers(self):
        fp_registers = self.register_manager.fp_registers
        dummy_var = VMVar("_dummy", "f32")
        for i in range(2, 32):
            self.assign_register(fp_registers.registers[i], dummy_var)
        dummy_var.register = None # prevent reassignment

    
    #----------------------------------------------------------------------------------------------
    # show code

    def show_code(self):
        for i, instr in enumerate(self.code):
            encoded = self.isa.encode_instruction(instr, i)
            out = f"{(i*4):03x}: {encoded:08x} {instr}"
            out += (" " * (35-len(out))) + log_grey("# " + instr.comment)
            log(out)

    #----------------------------------------------------------------------------------------------
    # binary / elf stuff

    def encode_all(self):
        encoded_code : bytes = b""
        for i, instr in enumerate(self.code):
            encoded_code += self.isa.encode_instruction(instr, i).to_bytes(4, "little")
        self.elf_code = encoded_code

    def check_disassembly(self):
        log("----------------------------------------------")
        bin_path = self.path.replace(".*", ".bin")
        # Write the encoded binary code to the bin file
        with open(bin_path, "wb") as f:
            f.write(self.elf_code)
        # gobjdump -D -b binary -m riscv:rv32 -M no-aliases,numeric --start-address=0x0 test.bin > test.asm
        asm_path = self.path.replace(".*", ".asm")
        arch = self.isa.gobjdump_arch_name()
        process = subprocess.run(["gobjdump", "-D", "-b", "binary", "-m", arch, "-M", "no-aliases,numeric", "--start-address=0x0", bin_path], capture_output=True, text=True)
        with open(asm_path, "w") as f:
            f.write(process.stdout)
        lines = read_file(asm_path).split("\n")
        lines = lines[next(i for i, line in enumerate(lines) if line.startswith("   0:")):]
        out = ""
        for i, line in enumerate(lines):
            parts = line.split("\t")
            if len(parts) < 3: continue
            opcode = parts[2].strip()
            operands = parts[3].strip() if len(parts) > 3 else ""
            operands = re.sub(r" # .*", "", operands).strip()
            operands = operands.replace(",rne", "")
            operands = operands.replace(",", ", ").replace("  ", " ")
            check = f"{opcode} {operands}".strip()
            ours = self.isa.show_instruction(self.code[i], i*4).strip() if i < len(self.code) else ""
            ours = re.sub(f"# .*", "", ours).strip()
            if check != ours:
                out += f"{i*4:03x}: {check} <=> {ours}" + "\n"
        if out != "":
            log(log_red("disassembly check failed:\n") + out)
            log_exit()
        else:
            log(log_green("disassembly check passed"))
