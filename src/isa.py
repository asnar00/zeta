# ᕦ(ツ)ᕤ
# isa.py
# author: asnaroo
# zero to anything

from util import *
from typing import Dict, List

#--------------------------------------------------------------------------------------------------
# CPU backend helpers: small value classes with no methods

# Register is a physical register
class Register:
    def __init__(self, name: str, type: str):
        self.defined_name = name                # formal name at full size        
        self.defined_type = type                # type at full size
        self.name = name                        # current name  
        self.var = None                         # current variable or None if none
    def __str__(self): return self.name
    def __repr__(self): return str(self)

# Constant is a compile-time constant value
class Constant:
    def __init__(self, value: bytes, type: str):
        self.value = value
        self.type = type
        self.offset = 0             # offset from start of read-only data section
    def __str__(self): return f"{self.value} ({self.type})"
    def __repr__(self): return str(self)

# Variable lives in read/write data
class Variable:
    def __init__(self, name: str, type: str, count: int):
        self.name = name
        self.type = type
        self.count = count
        self.offset = 0             # offset from start of read/write data section  
    def __str__(self): return f"{self.name} ({self.type})"
    def __repr__(self): return str(self)

# Operand can be a registe, constant, or variable
Operand = Register | Constant | Variable

# Instruction is a single instruction in the ISA
class Instruction:
    def __init__(self, opcode: str, dest: Register, sources: List[Operand]):
        self.opcode = opcode
        self.dest = dest
        self.sources = sources
        self.comment = ""
    def __str__(self):
        out = f"{self.opcode}"
        if self.dest is not None: out += f" {self.dest}"
        if self.sources: out += f", {', '.join([str(source) for source in self.sources])}"
        return out
    def __repr__(self): return str(self)

#--------------------------------------------------------------------------------------------------
# RegisterManager

class RegisterSet:
    def __init__(self, n_registers: int, type_map: Dict[str, str]):
        first_key = next(iter(type_map.keys()))
        first_value = type_map[first_key]
        self.registers = [Register(f"{first_value}{i}", first_key) for i in range(n_registers)]
        self.type_map = type_map
    def __str__(self): return ", ".join([str(register) for register in self.registers])
    def __repr__(self): return str(self)

class RegisterManager:
    def __init__(self, int_registers: RegisterSet, fp_registers: RegisterSet):
        self.int_registers = int_registers
        self.fp_registers = fp_registers
    def __str__(self): return "; ".join([str(register) for register in self.int_registers.registers])
    def __repr__(self): return str(self)

#--------------------------------------------------------------------------------------------------
# ISA is a CPU architecture base class

class ISA:
    def __init__(self): override_me()
    def pointer_type(self) -> str: override_me()
    def init_register_manager(self) -> RegisterManager: override_me()
    def register_prefix(self, type: str) -> str: override_me()
    def data_register(self, register_manager: RegisterManager) -> Register: override_me()
    def stack_register(self, register_manager: RegisterManager) -> Register: override_me()
    def load_rel_address(self, register: Register, address: int, pc: int) -> List[Instruction]: override_me()
    def load_int_immediate(self, register: Register, constant: int) -> List[Instruction]: override_me()
    def load_float_from_memory(self, register: Register, data_register: Register, offset: int) -> List[Instruction]: override_me()
    def get_opcode(self, opcode: str, type: str) -> str: override_me()

#--------------------------------------------------------------------------------------------------
# RISCV ISA

class RISCV32(ISA):
    def __init__(self):
        self.arithmetic_opcodes = set(["add", "sub", "mul", "div", "sqrt"])
    def pointer_type(self) -> str: return "i32"
    def init_register_manager(self) -> RegisterManager:
        int_registers = RegisterSet(32, {"i32" : "x", "i16" : "x", "i8" : "x", "u32" : "x", "u16" : "x", "u8" : "x"})
        fp_registers = RegisterSet(32, {"f32" : "f"})
        return RegisterManager(int_registers, fp_registers)
    def register_prefix(self, type: str) -> str: return "f" if type[0] == "f" else "x"
    def data_register(self, register_manager: RegisterManager) -> Register: return register_manager.int_registers.registers[3]
    def stack_register(self, register_manager: RegisterManager) -> Register: return register_manager.int_registers.registers[2]
    def load_rel_address(self, register: Register, address: int, pc: int) -> List[Instruction]:
        offset = address - pc
        if offset < -2048 or offset > 2047: raise Exception("offset out of range")
        return [Instruction("auipc", register, [(offset >> 12) & 0xffff]),
                Instruction("addi", register, [register, offset & 0xfff])]
    def load_int_immediate(self, register: Register, constant: int) -> List[Instruction]:
        if -2048 <= constant < 2048:
            return [Instruction("addi", register, [Register("x0"), constant])]
        else:
            upper = (constant + 0x800) >> 12
            lower = constant - (upper << 12)
            return [ Instruction("lui", register, [upper]),
                     Instruction("addi", register, [register, lower])]
    def load_float_from_memory(self, register: Register, data_register: Register, offset: int) -> List[Instruction]:
        return [Instruction("flw", register, [data_register, offset])]
    def get_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_opcodes:
            n_bits = int(type[1:])
            if n_bits == 32: return f"f{opcode}.s"
            elif n_bits == 64: return f"f{opcode}.d"
        raise Exception(f"riscv: unsupported opcode {opcode}")

#--------------------------------------------------------------------------------------------------
# ARM64 ISA

class ARM64(ISA):
    def __init__(self):
        self.arithmetic_opcodes = set(["add", "sub", "mul", "div", "sqrt"])
    def pointer_type(self) -> str: return "i64"
    def init_register_manager(self) -> RegisterManager:
        int_registers = RegisterSet(32, { "i64" : "x", "i32" : "w", "i16" : "w", "i8" : "w", "u32" : "w", "u16" : "w", "u8" : "w"})
        fp_registers = RegisterSet(32, {"f64" : "d", "f32" : "s"})
        return RegisterManager(int_registers, fp_registers)
    def register_prefix(self, type: str) -> str:
        n_bits = int(type[1:])
        if type[0] == "f":
            return "d" if n_bits == 64 else "s"
        else:
            return "x" if n_bits == 64 else "w"
    def data_register(self, register_manager: RegisterManager) -> Register: return register_manager.int_registers.registers[28]
    def stack_register(self, register_manager: RegisterManager) -> Register: return register_manager.int_registers.registers[29]
    def load_rel_address(self, register: Register, address: int, pc: int) -> List[Instruction]:
        offset = address - pc
        if -(1 << 20) <= offset < (1 << 20):                    # Use ADR if the offset is within ±1 MB (21-bit signed immediate)
            immlo = offset & 0x3
            immhi = (offset >> 2) & ((1 << 19) - 1)
            return [Instruction("adr", register, [immhi, immlo])]
        else:                                                   # Use ADRP + ADD for addresses outside the ADR immediate range.
            pc_page = pc & ~0xfff        # PC page base (aligned to 4KB)
            target_page = address & ~0xfff  # Target page base
            page_offset = (target_page - pc_page) >> 12  # Difference in pages
            if page_offset < -(1 << 20) or page_offset >= (1 << 20): raise Exception("page offset out of range")
            immlo = page_offset & 0x3
            immhi = (page_offset >> 2) & ((1 << 19) - 1)
            lower_offset = address & 0xfff  # Lower 12-bit offset within the page
            return [Instruction("adrp", register, [immhi, immlo], "load address: high 20 bits"),
                    Instruction("add", register, [register, lower_offset], "load address: low 12 bits")]
    def load_int_immediate(self, register: Register, int_value: int) -> List[Instruction]:
        constant = int_value & 0xffffffff  # Treat the value as a 32-bit unsigned constant.
        lower16 = constant & 0xffff
        upper16 = (constant >> 16) & 0xffff
        if constant == lower16: # If the constant fits in 16 bits, a single movz suffices.
            return [Instruction("movz", register, [lower16, 0])]
        else:
            return [Instruction("movz", register, [lower16, 0])] + \
                ([Instruction("movk", register, [upper16, 16])] if upper16 != 0 else [])
    def load_float_from_memory(self, register: Register, data_register: Register, offset: int) -> List[Instruction]:
        return [Instruction("ldr", register, [data_register, offset])]
    def get_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_opcodes:
            return f"f{opcode}" if type[0] == "f" else f"{opcode}"


    