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
        self.type = type                        # current type  
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
    def load(self, register: Register, data_register: Register, offset: int) -> List[Instruction]: override_me()
    def store(self, register: Register, data_register: Register, offset: int) -> List[Instruction]: override_me()
    def get_opcode(self, opcode: str, type: str) -> str: override_me()
    def encode_instruction(self, instr: Instruction) -> int: override_me()

#--------------------------------------------------------------------------------------------------
# RISCV ISA

class RISCV32(ISA):
    def __init__(self):
        self.arithmetic_opcodes = set(["add", "sub", "mul", "div", "sqrt"])
        self.load_opcodes = { "u8": "lbu", "u16": "lhu", "u32": "lwu", "u64": "ld", # unsigned ints
                              "i8": "lb", "i16": "lh", "i32": "lw", "i64": "ld", # signed ints
                              "f32": "flw", "f64": "fld" } # floats
        self.store_opcodes = { "u8": "sb", "u16": "sh", "u32": "sw", "u64": "sd", # unsigned ints
                              "i8": "sb", "i16": "sh", "i32": "sw", "i64": "sd", # signed ints
                              "f32": "fsw", "f64": "fstd" } # floats
        self.load_bits = {"lb":  0x00000003, "lh":  0x00001003, "lw":  0x00002003, "ld":  0x00003003, 
                          "lbu": 0x00004003, "lhu": 0x00005003, "lwu": 0x00006003,
                          "flw": 0x00002007, "fld": 0x00003007} 
        self.store_bits = { "sb": 0x00000023, "sh": 0x00001023, "sw": 0x00002023,"sd": 0x00003023,  # func3=011, opcode=0100011
                          "fsw": 0x00002027, "fsd": 0x00003027 }
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
    def load(self, register: Register, data_register: Register, offset: int) -> List[Instruction]:
        return [Instruction(self.load_opcodes[register.type], register, [data_register, offset])]
    def store(self, register: Register, data_register: Register, offset: int) -> List[Instruction]:
        return [Instruction(self.store_opcodes[register.type], register, [data_register, offset])]
    def get_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_opcodes:
            n_bits = int(type[1:])
            if n_bits == 32: return f"f{opcode}.s"
            elif n_bits == 64: return f"f{opcode}.d"
        raise Exception(f"riscv: unsupported opcode {opcode}")
    def encode_instruction(self, i: Instruction, pc: int) -> int:
        def rd(instr): return int(instr.dest.defined_name[1:]) if instr.dest is not None else 0
        def rs1(instr): return int(instr.sources[0].defined_name[1:]) if len(instr.sources) > 0 else 0
        def rs2(instr): return int(instr.sources[1].defined_name[1:]) if len(instr.sources) > 1 else 0
        def imm(instr, i_source): return int(instr.sources[i_source], 16) if isinstance(instr.sources[i_source], str) else instr.sources[i_source]    
        def encode_jal(target_address, pc, rd):
            imm = target_address - pc
            if imm % 2 != 0: raise Exception("imm must be aligned to 2 bytes (even number)")
            if imm < 0: imm = ((1 << 21) + imm) & 0x1fffff
            imm_20 = (imm & 0b100000000000000000000) >> 20
            imm_10_1 = (imm & 0b000000000011111111110) >> 1
            imm_11 = (imm & 0b000000000100000000000) >> 11
            imm_19_12 = (imm & 0b011111111000000000000) >> 12
            instr = (imm_20 << 31) | (imm_10_1 << 21) | (imm_11 << 20) | (imm_19_12 << 12) | (rd << 7) | 0x6f
            return instr
        if i.opcode == "auipc":        return (imm(i, 0) << 12) | (rd(i) << 7) | 0b0010111
        elif i.opcode == "lui":        return (imm(i, 0) << 12) | (rd(i) << 7) | 0b0110111
        elif i.opcode == "addi":       return ((imm(i, 1) & 0xfff) << 20) | (rs1(i) << 15) | (rd(i) << 7) | 0b0010011 
        elif i.opcode == "sw":         return ((imm(i, 1) >> 5) << 25) | (rd(i) << 20) | (rs1(i) << 15) | (0b010 << 12) | ((imm(i, 1) & 0x1f) << 7) | 0b0100111
        elif i.opcode in self.load_bits: return (imm(i, 1) << 20) | (rs1(i) << 15) | (rd(i) << 7) | self.load_bits[i.opcode]
        elif i.opcode in self.store_bits: return ((imm(i, 1) >> 5) << 25) | (rd(i) << 20) | (rs1(i) << 15) | ((imm(i, 1) & 0x1f) << 7) | self.store_bits[i.opcode]
        elif i.opcode == "fadd.s":     return (0b0000000 << 25) | (rs2(i) << 20) | (rs1(i) << 15) | (0b000 << 12) | (rd(i) << 7) | 0b1010011
        elif i.opcode == "fsub.s":     return (0b0000100 << 25) | (rs2(i) << 20) | (rs1(i) << 15) | (0b000 << 12) | (rd(i) << 7) | 0b1010011
        elif i.opcode == "fmul.s":     return (0b0001000 << 25) | (rs2(i) << 20) | (rs1(i) << 15) | (0b000 << 12) | (rd(i) << 7) | 0b1010011
        elif i.opcode == "fdiv.s":     return (0b0001100 << 25) | (rs2(i) << 20) | (rs1(i) << 15) | (0b000 << 12) | (rd(i) << 7) | 0b1010011
        elif i.opcode == "fsqrt.s":    return (0b0101100 << 25) | (0 << 20) | (rs1(i) << 15) | (0b000 << 12) | (rd(i) << 7) | 0b1010011
        elif i.opcode == "wfi":        return (0x105 << 20) | (0 << 15) | (0 << 12) | (0 << 7) | 0x73
        elif i.opcode == "jal":        return encode_jal(imm(i, 0)*4, pc*4, rd(i))
        else: raise Exception(f"unsupported instruction: {i.opcode}")
        

#--------------------------------------------------------------------------------------------------
# ARM64 ISA

class ARM64(ISA):
    def __init__(self):
        self.arithmetic_opcodes = set(["add", "sub", "mul", "div", "sqrt"])
        self.ldr_opcodes = {"u8": "ldrb", "u16": "ldrh", "u32": "ldr", "u64": "ldr", # unsigned ints
                            "i8": "ldrsb", "i16": "ldrsh", "i32": "ldrsw", "i64": "ldr",   # signed ints
                            "f32": "ldr", "f64": "ldr"}  # floats
        self.str_opcodes = { "u8": "strb", "u16": "strh", "u32": "str", "u64": "str", 
                            "i8": "strb", "i16": "strh", "i32": "str", "i64": "str", 
                            "f32": "str", "f64": "str" } 
        self.ldr_bits = { "u8": 0x39400000, "u16": 0x79400000, "u32": 0xB9400000, "u64": 0xF9400000,
                          "i8": 0x39C00000, "i16": 0x79C00000, "i32": 0xB9800000, "i64": 0xF9400000,
                          "f32": 0xBD400000, "f64": 0xBD400000}
        self.str_bits = { "u8": 0x39000000, "u16": 0x79000000, "u32": 0xB9000000, "u64": 0xF9000000, # unsigned ints
                          "i8": 0x39000000, "i16": 0x79000000, "i32": 0xB9000000, "i64": 0xF9000000, # signed ints
                          "f32": 0xBD000000, "f64": 0xFD000000} # floats
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
    def load(self, register: Register, data_register: Register, offset: int) -> List[Instruction]:
        return [Instruction(self.ldr_opcodes[register.type], register, [data_register, offset])]
    def store(self, register: Register, data_register: Register, offset: int) -> List[Instruction]:
        return [Instruction(self.str_opcodes[register.type], register, [data_register, offset])]
    def get_opcode(self, opcode: str, type: str) -> str:
        if opcode in self.arithmetic_opcodes:
            return f"f{opcode}" if type[0] == "f" else f"{opcode}"
    def encode_instruction(self, i: Instruction, pc: int) -> int:
        def reg_num(reg: Register) -> int: return int(reg.defined_name[1:])
        def imm(instr, idx: int) -> int: val = instr.sources[idx]; return int(val) if isinstance(val, int) else int(val, 16)
        def rd(instr): return reg_num(instr.dest)
        def rn(instr): return reg_num(instr.sources[0])
        def rm(instr): return reg_num(instr.sources[1])
        if i.opcode == "adr":               return (imm(i, 1) << 29) | (imm(i, 0) << 5) | (rd(i)) | 0x10000000
        elif i.opcode == "adrp":            return (imm(i, 1) << 29) | (imm(i, 0) << 5) | (rd(i)) | 0x90000000
        elif i.opcode == "add":             return ((imm(i, 1) & 0xfff) << 10) | (rn(i) << 5) | (rd(i)) | 0x11000000
        elif i.opcode.startswith("ldr"):     return ((imm(i, 1) & 0xfff) << 10) | (rn(i) << 5) | (rd(i)) | self.ldr_bits[i.dest.type]
        elif i.opcode.startswith("str"):     return ((imm(i, 1) & 0xfff) << 10) | (rn(i) << 5) | (rd(i)) | self.str_bits[i.dest.type]
        elif i.opcode == "movz":            return (((imm(i, 1) // 16) & 0x3) << 21) | ((imm(i, 0) & 0xffff) << 5) | rd(i) | 0x52800000
        elif i.opcode == "movk":            return (((imm(i, 1) // 16) & 0x3) << 21) | ((imm(i, 0) & 0xffff) << 5) | rd(i) | 0x72800000
        elif i.opcode == "fadd":            return (rm(i) << 16) | (rn(i) << 5) | rd(i) | 0x1E201000  # for FADD
        elif i.opcode == "fsub":            return (rm(i) << 16) | (rn(i) << 5) | rd(i) | 0x1E201400  # for FSUB
        elif i.opcode == "fmul":            return (rm(i) << 16) | (rn(i) << 5) | rd(i) | 0x1E201800  # for FMUL
        elif i.opcode == "fdiv":            return (rm(i) << 16) | (rn(i) << 5) | rd(i) | 0x1E201C00  # for FDIV
        elif i.opcode == "fsqrt":           return (rn(i) << 5) | rd(i) | 0x1E202000
        else: raise Exception(f"unsupported instruction: {i.opcode}")



    