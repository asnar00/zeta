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

class VmValue: pass

class VmVar(VmValue):
    i_var : int = 0
    def __init__(self, name: str, type: str):
        self.name : str = name
        self.type : str = type.name if isinstance(type, Type) else type
    def __str__(self): return f"{self.name}:{self.type}"
    def __repr__(self): return str(self)
    @staticmethod
    def tag(name: str) -> str:
        result = f"{name}_{VmVar.i_var}"
        VmVar.i_var += 1
        return result

class VmConst(VmValue):
    def __init__(self, type: str, count: int, values: List[str]):
        self.type : str = type
        self.count : int = count
        self.values : List[str] = values
    def __str__(self): return f"{self.type}[{self.count}]"
    def __repr__(self): return str(self)

class VmInstruction(VmValue):
    def __init__(self, opcode: str, dests: List[VmVar], sources: List[VmValue], comment: str = None, label: str = None):
        self.opcode : str = opcode
        self.dests : List[VmVar] = dests
        self.sources : List[VmValue] = sources
        self.label: str = None
        self.comment: str = None
    def __str__(self):
        out = ""
        if self.label is not None: out += f"{self.label}:\n"
        comment = f"\t# {self.comment}" if self.comment is not None else ""
        dest = f"{self.dests}" if len(self.dests) > 0 else "\t"
        out += f"    {self.opcode}\t{dest}\t{self.sources}{comment}"
        return out
    def __repr__(self): return str(self)

#--------------------------------------------------------------------------------------------------

class VmBlock:
    def __init__(self, inputs: List[VmVar]=None, outputs: List[VmValue]=None, instructions: List[VmInstruction]=None):
        self.inputs : List[VmVar] = inputs or []
        self.outputs : List[VmValue] = outputs or []
        self.instructions : List[VmInstruction] = instructions or []
    def __str__(self):
        if len(self.instructions) == 0: return "()"
        out = "("
        for result in self.outputs: out += f"{result}, "
        if len(self.outputs) > 0: out = out[:-2]
        out += " <- "
        for param in self.inputs: out += f"{param}, "
        if len(self.inputs) > 0: out = out[:-2]
        out += ")\n"
        for instruction in self.instructions: out += f"{instruction}\n"
        return out
    def __repr__(self): return str(self)

    @staticmethod
    # this is gonna need a lot of work
    def combine(blocks: List['VmBlock']) -> 'VmBlock':
        all_inputs = []
        all_instructions = []
        all_outputs = []
        for block in blocks:
            all_inputs.extend(block.inputs)
            all_instructions.extend(block.instructions)
            all_outputs.extend(block.outputs)
        
        return VmBlock(all_inputs, all_outputs, all_instructions)

    @staticmethod
    def replace_vars(self, old_vars: List[VmVar], new_vars: List[VmVar]) -> List[VmInstruction]:
        if len(old_vars) != len(new_vars):
            log(f"old_vars: {old_vars}")
            log(f"new_vars: {new_vars}")
            log_exit("replace_vars")
        new_instructions = []
        for instruction in self.instructions:
            new_dests = [new_vars[old_vars.index(dest)] if dest in old_vars else dest for dest in instruction.dests]
            new_sources = [new_vars[old_vars.index(source)] if source in old_vars else source for source in instruction.sources]
            new_instructions.append(VmInstruction(instruction.opcode, new_dests, new_sources, instruction.comment, instruction.label))
        return new_instructions



#--------------------------------------------------------------------------------------------------
