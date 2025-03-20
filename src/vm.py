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
    def __init__(self, name: str, type: str):
        self.name : str = name
        self.type : str = type.name if isinstance(type, Type) else type
    def __str__(self): return f"{self.name}:{self.type}"
    def __repr__(self): return str(self)

class VmConst(VmValue):
    def __init__(self, type: str, count: int, values: List[str]):
        self.type : str = type
        self.count : int = count
        self.values : List[str] = values
    def __str__(self): return f"{self.type}[{self.count}]"
    def __repr__(self): return str(self)

class VmInstruction(VmValue):
    def __init__(self, opcode: str, dest: VmVar, sources: List[VmValue], comment: str = None, label: str = None):
        self.opcode : str = opcode
        self.dest : VmVar = dest
        self.sources : List[VmValue] = sources
        self.label: str = None
        self.comment: str = None
    def __str__(self): 
        out = ""
        if self.label is not None: out += f"{self.label}:\n"
        out += f"    {self.opcode} {self.dest} {self.sources}\t# {self.comment}"
        return out
    def __repr__(self): return str(self)

#--------------------------------------------------------------------------------------------------

class VmBlock:
    def __init__(self, inputs: List[VmVar], instructions: List[VmInstruction], outputs: List[VmValue]):
        self.inputs : List[VmVar] = inputs
        self.instructions : List[VmInstruction] = instructions
        self.outputs : List[VmValue] = outputs
    def __str__(self):
        out = ""
        for result in self.outputs: out += f"{result}, "
        if len(self.outputs) > 0: out = out[:-2]
        out += " <- "
        for param in self.inputs: out += f"{param}, "
        if len(self.inputs) > 0: out = out[:-2]
        out += "\n"
        for instruction in self.instructions: out += f"{instruction}\n"
        return out
    def __repr__(self): return str(self)


#--------------------------------------------------------------------------------------------------
