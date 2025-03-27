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
    def __init__(self, opcode: str, dests: List[VmVar], sources: List[VmValue], comment: str = None):
        self.opcode : str = opcode
        self.dests : List[VmVar] = dests
        self.sources : List[VmValue] = sources
        self.comment: str = None
    def __str__(self):
        out = ""
        comment = f"\t# {self.comment}" if self.comment is not None else ""
        dest = f"{self.dests}" if len(self.dests) > 0 else ""
        if dest.startswith("["): dest = dest[1:-1]
        dest += " "*(10-len(dest))
        sources = ""
        for s in self.sources:
            if isinstance(s, VmInstruction): sources += s.label
            else: sources += str(s)
            sources += ", "
        if len(sources) > 0: sources = sources[:-2]
        sources += ""
        opcode = str(self.opcode)
        opcode += " "*(5-len(opcode))
        out += f"    {opcode}\t{dest}\t{sources}\t{comment}"
        return out
    def __repr__(self): return str(self)

class VmLabel(VmInstruction):
    def __init__(self, name: str):
        self.name = name
    def __str__(self):
        return f"{self.name}:"
    def __repr__(self): return str(self)

#--------------------------------------------------------------------------------------------------

class VmBlock:
    def __init__(self, inputs: List[VmVar]=None, outputs: List[VmValue]=None, instructions: List[VmInstruction]=None):
        self.inputs : List[VmVar] = inputs or []
        self.outputs : List[VmValue] = outputs or []
        self.instructions : List[VmInstruction] = instructions or []
    def __str__(self):
        out = ""
        if len(self.inputs) > 0:
            out += "in: ["
            for param in self.inputs: out += f"{param}, "
            out = out[:-2] + "]\n"
        for instruction in self.instructions: out += f"{instruction}\n"
        if len(self.outputs) > 0:
            out += "out:["
            for result in self.outputs: out += f"{result}, "
            out = out[:-2] + "]"
        return out
    def __repr__(self): return str(self)

    def replace_inputs(self, new_inputs: List[VmVar]) -> List[VmInstruction]:
        result = []
        for instruction in self.instructions:
            new_sources = []
            for i, source in enumerate(instruction.sources):
                if source in self.inputs:
                    index = self.inputs.index(source)
                    new_sources.append(new_inputs[index])
                else:
                    new_sources.append(source)
            result.append(VmInstruction(instruction.opcode, instruction.dests, new_sources, instruction.comment))
        return result

#--------------------------------------------------------------------------------------------------
