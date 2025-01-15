# ᕦ(ツ)ᕤ
# backends/__init__.py
# author: asnaroo
# zero to anything

from src.compiler import CompiledProgram
from src.entity import Entity
from src.util import *
from src import zero_classes as zc

class BackendConfig:
    def __init__(self): pass

class Backend:
    def __init__(self): pass
    def setup(self, cp: CompiledProgram, config: BackendConfig):
        self.clear_methods()
        self.setup_methods(cp, config)
    def generate(self, cp: CompiledProgram, config: BackendConfig) -> str:
        out = ""
        out += self.preamble(cp, config)
        return out

    def preamble(self, cp: CompiledProgram, config: BackendConfig) -> str:
        pass

    def generate_objects(self, obj_type: type, cp: CompiledProgram, config: BackendConfig) -> str:
        out = ""
        objects = cp.st.objects_of_type(obj_type)
        for obj in objects:
            code = obj.generate_code(cp, config) if hasattr(obj, "generate_code") else None     
            if code == None: continue
            out += code
        return out

    #-----------------------------------------------------------------------
    # override this in the backend subclass to set up per-class output methods
    def setup_methods(self, cp: CompiledProgram, config: BackendConfig): pass

    #-----------------------------------------------------------------------
    # below the line / helpers

    # backend base class calls this to clear all methods before calling setup_methods
    # so we can support multiple backends (one at a time)
    def clear_methods(self):
        for cls in Entity.classes.values():
            Entity.remove_method(cls, "generate_code")

    # indent a string (1 indent = 4 spaces)
    def indent(self, s: str, indent: int) -> str:
        lines = s.strip().split("\n")
        return "\n".join([("    " * indent) + line for line in lines])
