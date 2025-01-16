# ᕦ(ツ)ᕤ
# backends/__init__.py
# author: asnaroo
# zero to anything

from src.compiler import CompiledProgram, Compiler
from src.entity import Entity
from src.util import *
from src import zero_classes as zc

class BackendConfig:
    def __init__(self): pass

class Backend:
    def __init__(self, compiler: Compiler, cp: CompiledProgram, config: BackendConfig):
        self.compiler = compiler
        self.cp = cp
        self.config = config
        self.clear_methods()
        self.setup_generate()

    def generate(self): pass


    #-----------------------------------------------------------------------
    # override this in the backend subclass to set up per-class output methods
    def setup_generate(self): pass

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
