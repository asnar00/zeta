# ᕦ(ツ)ᕤ
# backends/__init__.py
# author: asnaroo
# zero to anything

from src.compiler import CompiledProgram
from src.entity import Entity

class BackendConfig:
    def __init__(self): pass

class Backend:
    def __init__(self): pass
    def generate(self, cp: CompiledProgram, config: BackendConfig): pass
