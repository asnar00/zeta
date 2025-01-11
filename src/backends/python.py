# ᕦ(ツ)ᕤ
# backends/python.py
# author: asnaroo
# zero to anything

from src.backends import Backend, BackendConfig
from src.compiler import *
from src.entity import *


#--------------------------------------------------------------------------------------------------
class PythonBackend(Backend):
    def __init__(self): pass
    def generate(self, cp: CompiledProgram, config: BackendConfig):
        log_clear()
        log("----------------------------------------------------------------")
        log("python backend")
        objects = cp.st.objects_of_type(zc.Type)
        for obj in objects:
            log(f"obj = {obj}")