# ᕦ(ツ)ᕤ
# codegen.py
# author: asnaroo
# zero to anything

from typing import Dict
from src.symbols import *
from src.entity import *
from copy import deepcopy

#--------------------------------------------------------------------------------------------------
# configuration options for code production

class CodegenConfig:
    def __init__(self):
        self.type_substitution = {}

    def setup_types(self, type_map: Dict[str, str]):
        self.type_substitution.update(type_map)

    def get_concrete_type(self, abstract_type_name: str) -> str:
        return self.type_substitution.get(abstract_type_name, abstract_type_name)

class CodeGenerator:
    def __init__(self):
        self.config = None
        self.st = None
        self.grammar = None

    def setup(self, config: CodegenConfig, st: SymbolTable, grammar: Grammar):
        self.config = config
        self.st = st
        self.grammar = grammar

    def find_entity(self, key: str, of_type: Any) -> Entity:
        items = self.st.find(key, of_type, None, True)
        return items[0].element if len(items) == 1 else None

    def reset(self):
        self.out = ""
        self.i_var = 0
        self.indent = 0

    def add_var(self, name, type_name) -> str:
        var_name = f"{name}_{self.i_var}"
        var_type_name = self.config.get_concrete_type(type_name)
        self.output(f"var('{var_name}', '{var_type_name}')")
        self.i_var += 1
        return var_name
    
    def get_var_index(self) -> int:
        result = self.i_var
        self.i_var += 1
        return result
    
    def output(self, s: str):
        self.out += "    "*self.indent + s + "\n"
        log(log_green(s))

    def error(self, s: str):
        log(log_red(s))
        log_exit(f"error: {s}")

    def show(self, e):
        return print_code_formatted(e, self.grammar).replace("\n", "↩︎").replace("    ", "")
    
#--------------------------------------------------------------------------------------------------
# super below the line

def try_replace(var, replace):
    if "." in var:
        vars = var.split(".")
        if vars[0] in replace:
            return replace[vars[0]] + "." + ".".join(vars[1:])
    else:
        if var in replace: return replace[var]
    return var