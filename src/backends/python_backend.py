# ᕦ(ツ)ᕤ
# backends/python.py
# author: asnaroo
# zero to anything

from typing import Dict
from src.backends import Backend, BackendConfig
from src.compiler import *
from src.entity import *
import numpy as np
from copy import deepcopy
#--------------------------------------------------------------------------------------------------
class PythonBackend(Backend):
    def generate(self):
        self.out = ""
        self.i_var = 0
        #self.out += self.preamble()
        test_function = self.compiler.cp.st.find("vector", zc.FunctionDef, None, True)[0].element
        var = self.add_var("a", "vector")
        test_function.generate([var, "1", "2", "3"])
        log("----------------------------------------------")
        log(self.out)
        log_exit("done")
        return self.out

    #-----------------------------------------------------------------------
    def setup_generate(self):
        backend = self

        @Entity.method(zc.FunctionDef)     # FunctionDef.generate
        def generate(self, args: List[str]):
            log(f"FunctionDef.generate: {backend.show(self)}, {args}")
            func_vars = backend.get_function_vars(self)
            log(f"  func_vars: {func_vars}")
            replace = make_replace_dict(func_vars, args)
            log(f"  replace: {replace}")
            for s in self.body.statements:
                s.generate(replace)

        @Entity.method(zc.Assignment)   # Assignment.generate
        def generate(self, replace):
            log(f"Assignment.generate: {backend.show(self)}, {replace}")
            if self.lhs is not None:
                lhs = self.lhs.generate(replace)
                log(f"  lhs: {lhs}")
            rhs = self.rhs.generate(replace)
            log(f"  rhs: {rhs}")
            if len(lhs) != len(rhs): backend.error(f"Assignment.generate: {backend.show(self)}, {lhs} != {rhs}")
            for i in range(len(lhs)):
                backend.output(f"{lhs[i]} = {rhs[i]}")

        @Entity.method(zc.AssignmentLhs)   # AssignmentLhs.generate
        def generate(self, replace):
            results = []
            for r in self.results:
                results.append(r.generate(replace))
            return results
        
        @Entity.method(zc.ResultVariableRef)   # ResultVariableRef.generate
        def generate(self, replace):
            result_names = [ str(var) for var in self.variable.variables]
            if result_names[0] in replace:
                result_names[0] = replace[result_names[0]]
            return ".".join(result_names)
        
        @Entity.method(zc.ResultVariableDef)   # ResultVariableDef.generate
        def generate(self, replace):
            result_names = [ str(name.name) for name in self.names]
            if result_names[0] in replace:
                result_names[0] = replace[result_names[0]]
            return ".".join(result_names)
        
        @Entity.method(zc.VariableRef)   # VariableRef.generate
        def generate(self, replace):
            result_names = [ str(var) for var in self.variables]
            if result_names[0] in replace:
                result_names[0] = replace[result_names[0]]
            return [".".join(result_names)]

    
    #-----------------------------------------------------------------------
    # below the line

    def add_var(self, name, type) -> str:
        var_name = f"{name}_{self.i_var}"
        self.output(f"var {var_name}: {type}")
        self.i_var += 1
        return var_name
    
    def output(self, s: str):
        self.out += s + "\n"
        log(log_green(s))

    def error(self, s: str):
        log(log_red(s))
        log_exit(f"error: {s}")

    def show(self, e):
        return print_code_formatted(e, self.compiler.grammar).replace("\n", "↩︎").replace("    ", "")

    def get_function_vars(self, funcDef: zc.FunctionDef) -> List[str]:
        vars = []
        for r in funcDef.results.results:
            for name in r.names:
                vars.append(str(name.name))
        for item in funcDef.signature.elements:
            if isinstance(item, zc.FunctionSignatureParams):
                for param in item.params:
                    for name in param.names:
                        vars.append(str(name.name))
        return vars

#--------------------------------------------------------------------------------------------------
# super below the line

def make_replace_dict(func_vars: List[str], args: List[str]) -> Dict[str, str]:
    replace = {}
    for i in range(len(func_vars)):
        replace[func_vars[i]] = args[i]
    return replace
