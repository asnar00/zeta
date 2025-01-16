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
        self.test_function()
        self.reset()
        test_function = self.find_function("test_vectormath")
        test_function.generate({})

        
        log("----------------------------------------------")
        log(self.out)
        log_exit("done")
        return self.out
    
    def test_function(self):
        self.reset()
        test_function = self.find_function("vector◦◦◦")
        var = self.add_var("a", "vector")
        test_function.generate({"x":"1", "y":"2", "z":"3", "_results":[var]})
        test("test_function", self.out, """
            var a_0: vector
            # _function (vector _v) = vector (number x, y, z = 0)
            mov a_0.x, 1
            mov a_0.y, 2
            mov a_0.z, 3
        """)


    #-----------------------------------------------------------------------
    def setup_generate(self):
        backend = self

        @Entity.method(zc.FunctionCall)  # FunctionCall.generate
        def generate(self, replace):
            log(f"FunctionCall.generate: {backend.show(self)}, {replace}")
            function = self._resolved_functions[0]
            result_vars = replace["_results"] if "_results" in replace else []
            replace.pop("_results", None)
            if len(result_vars) == 0:
                result_vars = backend.make_temp_vars(function)
            args = []
            for item in self.items:
                if not hasattr(item, "name"):
                    args += item.generate(replace)
            log(f"  args: {args}")
            function = self._resolved_functions[0]
            params = backend.get_function_params(function)
            log(f"  params: {params}")
            for p, a in zip(params, args):
                replace[p] = a
            replace["_results"] = result_vars
            log(f"  replace: {replace}")
            results = function.generate(replace)
            return result_vars
            log_exit("FunctionCall.generate")

        @Entity.method(zc.FunctionCallArguments) # FunctionCallArguments.generate
        def generate(self, replace):
            log(f"FunctionCallArguments.generate: {backend.show(self)}, {replace}")
            args = []
            for item in self.arguments:
                args += item.generate(replace)
            return args
        
        @Entity.method(zc.FunctionCallArgument) # FunctionCallArgument.generate
        def generate(self, replace):
            log(f"FunctionCallArgument.generate: {backend.show(self)}, {replace}")
            return self.value.generate(replace)
        
        @Entity.method(zc.Constant) # Constant.generate
        def generate(self, replace) -> List[str]:
            return [str(self.value)]
        
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.generate
        def generate(self, replace) -> List[str]:
            return self.variable.generate(replace)

        @Entity.method(zc.Function)     # Function.generate
        def generate(self, replace):
            log(f"Function.generate: {backend.show(self)}, {replace}")
            backend.output(f"# {backend.show(self).split('↩︎')[0]}")
            for s in self.body.statements:
                s.generate(replace)

        @Entity.method(zc.SingleFunctionDef)
        def generate(self, replace):
            log(f"SingleFunctionDef.generate: {backend.show(self)}, {replace}")
            self.func_def.generate(replace)

        @Entity.method(zc.FunctionDef)     # FunctionDef.generate
        def generate(self, replace):
            log(f"FunctionDef.generate: {backend.show(self)}, {replace}")
            if "_results" in replace:
                results = backend.get_function_results(self)
                for r, v in zip(results, replace["_results"]):
                    replace[r] = v
            for s in self.body.statements:
                s.generate(replace)

        @Entity.method(zc.Assignment)   # Assignment.generate
        def generate(self, replace):
            log(f"Assignment.generate: {backend.show(self)}, {replace}")
            if self.lhs is not None:
                lhs = self.lhs.generate(replace)
                log(f"  lhs: {lhs}")
            rhs_replace = replace.copy()
            lhs_vars = [str(var) for var in lhs.values()]
            rhs_replace.update({"_results": lhs_vars})
            rhs = self.rhs.generate(rhs_replace)
            log(f"  rhs: {rhs}")
            if len(lhs) != len(rhs): backend.error(f"Assignment.generate: {backend.show(self)}, {lhs} != {rhs}")
            for l, r in zip(lhs, rhs):
                if not r in lhs_vars:
                    backend.output(f"mov {l}, {r}")

        @Entity.method(zc.AssignmentLhs)   # AssignmentLhs.generate
        def generate(self, replace):
            results = {}
            for r in self.results:
                results.update(r.generate(replace))
            return results
        
        @Entity.method(zc.ResultVariableRef)   # ResultVariableRef.generate
        def generate(self, replace):
            result_names = [ str(var) for var in self.variable.variables]
            if result_names[0] in replace:
                result_names[0] = replace[result_names[0]]
            result_name = ".".join(result_names)
            return { result_name : result_name }
        
        @Entity.method(zc.ResultVariableDef)   # ResultVariableDef.generate
        def generate(self, replace):
            result_names = [ str(name.name) for name in self.names]
            if result_names[0] in replace:
                result_names[0] = replace[result_names[0]]
            result_name = ".".join(result_names)
            actual_name = backend.add_var(result_name, str(self.type.name))
            return { result_name : actual_name }
        
        @Entity.method(zc.VariableRef)   # VariableRef.generate
        def generate(self, replace):
            result_names = [ str(var) for var in self.variables]
            if result_names[0] in replace:
                result_names[0] = replace[result_names[0]]
            result_name = ".".join(result_names)
            return { result_name : result_name }

    
    #-----------------------------------------------------------------------
    # below the line

    def find_function(self, short_sig: str) -> zc.Function:
        return self.compiler.cp.st.find(short_sig, zc.Function, None, True)[0].element

    def reset(self):
        self.out = ""
        self.i_var = 0

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

    def get_function_replace(self, func_def: zc.Function, func_args: List[str]) -> Dict:
        func_params = self.get_function_params(func_def)
        return make_replace_dict(func_args, func_params)

    def get_function_params(self, func: zc.Function) -> List[str]:
        vars = []
        for item in func.signature.elements:
            if isinstance(item, zc.FunctionSignatureParams):
                for param in item.params:
                    for name in param.names:
                        vars.append(str(name.name))
        return vars
    
    def get_function_results(self, func: zc.Function) -> List[str]:
        vars = []
        if func.results is not None:
            for r in func.results.results:
                for name in r.names:
                    vars.append(str(name.name))
        return vars
    
    def get_function_result_types(self, func: zc.Function) -> List[str]:
        types = []
        if func.results is not None:
            for r in func.results.results:
                for name in r.names:
                    types.append(str(r.type.name))
        return types
    
    def make_temp_vars(self, func: zc.Function) -> List[str]:
        result_vars = []
        results = self.get_function_results(func)
        types = self.get_function_result_types(func)
        log(f"  results: {results}")
        log(f"  types: {types}")
        temp_results = []
        for r, t in zip(results, types):
            temp_var = self.add_var(r, t)
            result_vars.append(temp_var)
        return result_vars
    
#--------------------------------------------------------------------------------------------------
# super below the line

def make_replace_dict(func_vars: List[str], args: List[str]) -> Dict[str, str]:
    replace = {}
    for i in range(len(func_vars)):
        replace[func_vars[i]] = args[i]
    return replace
