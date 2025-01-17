# ᕦ(ツ)ᕤ
# backends/python.py
# author: asnaroo
# zero to anything

from typing import Dict
from src.backends import Backend, BackendConfig
from src.compiler import *
from src.entity import *
from copy import deepcopy
#--------------------------------------------------------------------------------------------------
class PythonBackend(Backend):
    def generate(self):
        self.test_function()
        self.reset()
        test_function = self.find_function("test_vectormath")
        self.output(f"def {self.emit_fn_name(test_function)}():")
        self.indent += 1
        test_function.generate({})
        self.indent -= 1

        log_clear()
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
            var('a_0', 'vector')
            mov('a_0.x', '1')
            mov('a_0.y', '2')
            mov('a_0.z', '3')
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
                log(" making temp_vars")
                result_vars = backend.make_temp_vars(function)
            args = []
            for item in self.items:
                if not hasattr(item, "name"):
                    args += item.generate(replace)
            log(f"  args: {args}")
            function = self._resolved_functions[0]
            params = backend.get_function_params(function)
            log(f"  params: {params}")
            fn_replace = {}
            for p, a in zip(params, args):
                a = try_replace(a, replace)
                fn_replace[p] = a
            fn_replace["_results"] = result_vars
            log(f"  fn_replace: {fn_replace}")
            results = function.generate(fn_replace)
            return result_vars

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
            if isinstance(self.body, zc.EmitFunctionBody):
                return backend.emit(self, replace)
            return self.body.generate(replace)
        
        @Entity.method(zc.FunctionStatements)   # FunctionStatements.generate
        def generate(self, replace) -> List[str]:
            log(f"FunctionStatements.generate: {backend.show(self)}, {replace}")
            for s in self.statements:
                if not isinstance(s, zc.Assignment): continue
                lhs = s.lhs.generate(replace) if s.lhs else []
                log(f"  lhs: {lhs}")
                fn_replace = replace.copy()
                if len(lhs) > 0: fn_replace["_results"] = lhs
                rhs = s.rhs.generate(fn_replace)
                log(f"  rhs: {rhs}")
                if len(lhs) != len(rhs):
                    backend.error(f"FunctionStatements.generate: {backend.show(self)}, {lhs} != {rhs}")
                for l, r in zip(lhs, rhs):
                    if l != r:
                        backend.output(f"mov('{l}', '{r}')")
                log(f"  after assign: replace = {replace}")

        @Entity.method(zc.AssignmentLhs)   # AssignmentLhs.generate
        def generate(self, replace) -> List[str]:
            results = []
            for r in self.results:
                results += r.generate(replace)
            return results
        
        @Entity.method(zc.ResultVariableRef)   # ResultVariableRef.generate
        def generate(self, replace):
            return self.variable.generate(replace)

        @Entity.method(zc.ResultVariableDef)   # ResultVariableDef.generate
        def generate(self, replace):
            result_names = [ str(name.name) for name in self.names]
            actual_names = [backend.add_var(r, str(self.type.name)) for r in result_names]
            log(f"  result_names: {result_names}")
            log(f"  actual_names: {actual_names}")
            for r, a in zip(result_names, actual_names):
                replace[r] = a
            return actual_names
        
        @Entity.method(zc.VariableRef)   # VariableRef.generate
        def generate(self, replace) -> List[str]:
            log(f"VariableRef.generate: {backend.show(self)}, {replace}")
            full = ".".join(str(v) for v in self.variables)
            return [try_replace(full, replace)]
    
    #-----------------------------------------------------------------------
    # below the line

    def find_function(self, short_sig: str) -> zc.Function:
        return self.compiler.cp.st.find(short_sig, zc.Function, None, True)[0].element

    def reset(self):
        self.out = ""
        self.i_var = 0
        self.indent = 0

    def add_var(self, name, type) -> str:
        var_name = f"{name}_{self.i_var}"
        self.output(f"var('{var_name}', '{type}')")
        self.i_var += 1
        return var_name
    
    def output(self, s: str):
        self.out += "    "*self.indent + s + "\n"
        log(log_green(s))

    def error(self, s: str):
        log(log_red(s))
        log_exit(f"error: {s}")

    def show(self, e):
        return print_code_formatted(e, self.compiler.grammar).replace("\n", "↩︎").replace("    ", "")

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
    
    def emit(self, func: zc.Function, replace):
        log(f"emit: {self.show(func)}, {replace}")
        fn_name = self.emit_fn_name(func)
        result_vars = self.get_function_results(func)
        result_vars = [try_replace(r, replace) for r in result_vars]
        params = self.get_function_params(func) 
        params = [try_replace(p, replace) for p in params]
        log(f"  fn_name: {fn_name}")
        log(f"  result_vars: {result_vars}")
        log(f"  params: {params}")
        result_vars = [f"'{r}'" for r in result_vars]
        params = [f"'{p}'" for p in params]
        self.output(f"{fn_name}({', '.join(result_vars)}, {', '.join(params)})")
        return result_vars

    def emit_fn_name(self, func: zc.Function):
        out = ""
        for element in func.signature.elements:
            if hasattr(element, "name"): out += f"{element.name}_"
        if out.endswith("_"): out = out[:-1]
        return out
    
    def typed_emit_fn(self, func: zc.Function):
        out = ""
        for element in func.signature.elements:
            if hasattr(element, "name"): out += f"{element.name}_"
            elif isinstance(element, zc.FunctionSignatureParams):
                for param in element.params:
                    type = str(param.type.name)
                    for name in param.names:
                        out += f"{type}_"
        result_types = self.get_function_result_types(func)
        for t in result_types: out += f"_{str(t)}"
        return out
    

    
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