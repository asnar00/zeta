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
        test_functionDef = self.compiler.cp.st.find("test_vectormath", zc.Function, None, True)[0].element
        test_functionDef.generate({})
        log(self.out)
        log_exit("done")
        return self.out

    #-----------------------------------------------------------------------
    def setup_generate(self):
        backend = self

        @Entity.method(zc.Function) # Function.generate
        def generate(self, replace):
            backend.output(f"_function {backend.function_handle(self)}")
            for s in self.body.statements:
                s.generate(replace)

        @Entity.method(zc.SingleFunctionDef) # SingleFunctionDef.generate
        def generate(self, replace):
            self.funcDef.generate(replace)

        @Entity.method(zc.FunctionDef) # FunctionDef.generate
        def generate(self, replace):
            log(f"FunctionDef.generate {self}, {replace}")
            for s in self.body.statements:
                s.generate(replace)
            backend.done()

        @Entity.method(zc.Assignment) # Assignment.generate
        def generate(self, replace):
            log(f"Assignment.generate {backend.show(self)}, {replace}")
            dest_vars = self.lhs.generate(replace)
            result_names = backend.get_result_names(self.lhs)
            log(f"  result_names: {result_names}")
            log(f"  dest_vars: {dest_vars}")
            output_dest_vars = self.rhs.generate(replace, dest_vars)
            log(f"  output_dest_vars: {output_dest_vars}")
            return output_dest_vars

        @Entity.method(zc.AssignmentLhs) # AssignmentLhs.generate
        def generate(self, replace) -> List[str]: # return var names
            log(f"AssignmentLhs.generate {backend.show(self)}, {replace}")
            dest_vars = []
            for result in self.results:
                dest_vars += result.generate(replace)
            return dest_vars

        @Entity.method(zc.ResultVariableDef) # ResultVariableDef.generate
        def generate(self, replace) -> List[str]:
            log(f"ResultVariableDef.generate {backend.show(self)}, {replace}")
            result_vars = []
            for name in self.names:
                temp_name = backend.add_variable(str(name.name), str(self.type.name))
                result_vars.append(temp_name)
            return result_vars
        
        @Entity.method(zc.ResultVariableRef) # ResultVariableRef.generate
        def generate(self, replace) -> List[str]:
            log(f"ResultVariableRef.generate {backend.show(self)}, {replace}")
            var = backend.map_variable(self.variable)
            return [var]
        
        @Entity.method(zc.FunctionCall) # FunctionCall.generate
        def generate(self, replace, dest_vars: List[str]|None = None) -> List[str]:
            log(f"FunctionCall.generate {backend.show(self)}, {replace}")
            function = self._resolved_functions[0]
            param_names = backend.get_param_names(function)
            if dest_vars is None:
                # todo: make temp vars for result of function call
                dest_vars = backend.add_temp_vars(function.results)
            arg_vars = deepcopy(dest_vars)
            log(f"  param_names: {param_names}")
            for item in self.items:
                if not hasattr(item, "name"):
                    arg_vars += item.generate(replace)
            log(f"  arg_vars: {arg_vars}")
            new_replace = {}
            if len(arg_vars) != len(param_names):
                backend.compiler.error(f"function call {backend.show(self)} has {len(arg_vars)} arguments, but {len(param_names)} parameters")
            for i in range(len(arg_vars)):
                new_replace[param_names[i]] = arg_vars[i]
            new_replace.update(replace)
            log(f"  new_replace: {new_replace}")
            return dest_vars
        
        @Entity.method(zc.FunctionCallArguments) # FunctionCallArguments.generate
        def generate(self, replace) -> List[str]:
            log(f"FunctionCallArguments.generate {backend.show(self)}, {replace}")
            arg_vars = []
            for arg in self.arguments:
                arg_vars += arg.generate(replace)
            return arg_vars
        
        @Entity.method(zc.FunctionCallArgument) # FunctionCallArgument.generate
        def generate(self, replace) -> List[str]:
            log(f"FunctionCallArgument.generate {backend.show(self)}, {replace}")
            return self.value.generate(replace)

        @Entity.method(zc.Constant) # Constant.generate
        def generate(self, replace) -> List[str]:
            log(f"Constant.generate {backend.show(self)}, {replace}")
            return [str(self.value)]
        
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.generate
        def generate(self, replace) -> List[str]:
            log(f"FunctionCallVariable.generate {backend.show(self)}, {replace}")
            return self.variable.generate(replace)
        
        @Entity.method(zc.VariableRef) # VariableRef.generate
        def generate(self, replace) -> List[str]:
            log(f"VariableRef.generate {backend.show(self)}, {replace}")
            return [backend.map_variable(self)]

    #-----------------------------------------------------------------------

    def done(self):
        log(log_grey("-----------------------------------------"))
        log(self.out)
        log_exit("backend")

    def show(self, obj: Any):
        return print_code_formatted(obj, self.compiler.grammar)

    def output(self, s: str):
        self.out += s + "\n"

    def add_variable(self, name: str, type_name: str) -> str:
        temp_name = f"{name}_{self.i_var}"
        self.i_var += 1
        self.output(f"var {type_name} {temp_name}")
        return temp_name
    
    def add_temp_vars(self, results: zc.FunctionResults) -> List[str]:
        temp_vars = []
        for var_def in results.results:
            if isinstance(var_def, zc.ResultVariableDef):
                for name in var_def.names:
                    temp_vars.append(self.add_variable(str(name.name), str(var_def.type.name)))
        return temp_vars

    def map_variable(self, variable: zc.VariableRef, args = {}) -> str:
        id0 = str(variable.variables[0])
        if id0 in args: id0 = args[id0]
        rest = ".".join(str(v) for v in variable.variables[1:])
        if rest != "": id0 += "." + rest
        return id0
    
    def get_param_names(self, function: zc.Function) -> List[str]:
        param_names = []
        for fr in function.results.results:
            for name in fr.names:
                param_names.append(str(name.name))
        for element in function.signature.elements:
            if isinstance(element, zc.FunctionSignatureParams):
                for var_def in element.params:
                    for name in var_def.names:
                        param_names.append(str(name.name))
        return param_names
    
    def get_result_names(self, results: zc.FunctionResults) -> List[str]:
        result_names = []
        for var_def in results.results:
            if isinstance(var_def, zc.ResultVariableDef): 
                for name in var_def.names:
                    result_names.append(str(name.name))
        return result_names

    def function_handle(self, function: zc.Function) -> str:
        out = ""
        for element in function.signature.elements:
            if hasattr(element, "name"):
                out += str(element.name) + "_"
            elif isinstance(element, zc.FunctionSignatureParams):
                for var_def in element.params:
                    type_name = str(var_def.type.name)
                    for name in var_def.names:
                        out += type_name + "_"
        if function.results:
            out += "_"
            for var_def in function.results.results:
                type_name = str(var_def.type.name)
                for name in var_def.names:
                    out += type_name + "_"
        if out.endswith("_"): out = out[:-1]
        return out

    def preamble(self) -> str:
        return log_deindent("""
            # ᕦ(ツ)ᕤ
            # generated by zeta.py

            import numpy as np

            #-------------------------------------------------------
            # concrete types
                  
            _u8 = np.uint8
            _u16 = np.uint16
            _u32 = np.uint32
            _u64 = np.uint64
            _i8 = np.int8
            _i16 = np.int16
            _i32 = np.int32
            _i64 = np.int64
            _f32 = np.float32
            _f64 = np.float64
                            
            #-------------------------------------------------------
            # concrete functions
                            
            def _add_f32_f32__f32(a: _f32, b: _f32) -> _f32: return a + b
            def _sub_f32_f32__f32(a: _f32, b: _f32) -> _f32: return a - b
            def _mul_f32_f32__f32(a: _f32, b: _f32) -> _f32: return a * b
            def _div_f32_f32__f32(a: _f32, b: _f32) -> _f32: return a / b
            def _sqrt_f32__f32(a: _f32) -> _f32: return np.sqrt(a)

            def _add_i32_i32__i32(a: _i32, b: _i32) -> _i32: return a + b
            def _sub_i32_i32__i32(a: _i32, b: _i32) -> _i32: return a - b
            def _mul_i32_i32__i32(a: _i32, b: _i32) -> _i32: return a * b
            def _div_i32_i32__i32(a: _i32, b: _i32) -> _i32: return a / b
            def _sqrt_i32__i32(a: _i32) -> _i32: return np.sqrt(a)
        
            
""")