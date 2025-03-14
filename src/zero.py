# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from compiler import *
from vm import *
from backend import *
from copy import deepcopy
import zero_classes as zc
import re

#--------------------------------------------------------------------------------------------------
# main

s_test_features = [
"""
feature Program
    char out$
""",
"""
feature Hello extends Program
    on hello()
        out$ << "ᕦ(ツ)ᕤ\\n"
    replace run()
        hello()
""",
"""
feature Goodbye extends Hello
    on bye()
        out$ << "kthxbye."
    after hello()
        bye()
""",
"""
feature Math extends Program
    type int > i8, i16, i32, i64
    type float > f32, f64
    type number > int, float
    on (number n) = (number a) + (number b)
        n = add(a, b)
    on (number n) = (number a) - (number b)
        n = sub(a, b)
    on (number n) = (number a) * (number b)
        n = mul(a, b)
    on (number n) = (number a) / (number b)
        n = div(a, b)
""",
"""
feature VectorMath extends Math
    type vector | vec = 
        number x, y, z = 0
    on (vec v) = (vec a) + (vec b)
        v = vec(a.x + b.x, a.y + b.y, a.z + b.z)
    on (vec v) = (vec a) - (vec b)
        v = vec(a.x - b.x, a.y - b.y, a.z - b.z)
    on (vec v) = (vec a) * (number n)
        v = vec(a.x * n, a.y * n, a.z * n)
    on (vec r) = (vec v) / (number n)
        r = vec(v.x / n, v.y / n, v.z / n)
    on (number n) = (vec a) dot (vec b)
        n = a.x * b.x + a.y * b.y + a.z * b.z
    on (number l) = length(vec v)
        l = sqrt(v dot v)
    on (number l) = distance between (vec a) and (vec b)
        l = length(a - b)
    on (vec n) = normalise(vec v)
        n = v / length(v)
    replace run()
        vec a = vec(1, 2, 3)
        vec b = vec(4, 5, 6)
        number d = distance between a and b
""",
"""
feature Backend
    type u8, u16, u32, u64
    type i8, i16, i32, i64
    type f16, f32, f64
    type char > u8
    type string = char$
    on (i32 r) = add(i32 a, b) emit
    on (i32 r) = sub(i32 a, b) emit
    on (i32 r) = mul(i32 a, b) emit
    on (i32 r) = div(i32 a, b) emit
    on (i32 r) = sqrt(i32 n) emit
    on (f32 r) = add(f32 a, b) emit
    on (f32 r) = sub(f32 a, b) emit
    on (f32 r) = mul(f32 a, b) emit
    on (f32 r) = div(f32 a, b) emit
    on (f32 r) = sqrt(f32 n) emit
""",
"""
feature ConcreteMath extends Math
    type bit = 0 | 1
    type uint(n) = 
        bit[n] value
    u8 = uint(8)
    u16 = uint(16)
    u32 = uint(32)
    u64 = uint(64)
    type int(n) =
        bit[n] value
    i8 = int(8)
    i16 = int(16)
    i32 = int(32)
    i64 = int(64)
    type float(e, m) =
        bit sign
        u(e) exponent | exp
        u(m) mantissa | man
    type f16 = float(5, 10)
    type f32 = float(8, 23)
    type f64 = float(11, 52)
"""
]

@this_is_the_test
def test_zero():
    log("test_zero")

    compiler = Compiler(zc)
    compiler.add_modules([module_Features(), module_Expressions(), module_Variables(), module_Types(), module_Functions(), module_Tests()])
    compiler.set_concrete_types({ "number": "f32", "int": "i32"})
    
    test_verbose(False)
    log_max_depth(12)

    compiler.setup()
    #context = Context("test_vector_math", ["Program", "Math", "VectorMath", "Backend"])
    context = Context("test_hello", ["Program", "Hello", "Backend"])
    program = compiler.compile(s_test_features, context)
    if not program.is_ok():
        #log_clear()
        #log(program.show_report())
        for stage in program.reports[-1:]:
            log(visual_report_from_stage(stage, program.code))
        log_exit("program is not ok")
    log("\n----------------------------------------------")
    #log("after optimisation:")
    #log(program.assembly)
    #log_exit("")
    #backend = PythonBackend("test/test.py")
    riscv_backend = CPUBackend("test/riscv/test.*", RISCV32(), debug=False, restrict=True)
    arm_backend = CPUBackend("test/arm/test.*", ARM64(), debug=True, restrict=True)
    #log_clear()
    riscv_backend.generate(program.assembly)
    riscv_log = log_get()
    #log_clear()
    #arm_backend.generate(program.assembly)
    #arm_log = log_get()
   # log_clear()
    #display_logs([riscv_log, arm_log])
    log_flush()
    riscv_results = riscv_backend.run()
    #arm_results = arm_backend.run()

def display_logs(logs: List[str]):
    col_width = 64
    lines = [l.split("\n") for l in logs]
    max_len = max(len(l) for l in lines)
    for i in range(max_len):
        out = ""
        for line_list in lines:
            if i < len(line_list):
                out += line_list[i]
            out += (" " * (col_width - len(log_strip(out))))
        log(out)

#--------------------------------------------------------------------------------------------------
# print ast as nicely formatted code

def test_print_code(ast):
    log_clear()
    
    test("print_formatted", print_code_formatted(ast), """
        feature Program
            type string | str = char$
            string out$
        feature Hello extends Program
            on hello ( string name )
                string message = "hello, \\(name)!"
                out$ << message
            replace run ( )
                hello ( "world" )
        feature Goodbye extends Hello
            on bye ( )
                out$ << "kthxbye."
            after hello ( string name )
                bye ( )
        context MyContext = Program, Hello
         """)

#--------------------------------------------------------------------------------------------------
# features and contexts 

class module_Features(LanguageModule):
    def setup_grammar(self, compiler: Compiler):
        compiler.grammar.add("""
            NameDef := name:<identifier> ("|" alias:<identifier>)?
            FeatureDef := "feature" NameDef ("extends" parent:FeatureDef&)? "{" components:Component*; "}"
            Component :=
            Program := components:(FeatureDef)+;
            """)
        
    def setup_scope(self, compiler: Compiler):
        @Entity.method(zc.FeatureDef) # FeatureDef.get_scope
        def get_scope(self): return self

        @Entity.method(zc.FeatureDef) # FeatureDef.can_see_scope
        def can_see_scope(self, scope, read_only: bool):
            if not isinstance(scope, zc.FeatureDef): return False
            if read_only: return True
            featureDef = self
            safe_count = 100
            while True and safe_count > 0:
                parent = featureDef.parent
                if parent == None: return False
                if isinstance(parent, Lex):
                    parent = compiler.find_symbol(parent, zc.FeatureDef, None, raise_errors=False)
                    if parent == None: return False # we'll get a resolve error later
                if parent == scope: return True
                featureDef = parent
                safe_count -= 1
            if safe_count <=0: raise Exception("safe-count exceeded")
            return False

    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.FeatureDef) # FeatureDef.add_symbols
        def add_symbols(self, scope):
            compiler.add_symbol(self.name, self, scope, alias=self.alias)

        @Entity.method(Lex) # Lex.resolve
        def resolve(self, scope, type_name):
            cls = Entity.get_class(type_name)
            return compiler.find_symbol(self, cls, scope, raise_errors=True, read_only=True)

    def setup_generate(self, compiler: Compiler):
        codegen = compiler.codegen
        @Entity.method(zc.Program) # Program.generate
        def generate(self):
            run_func = codegen.find_entity("run", zc.Function)
            if run_func == None:
                compiler.error(self, "no run function found")
                return
            codegen.reset()
            run_func.generate({}, [])

    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("feature_0", parse_code("", "FeatureDef", grammar), """
            FeatureDef
                premature end: expected 'feature'
                name: str = None
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = None
            """)
        
        test("feature_1", parse_code("feature", "FeatureDef", grammar), """
            FeatureDef
                premature end: expected NameDef
                name: str = None
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = None
            """)

        test("feature_2", parse_code("feature MyFeature", "FeatureDef", grammar), """
            FeatureDef
                premature end: expected '{'
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = None
            """)

        test("feature_3", parse_code("feature MyFeature extends", "FeatureDef", grammar), """
            FeatureDef
                :...:19 mismatch: expected '{'
                name: str = MyFeature
                alias: str = None
                parent => premature end: expected parent:FeatureDef&
                components: List[Component] = None
            """)
        
        test("feature_4", parse_code("feature MyFeature extends Another", "FeatureDef", grammar), """
            FeatureDef
                premature end: expected '{'
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef => Another
                components: List[Component] = None    
            """)
        
        test("feature_5", parse_code("feature MyFeature {}", "FeatureDef", grammar), """
            FeatureDef
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = []
            """)
        
        test("feature_6", parse_code("feature MyFeature extends Another {}", "FeatureDef", grammar), """
            FeatureDef
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef => Another
                components: List[Component] = []
            """)


#--------------------------------------------------------------------------------------------------
# Expressions

class module_Expressions(LanguageModule):
    def setup_grammar(self, compiler: Compiler):
        compiler.grammar.add("""
            Expression :=
            Constant < Expression := value:(<number> | <string>)
            VariableRef < Expression := variables:<identifier>+.
            Bracketed < Expression := "(" expression:Expression ")"
            FunctionCall < Expression := items:FunctionCallItem+
            FunctionCallItem :=
            FunctionCallConstant < FunctionCallItem := constant:Constant
            FunctionCallOperator < FunctionCallItem := name:<operator>
            FunctionCallWord < FunctionCallItem := name:<identifier>
            FunctionCallVariable < FunctionCallItem := variable:VariableRef
            FunctionCallArguments < FunctionCallItem := "(" arguments:FunctionCallArgument*, ")"
            FunctionCallArgument := (argument:Variable& "=")? value:Expression
                    """)
        
    def setup_validate(self, compiler: Compiler):
        @Entity.method(zc.FunctionCall) # FunctionCall.validate
        def validate(self) -> str: # function call must have at least one bracketed term or operator
            n_bracketed = 0; n_operators = 0; n_words = 0
            for item in self.items:
                if isinstance(item, zc.FunctionCallArguments): n_bracketed += 1
                elif isinstance(item, zc.FunctionCallOperator): n_operators += 1
                elif isinstance(item, zc.FunctionCallWord): n_words += 1
            if n_bracketed == 0 and n_operators == 0 and (n_words < 2): 
                return "function call must have at least one argument or operation, or be two or more words"
            return ""
    
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.validate
        def validate(self) -> str:
            if len(self.variable.variables) < 2: return "FunctionCallVariable must have at least two variables"
            return ""
        
    def setup_naming(self, compiler: Compiler):
        @Entity.method(zc.VariableRef) # VariableRef.get_name
        def get_name(self) -> str:
            if self.variables:
                return ".".join(str(v) for v in self.variables)
            return ""
        @Entity.method(zc.FunctionCall) # FunctionCall.get_name
        def get_name(self) -> str:
            return log_strip(print_code_formatted(self, compiler.grammar))
            
    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.VariableRef) # VariableRef.resolve
        def resolve(self, scope, type_name):
            self._resolved_vars = self.resolve_names(scope, read_only=True)
            return self
        
        @Entity.method(zc.VariableRef) # VariableRef.resolve_names
        def resolve_names(self, scope, read_only) -> List[zc.Variable]:
            resolved_vars = []
            for var in self.variables: # all Lex at this point
                resolved_var = compiler.find_symbol(var, zc.Variable, scope, raise_errors=False, read_only=read_only)
                if resolved_var == None:
                    compiler.error(var, f"variable not found: {var}")
                    resolved_vars.append(var)
                    return self
                else:
                    compiler.report(var, f"{resolved_var} in {scope}")
                    resolved_vars.append(resolved_var)
                    scope = resolved_var.type
                    if isinstance(scope, Lex): compiler.error(var, f"scope is Lex: {scope}")
            return resolved_vars

        @Entity.method(zc.FunctionCall) #FunctionCall.resolve
        def resolve(self, scope, type_name):
            for i, item in enumerate(self.items):
                if isinstance(item, zc.FunctionCallWord):
                    var = compiler.find_symbol(item.name, zc.Variable, scope, raise_errors=False, read_only = True)
                    if var:
                        var_ref = zc.VariableRef(variables=[item.name])
                        var_ref._resolved_vars = [var]
                        self.items[i] = zc.FunctionCallVariable(variable=var_ref)
            return self

        @Entity.method(zc.FunctionCall) # FunctionCall.embracket
        def embracket(self):
            i_operator = find_lowest_ranked_operator(self.items)
            if i_operator == -1: return self
            before = self.items[0:i_operator]
            if len(before) > 1: before = [zc.FunctionCall(items=before)]
            after = self.items[i_operator+1:]
            if len(after) > 1: after = [zc.FunctionCall(items=after)]
            compiler.report(self.items[i_operator].name, f"split: left = \"{log_strip(print_code_formatted(before[0], compiler.grammar))}\", right = \"{log_strip(print_code_formatted(after[0], compiler.grammar))}\"")
            self.items = before + [self.items[i_operator]] + after
            return self
    
        def find_lowest_ranked_operator(items):
            i_found = -1
            lowest_rank = 10240
            for i, item in enumerate(items):
                if isinstance(item, zc.FunctionCallOperator) and item.name != ".":
                    rank = item.name.rank
                    if rank <= lowest_rank:
                        lowest_rank = rank
                        i_found = i
            return i_found
            
    def setup_check_types(self, compiler: Compiler):
        @Entity.method(zc.Constant) # Constant.check_type
        def check_type(self, scope):
            type_name = "number" if self.value.type=="number" else "string"
            self._type = compiler.find_symbol(type_name, zc.Type, scope, raise_errors=True, read_only=True)
            compiler.report(self.value, f"{self._type}")
        
        @Entity.method(zc.VariableRef) # VariableRef.check_type
        def check_type(self, scope):
            if not hasattr(self, "_resolved_vars"):
                self._type = None
                compiler.error(self.variables[0], f"variable not resolved")
            else:
                for i, var in enumerate(self._resolved_vars):
                    compiler.report(self.variables[i], f"{var.type}")
                self._type = self._resolved_vars[-1].type
        
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.check_type
        def check_type(self, scope):
            self._type = self.variable._type
            if self._type: compiler.report(get_first_lex(self.variable), f"{self._type}")
            
        @Entity.method(zc.FunctionCall) # FunctionCall.check_type
        def check_type(self, scope):
            cf= log_strip(print_code_formatted(self, compiler.grammar))
            self._resolved_functions = []
            functions = find_functions(self, scope)
            self._resolved_functions = functions
            for f in functions:
                if not hasattr(f, "_type"): f.check_type(scope)
            if len(functions) == 0:
                compiler.error(self, f"no functions found for {cf} in {scope}")
                self._type = None
            elif len(functions) > 1:
                self._type = zc.MaybeTypes([f._type for f in functions])
            else:
                self._type = functions[0]._type
            first_item = next((item for item in self.items if hasattr(item, "name")), None)
            compiler.report(first_item.name, f"{self._type}")

        @Entity.method(zc.Function) # Function.check_type
        def check_type(self, scope):
            result_types = []
            if self.results != None:
                for frv in self.results.results:
                    result_types.append(frv.type)
            if len(result_types) == 0: self._type = None
            elif len(result_types) == 1: self._type = result_types[0]
            else: self._type = zc.MultipleTypes(result_types)

        @log_suppress
        def find_functions(fc: zc.FunctionCall, scope) -> List[zc.Function]: 
            if fc._resolved_functions: return fc._resolved_functions
            short_sig = find_short_sig(fc)
            location = find_location(fc)
            fc_types = get_param_types(fc)
            for param_type in fc_types:
                if isinstance(param_type, Lex):
                    compiler.error(get_first_lex(fc), "fc_types contains a Lex type.")
                    break
            functions = [f.element for f in compiler.cp.st.find(short_sig, zc.Function, scope, read_only=True)]
            fn_types = [get_sig_param_types(f.signature) for f in functions]
            distances = [get_distances(fc_types,fn_types[i]) for i in range(len(fn_types))]
            i_best = find_best_positive_distance(distances)
            if i_best is not None: return [functions[i_best]]
            # filter out functions with None in their distances
            filtered_functions = []
            for i, dist_list in enumerate(distances):
                if None not in dist_list: filtered_functions.append(functions[i])
            if len(filtered_functions) == 0:
                compiler.error(fc, f"no function found for {log_strip(print_code_formatted(fc, compiler.grammar))} in {scope}")
                return []
            fc._constraints = fn_types # dunno what this is really for, but we'll find out later
            return filtered_functions
        
        def get_param_types(fc: zc.FunctionCall) -> List[zc.Type]:
            out = []
            for item in fc.items:
                if hasattr(item, "_type"):
                    out.append(item._type)
                elif isinstance(item, zc.FunctionCallArguments):
                    for arg in item.arguments:
                        if arg.value:
                            out.append(arg.value._type)
            return out
        
        def get_sig_param_types(signature: zc.FunctionSignature) -> List[zc.Type]:
            out = []
            for item in signature.elements:
                if isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        param_type = param.type
                        if isinstance(param_type, Lex):
                            param_type = compiler.find_symbol(param_type, zc.Type, None, raise_errors=False)
                        for param_name in param.names:
                            out.append(param_type)
            return out
        
        def find_short_sig(fc: zc.FunctionCall) -> str:
            out = ""
            for item in fc.items:
                if isinstance(item, zc.FunctionCallArguments):
                    out += "◦" * len(item.arguments)
                elif hasattr(item, "_type"):
                    out += "◦"
                elif hasattr(item, "name"):
                    out += str(item.name)
            return out
        
        def find_location(fc: zc.FunctionCall) -> str:
            for item in fc.items:
                if hasattr(item, "name"):
                    return item.name.location()
            return ""
        
        def get_distances(sig_types, fc_types):
            distances = []
            for i, t in enumerate(sig_types):
                distance = t.find_relationship(fc_types[i])
                distances.append(distance)
            return distances

        def find_best_positive_distance(distances: List[List[int]]) -> int|None:
            i_best = None
            best_sum = 102400
            for i, dist_list in enumerate(distances):
                if None in dist_list: continue
                if all(d >= 0 for d in dist_list):
                    sum_distances = sum(dist_list)
                    if sum_distances < best_sum:
                        best_sum = sum_distances
                        i_best = i
            return i_best

    def setup_generate(self, compiler: Compiler):
        codegen = compiler.codegen

        @Entity.method(zc.FunctionCall)  # FunctionCall.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            function = self._resolved_functions[0]
            result_vars = replace["_results"] if "_results" in replace else []
            replace.pop("_results", None)
            if len(result_vars) == 0:
                result_vars = function.make_temp_vars()
            args = []
            first_lex = None
            for item in self.items:
                if not hasattr(item, "name"):
                    args += item.generate(replace, source_loc)
                elif first_lex is None:
                    first_lex = item
            function = self._resolved_functions[0]
            params = function.get_params()
            fn_replace = {}
            for p, a in zip(params, args):
                a = codegen.try_replace(a, replace)
                fn_replace[p] = a
            fn_replace["_results"] = result_vars
            results = function.generate(fn_replace, source_loc + [first_lex])
            return result_vars
        
        @Entity.method(zc.FunctionCallArguments) # FunctionCallArguments.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            args = []
            for item in self.arguments:
                args += item.generate(replace, source_loc)
            return args
        
        @Entity.method(zc.FunctionCallArgument) # FunctionCallArgument.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            return self.value.generate(replace, source_loc)
        
        @Entity.method(zc.Constant) # Constant.generate
        def generate(self, replace: Dict, source_loc: List[Lex]) -> List[str]:
            return [str(self.value)]
        
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.generate
        def generate(self, replace: Dict, source_loc: List[Lex]) -> List[str]:
            return self.variable.generate(replace, source_loc)
        
        @Entity.method(zc.VariableRef)   # VariableRef.generate
        def generate(self, replace: Dict, source_loc: List[Lex]) -> List[str]:
            full = ".".join(str(v) for v in self.variables)
            return [codegen.try_replace(full, replace)]

    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("expression_0", parse_code("1", "Expression", grammar), """
            Constant
                value: str = 1
            """)

        test("expression_1", parse_code("a", "Expression", grammar), """
            VariableRef
                variables: List[str]
                    a
            """)
        
        test("expression_1a", parse_code("a.b", "Expression", grammar), """
            VariableRef
                variables: List[str]
                    a
                    b
            """)

        test("expression_2", parse_code("a + b", "Expression", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        name: str = a
                    FunctionCallOperator
                        name: str = +
                    FunctionCallWord
                        name: str = b
            """)
        
        test("expression_2a", parse_code("v.x + v.y", "Expression", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallVariable
                        variable: VariableRef
                            VariableRef
                                variables: List[str]
                                    v
                                    x
                    FunctionCallOperator
                        name: str = +
                    FunctionCallVariable
                        variable: VariableRef
                            VariableRef
                                variables: List[str]
                                    v
                                    y
                        """)
        
        test("parameter_0", parse_code("a = 1", "FunctionCallArgument", grammar), """
                FunctionCallArgument
                    argument: Variable => a
                    value: Expression
                        Constant
                            value: str = 1
                """)

        test("parameter_1", parse_code("a + b", "FunctionCallArgument", grammar), """
            FunctionCallArgument
                argument: Variable = None
                value: Expression
                    FunctionCall
                        items: List[FunctionCallItem]
                            FunctionCallWord
                                name: str = a
                            FunctionCallOperator
                                name: str = +
                            FunctionCallWord
                                name: str = b
            """)

        test("brackets_0", parse_code("(a + b)", "Bracketed", grammar), """
            Bracketed
                expression: Expression
                    FunctionCall
                        items: List[FunctionCallItem]
                            FunctionCallWord
                                name: str = a
                            FunctionCallOperator
                                name: str = +
                            FunctionCallWord
                                name: str = b
            """)
        
        test("brackets_1", parse_code("(v = a + b)", "Expression", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallArguments
                        arguments: List[FunctionCallArgument]
                            FunctionCallArgument
                                argument: Variable => v
                                value: Expression
                                    FunctionCall
                                        items: List[FunctionCallItem]
                                            FunctionCallWord
                                                name: str = a
                                            FunctionCallOperator
                                                name: str = +
                                            FunctionCallWord
                                                name: str = b
            """)
        
        test("expression_3", parse_code("2 - c", "Expression", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallConstant
                        constant: Constant
                            Constant
                                value: str = 2
                    FunctionCallOperator
                        name: str = -
                    FunctionCallWord
                        name: str = c
        """)

        test("expression_4", parse_code("a + (2 - c)", "Expression", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        name: str = a
                    FunctionCallOperator
                        name: str = +
                    FunctionCallArguments
                        arguments: List[FunctionCallArgument]
                            FunctionCallArgument
                                argument: Variable = None
                                value: Expression
                                    FunctionCall
                                        items: List[FunctionCallItem]
                                            FunctionCallConstant
                                                constant: Constant
                                                    Constant
                                                        value: str = 2
                                            FunctionCallOperator
                                                name: str = -
                                            FunctionCallWord
                                                name: str = c
        """)

        test("expression_5", parse_code("hello(name = \"world\")", "Expression", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        name: str = hello
                    FunctionCallArguments
                        arguments: List[FunctionCallArgument]
                            FunctionCallArgument
                                argument: Variable => name
                                value: Expression
                                    Constant
                                        value: str = "world"
            """)
        
        test("expression_6", parse_code("bye()", "FunctionCall", grammar), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        name: str = bye
                    FunctionCallArguments
                        arguments: List[FunctionCallArgument] = []
            """)
    
#--------------------------------------------------------------------------------------------------
# variables

class module_Variables(LanguageModule):
    def setup_grammar(self, compiler: Compiler):
        compiler.grammar.add("""
            Variable := type:Type& NameDef "=" value:Expression
            VariableDef < Component := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&)) ("=" value:Expression)?
        """)

    def setup_scope(self, compiler: Compiler):
        @Entity.method(zc.Variable) # Variable.get_scope
        def get_scope(self) -> str: 
            return self.type if isinstance(self.type, zc.Type) else None

    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.VariableDef) # VariableDef.add_symbols
        def add_symbols(self, scope):
            self._defined_vars = []
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias, value=self.value)
                compiler.add_symbol(name.name, var, scope, alias=name.alias)
                self._defined_vars.append(var)

    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("variable_0", parse_code("int a", "VariableDef", grammar), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression = None
            """)
        
        test("variable_1", parse_code("a : int", "VariableDef", grammar), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression = None
            """)
        
        test("variable_2", parse_code("a : int = 0", "VariableDef", grammar), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression
                    Constant
                        value: str = 0
            """) 
        
        test("variable_3", parse_code("int a = 0", "VariableDef", grammar), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression
                    Constant
                        value: str = 0
            """)
        
        test("variable_4", parse_code("r|red, g|green, b|blue: number = 0", "VariableDef", grammar), """
            VariableDef
                type: Type => number
                names: List[NameDef]
                    NameDef
                        name: str = r
                        alias: str = red
                    NameDef
                        name: str = g
                        alias: str = green
                    NameDef
                        name: str = b
                        alias: str = blue
                value: Expression
                    Constant
                        value: str = 0
            """)
        
        test("variable_5", parse_code("int x,y = 0", "VariableDef", grammar), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = x
                        alias: str = None
                    NameDef
                        name: str = y
                        alias: str = None
                value: Expression
                    Constant
                        value: str = 0
            """)

#--------------------------------------------------------------------------------------------------
# types

class module_Types(LanguageModule):
    def setup_grammar(self, compiler: Compiler):
        compiler.grammar.add("""
            Type := NameDef properties:Variable* parents:Type&* children:Type&* options:<identifier>+
            TypeDef < Component := "type" names:NameDef+, rhs:TypeRhs?
            TypeRhs :=
            TypeAlias < TypeRhs := "=" type:Type&
            StructDef < TypeRhs := "=" "{" properties:VariableDef+; "}"
            TypeParentDef < TypeRhs := "<" parents:Type&+,
            TypeChildrenDef < TypeRhs := ">" children:Type&+,
            TypeEnumDef < TypeRhs := "=" options:EnumOption+|
            EnumOption := 
            EnumOptionId < EnumOption := val:<identifier>
            EnumOptionNumber < EnumOption := val:<number>
            MaybeTypes < Type := types:Type&*
            MultipleTypes < Type := types:Type&*
        """)

    def setup_validate(self, compiler: Compiler):
        @Entity.method(zc.TypeEnumDef)             # TypeEnumDef.validate
        def validate(self) -> str: 
            return "" if len(self.options) > 1 else "enum must have at least two options"
        @Entity.method(zc.TypeAlias) # TypeAlias.validate
        def validate(self) -> str:
            if not str(self.type).endswith("$"): return ""
            rank = str(self.type).count('$')
            l = self.type
            self.type = Lex(l.source, l.pos, l.val[:-rank], l.type)
            self.rank = Lex(l.source, l.pos + len(l.val) - rank, l.val[-rank:], l.type)
            return ""
        
    def setup_naming(self, compiler: Compiler):
        @Entity.method(zc.MaybeTypes)
        def get_name(self) -> str:
            return "|".join(str(t.get_name()) for t in self.types)
        @Entity.method(zc.MultipleTypes)
        def get_name(self) -> str:
            return ",".join(str(t.get_name()) for t in self.types)
        
    def setup_scope(self, compiler: Compiler):
        @Entity.method(zc.TypeDef) #TypeDef.get_scope
        def get_scope(self) -> str:
            return self._resolved_types[0] if hasattr(self, "_resolved_types") else None

        @Entity.method(zc.Type) #Type.get_scope
        def get_scope(self) -> str: return self

        @Entity.method(zc.Type) #Type.can_see_scope
        def can_see_scope(self, scope, read_only):
            if read_only and isinstance(scope, zc.FeatureDef):
                return True
            return self == scope

    def setup_constructors(self, compiler: Compiler):
        @Entity.method(zc.TypeDef)
        def make_constructor(self):
            if not isinstance(self.rhs, zc.StructDef): return
            name = self.names[0].name
            result_var = "_" + str(name)[0].lower()
            code = f"on ({name} {result_var}) = {name}("
            for prop in self.rhs.properties:
                code += f"{prop.type} "
                for var in prop.names: code += f"{var.name}, "
                if code.endswith(", "): code = code[:-2]
                if prop.value: code += f" = {print_code_formatted(prop.value, compiler.grammar)}"
            code += ") {"
            for prop in self.rhs.properties:
                for var in prop.names:
                    code += f"{result_var}.{var.name} = {var.name}; "
            code += "}"
            compiler.report(self.names[0].name, f"make_constructor:{code}")
            constructor = parse_simple(code, "FunctionDef", compiler.grammar)
            constructor._is_constructor_for_typedef = self
            self._constructor = constructor

    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.TypeDef) # TypeDef.add_symbols
        def add_symbols(self, scope):
            name = self.names[0]
            for name in self.names:
                existing_type_objects = compiler.cp.st.find(name.name, zc.Type, scope, read_only=False)
                if len(existing_type_objects) == 0:
                    self._resolved_types = []
                    type_object = zc.Type(name=name.name, alias=name.alias)
                    compiler.add_symbol(name.name, type_object, scope, alias=name.alias)
                    self._resolved_types.append(type_object)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties = self.rhs.properties
                        compiler.add_symbol(name.name, self._constructor, scope, alias=name.alias)
                    elif isinstance(self.rhs, zc.TypeAlias):
                        type_object.alias = f"{self.rhs.type}{self.rhs.rank}"
                elif len(existing_type_objects) == 1:
                    self._resolved_types.append(existing_type_objects[0].element)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties += self.rhs.properties
                else:
                    raise Exception(f"symbol clash in TypeDef: {self.name} => {existing_type_objects}")
                
        @Entity.method(zc.TypeDef)
        def resolve(self, scope, type_name):
            first_lex = get_first_lex(self)
            if isinstance(self.rhs, zc.TypeParentDef):
                #log(" is parent")
                for parent in self.rhs.parents:
                    assert_parent_type(self._resolved_types[0], parent)
                    compiler.report(first_lex, f"asserting that '{self._resolved_types[0].name}' is a child of '{parent.name}'")
            elif isinstance(self.rhs, zc.TypeChildrenDef):
                #log(" is children")
                for child in self.rhs.children:
                    assert_parent_type(child, self._resolved_types[0])
                    compiler.report(first_lex, f"asserting that '{child.name}' is a child of '{self._resolved_types[0].name}'")
        
        def assert_parent_type(type: zc.Type, parent: zc.Type):
            if isinstance(type, Lex): raise Exception(f"type {type} is lex")
            if isinstance(type, str) or isinstance(parent, str): return
            if type.parents == None: type.parents = []
            if parent not in type.parents: type.parents.append(parent)
            if parent.children == None: parent.children = []
            if type not in parent.children: parent.children.append(type)

    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("type_0", parse_code("type vec | vector", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = vec
                        alias: str = vector
                rhs: TypeRhs = None
            """)
    
        test("type_1", parse_code("type vec | vector = { x, y, z: number =0 }", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = vec
                        alias: str = vector
                rhs: TypeRhs
                    StructDef
                        properties: List[VariableDef]
                            VariableDef
                                type: Type => number
                                names: List[NameDef]
                                    NameDef
                                        name: str = x
                                        alias: str = None
                                    NameDef
                                        name: str = y
                                        alias: str = None
                                    NameDef
                                        name: str = z
                                        alias: str = None
                                value: Expression
                                    Constant
                                        value: str = 0
            """)
        
        test("type_2", parse_code("type int > i8, i16", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = int
                        alias: str = None
                rhs: TypeRhs
                    TypeChildrenDef
                        children => [i8, i16]
            """)
        
        test("type_3", parse_code("type offset < vector", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = offset
                        alias: str = None
                rhs: TypeRhs
                    TypeParentDef
                        parents => [vector]
            """)
        
        test("type_4", parse_code("type evil = no | yes | maybe", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = evil
                        alias: str = None
                rhs: TypeRhs
                    TypeEnumDef
                        options: List[EnumOption]
                            EnumOptionId
                                val: str = no
                            EnumOptionId
                                val: str = yes
                            EnumOptionId
                                val: str = maybe
            """)
        test("type_4a", parse_code("type bit = 0 | 1", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = bit
                        alias: str = None
                rhs: TypeRhs
                    TypeEnumDef
                        options: List[EnumOption]
                            EnumOptionNumber
                                val: str = 0
                            EnumOptionNumber
                                val: str = 1
            """)
        
        test("type_5", parse_code("type str | string = char$$$", "TypeDef", grammar), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = str
                        alias: str = string
                rhs: TypeRhs
                    TypeAlias
                        type: Type => char
                        rank: None => $$$
            """)

#--------------------------------------------------------------------------------------------------
# functions

class module_Functions(LanguageModule):
    def setup_grammar(self, compiler: Compiler):
        compiler.grammar.add("""
            Function := "_function" results:FunctionResults signature:FunctionSignature body:FunctionBody
            FunctionDef < Component := FunctionModifier results:FunctionResults? signature:FunctionSignature body:FunctionBody
            FunctionModifier := modifier:("on" | "before" | "after" | "replace")
            FunctionResults := "(" results:ResultVariableDef+, ")" assign_op:("=" | "<<")
            FunctionSignature := elements:FunctionSignatureElement+
            FunctionSignatureElement :=
            FunctionSignatureWord < FunctionSignatureElement := name:(<identifier> | <operator>)
            FunctionSignatureParams < FunctionSignatureElement := "(" params:VariableDef*, ")"
            FunctionBody := 
            FunctionStatements < FunctionBody := "{" statements:Statement*; "}"
            EmitFunctionBody < FunctionBody := "emit"
            Statement := 
            Assignment < Statement := (lhs:AssignmentLhs)? rhs:Expression
            AssignmentLhs := results:ResultVariable+, assign_op:("=" | "<<") 
            ResultVariable :=
            ResultVariableDef < ResultVariable := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&))
            ResultVariableRef < ResultVariable := variable:VariableRef
            CompositeFunction < Statement:=
            SingleFunctionDef < CompositeFunction := func_def:FunctionDef&
            SequenceFunctionDef < CompositeFunction := "seq" "(" comps:CompositeFunction+, ")"
            ParallelFunctionDef < CompositeFunction := "par" "(" comps:CompositeFunction+, ")"
        """)

    def setup_naming(self, compiler: Compiler):
        @Entity.method(zc.FunctionDef)  # FunctionDef.get_name
        def get_name(self) -> str:
            name= self.signature.typed_handle() if self.signature else ".."
            if hasattr(self, "_owner") and self._owner: name = str(self._owner.get_name()) + "." + name

            return re.sub(r'Type\((.*?)\)', r'\1', name)
        
        @Entity.method(zc.Function)
        def get_name(self) -> str:
            return self.signature.typed_handle() if self.signature else ".."
        
        @Entity.method(zc.FunctionSignature)
        def typed_handle(self) -> str:
            out = ""
            for e in self.elements:
                if hasattr(e, "name"): out += str(e.name)
                elif isinstance(e, zc.FunctionSignatureParams):
                    out += "("
                    for p in e.params:
                        for v in p.names:
                            out += str(p.type) + ", "
                    if out.endswith(", "): out = out[:-2]
                    out += ")"
            return out
        
    def setup_scope(self, compiler: Compiler):
        @Entity.method(zc.FunctionDef) # FunctionDef.get_scope
        def get_scope(self): return self
        @Entity.method(zc.Function) # Function.get_scope
        def get_scope(self): return self

        @Entity.method(zc.FunctionDef)
        def can_see_scope(self, scope, read_only):
            if self == scope: return True
            if not isinstance(scope, zc.FeatureDef): return False
            if self._owner == scope: return True
            if self._owner == scope: return True
            if self._owner.can_see_scope(scope, read_only): return True
            return False
        
    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.ResultVariableDef) # ResultVariableDef.add_symbols
        def add_symbols(self, scope):
            self._defined_vars = []
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias)
                compiler.add_symbol(name.name, var, scope, alias=name.alias)
                self._defined_vars.append(var)

        @Entity.method(zc.FunctionDef) # FunctionDef.add_symbols
        def add_symbols(self, scope):
            long_handle = typed_handle(self)
            short_handle = untyped_handle(self)
            compiler.cp.st.add(long_handle, self, scope)
            compiler.cp.st.add(short_handle, self, scope)
            first_lex = get_first_name(self.signature)
            compiler.report(first_lex, f"\"{long_handle}\" => {self} in {scope}")
            compiler.report(first_lex, f"\"{short_handle}\" => {self} in {scope}")
            self._owner = scope
            function = compiler.find_symbol(long_handle, zc.Function, None, raise_errors=False, read_only=False)
            if function == None:
                function = make_function(self, compiler.cp.st)
                self._function = function
                compiler.cp.st.add(long_handle, function, None)
                compiler.cp.st.add(short_handle, function, None)
                compiler.report(first_lex, f"\"{long_handle}\" => {function} in {scope}")
                compiler.report(first_lex, f"\"{short_handle}\" => {function} in {scope}")
                if hasattr(self, "_is_constructor_for_typedef"):    # add alias handle
                    add_alias_symbol(self, function, compiler.cp.st, long_handle, short_handle)
            else:
                modify_function(self, function, compiler.cp.st)

        @Entity.method(zc.ResultVariableRef)  # ResultVariableRef.resolve
        def resolve(self, scope, type_name):
            self.variable._resolved_vars = self.variable.resolve_names(scope, read_only=False)

        def get_first_name(signature: zc.FunctionSignature) -> str:
            for e in signature.elements:
                if hasattr(e, "name"): return e.name
            return None

        def typed_handle(func_def) -> str:
            name = ""
            for item in func_def.signature.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.name) + " "
                elif isinstance(item, zc.FunctionSignatureParams):
                    if name.endswith(" "): name = name[:-1]
                    brace = "("
                    for param in item.params:
                        for param_name in param.names:
                            brace += str(param.type)+ ", "
                    if brace.endswith(", "): brace = brace[:-2]
                    brace += ")"
                    name += brace
            return name
        
        def untyped_handle(func_def) -> str:
            name = ""
            for item in func_def.signature.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.name)
                elif isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        for param_name in param.names:
                            name += "◦"
            return name    

        def make_function(func_def, st):
            refToFunc = zc.SingleFunctionDef(func_def=func_def)
            statements = zc.FunctionStatements(statements=[refToFunc])
            function = zc.Function( results=func_def.results, signature=func_def.signature, body=statements)
            return function
        
        def add_alias_symbol(func_def, function, st, long_handle, short_handle):
            typedef = func_def._is_constructor_for_typedef
            name = typedef.names[0].name
            alias = typedef.names[0].alias
            if alias:
                alias_long_handle = long_handle.replace(str(name), str(alias))
                alias_short_handle = short_handle.replace(str(name), str(alias))
                st.add(alias_long_handle, function, None)
                st.add(alias_short_handle, function, None)
                first_lex = get_first_lex(func_def)
                compiler.report(first_lex, f"\"{alias_long_handle}\" => {function}")
                compiler.report(first_lex, f"\"{alias_short_handle}\" => {function}")
        
        def modify_function(func_def, existing, st):
            long_handle = func_def.signature.typed_handle()
            mod = str(func_def.modifier)
            this_def = zc.SingleFunctionDef(func_def=func_def)
            if mod == "replace":
                existing.body.statements = [this_def]
            elif mod == "after":
                sm = existing.body.statements[0]
                if isinstance(sm, zc.SequenceFunctionDef):
                    sm.comps.append(this_def)
                else:
                    sm = zc.SequenceFunctionDef(comps = [sm, this_def])
                    existing.body.statements = [sm]
            elif mod == "before":
                sm = existing.body.statements[0]
                if isinstance(sm, zc.SequenceFunctionDef):
                    sm.comps.append(this_def)
                else:
                    sm = zc.SequenceFunctionDef(comps = [this_def, sm])
                    existing.body.statements = [sm]
            elif mod == "on":
                sm = existing.body.statements[0]
                if isinstance(sm, zc.ParallelFunctionDef):
                    sm.comps.append(this_def)
                else:
                    sm = zc.ParallelFunctionDef(comps = [sm, this_def])
                    existing.body.statements = [sm]

    def setup_check_types(self, compiler: Compiler):
        @Entity.method(zc.ResultVariableRef) # ResultVariableRef.check_types
        def check_type(self, scope): 
            self._type = self.variable._type
            compiler.report(get_first_lex(self), f"{self._type}")

        @Entity.method(zc.ResultVariableDef) # ResultVariableDef.check_types
        def check_type(self, scope): 
            self._type = self.type
            compiler.report(get_first_lex(self), f"{self._type}")

        @Entity.method(zc.AssignmentLhs) # AssignmentLhs.check_types
        def check_type(self, scope):
            if len(self.results) == 0: self._type = None
            elif len(self.results) == 1: self._type = self.results[0]._type
            else: self._type = zc.MultipleTypes([r._type for r in self.results])
            compiler.report(get_first_lex(self), f"{self._type}")

        @Entity.method(zc.Assignment) #Assignment.check_types
        def check_type(self, scope):
            type_a = self.lhs._type if hasattr(self.lhs, "_type") else None
            type_b = self.rhs._type
            if not can_assign(type_a, type_b):
                compiler.error(get_first_lex(self), f"cannot assign {type_b} to {type_a}")

        def can_assign(type_a, type_b):
            if type_a is not None: type_a = type_a.get_alias()
            if type_b is not None: type_b = type_b.get_alias()
            if type_a == type_b: return True
            if isinstance(type_a, zc.Type) and isinstance(type_b, zc.MaybeTypes):
                distances = [type_a.find_relationship(t) for t in type_b.types]
                if any(d is None for d in distances): return False
                if all(isinstance(d, int) and d <= 0 for d in distances): return True
                log_exit("not implemented: can_assign " + str(type_a) + " = " + str(type_b))
                return True
            return False
        
        @Entity.method(zc.Type) # Type.get_alias
        def get_alias(self) -> zc.Type:
            if self.alias is None: return self
            alias = self.alias
            rank = 0
            if alias.endswith("$"):
                rank = alias[-1]
                alias = alias[:-1]
            alias_type = compiler.find_symbol(alias, zc.Type, None, raise_errors=True, read_only=True)
            if alias_type is None: compiler.error(get_first_lex(self), f"alias {self.alias} not found")
            return alias_type

        # -ve means type_b is a child of self, +ve type_b is a parent of self, None means no relationship
        # reminder: type_a > type_b means "every type_a is a type_b, but not every type_b is a type_a"
        @Entity.method(zc.Type) # Type.find_relationship
        def find_relationship(self, type_b: zc.Type) -> int|None:
            depth = find_type_relationship_rec(self, type_b, 0)
            if depth != None: return depth
            depth = find_type_relationship_rec(type_b, self, 0)
            if depth != None: return -depth
            return None
        
        def find_type_relationship_rec(child: zc.Type, parent: zc.Type, depth: int) -> int|None:
            if child == parent: return depth
            if child.parents == None: return None
            for p in child.parents:
                if not isinstance(p, zc.Type): raise Exception(f"find_type_relationship_rec: {p} is not a Type")
                result = find_type_relationship_rec(p, parent, depth + 1)
                if result: return result
            return None
        
    def setup_generate(self, compiler: Compiler):
        codegen = compiler.codegen
                
        @Entity.method(zc.Function)     # Function.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            for s in self.body.statements:
                s.generate(replace, source_loc)

        @Entity.method(zc.SingleFunctionDef)
        def generate(self, replace: Dict, source_loc: List[Lex]):
            self.func_def.generate(replace, source_loc)

        @Entity.method(zc.SequenceFunctionDef)
        def generate(self, replace: Dict, source_loc: List[Lex]):
            for comp in self.comps:
                comp.generate(replace, source_loc)

        @Entity.method(zc.ParallelFunctionDef)
        def generate(self, replace: Dict, source_loc: List[Lex]):
            for comp in self.comps:
                comp.generate(replace, source_loc)

        @Entity.method(zc.FunctionDef)     # FunctionDef.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            if "_results" in replace:
                results = self.get_results()
                for r, v in zip(results, replace["_results"]):
                    replace[r] = v
            if isinstance(self.body, zc.EmitFunctionBody):
                return self.emit(replace)
            return self.body.generate(replace, source_loc)
        
        @Entity.method(zc.FunctionStatements)   # FunctionStatements.generate
        def generate(self, replace: Dict, source_loc: List[Lex]) -> List[str]:
            for s in self.statements:
                if not isinstance(s, zc.Assignment): continue
                lhs = s.lhs.generate(replace, source_loc) if s.lhs else []
                fn_replace = replace.copy()
                if len(lhs) > 0: fn_replace["_results"] = lhs
                rhs = s.rhs.generate(fn_replace, source_loc)
                if len(lhs) != len(rhs):
                    compiler.error(f"FunctionStatements.generate: {codegen.show(self)}, {lhs} != {rhs}")
                for l, r in zip(lhs, rhs):
                    if l != r:
                        if "_" in r:
                            codegen.output("mov", l, [r])
                        else:
                            codegen.output("const", l, [r])

        @Entity.method(zc.AssignmentLhs)   # AssignmentLhs.generate
        def generate(self, replace: Dict, source_loc: List[Lex]) -> List[str]:
            results = []
            for r in self.results:
                results += r.generate(replace, source_loc)
            return results
        
        @Entity.method(zc.ResultVariableRef)   # ResultVariableRef.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            return self.variable.generate(replace, source_loc)

        @Entity.method(zc.ResultVariableDef)   # ResultVariableDef.generate
        def generate(self, replace: Dict, source_loc: List[Lex]):
            result_names = [ str(name.name) for name in self.names]
            actual_names = [self.type.add_var(r) for r in result_names]
            for r, a in zip(result_names, actual_names):
                replace[r] = a
            return actual_names
        
        @Entity.method(zc.Type) # Type.add_var
        def add_var(self, name: str) -> str:
            i_var = codegen.alloc_var_index()
            var_name = f"{name}_{i_var}"
            if self.properties:
                for p in self.properties:
                    for n in p.names:
                        codegen.add_var(f"{var_name}.{n.name}", p.type.name)
            else:
                codegen.add_var(var_name, self.name)
            return var_name
        
        @Entity.method(zc.Function) # Function.get_params
        def get_params(self) -> List[str]:
            vars = []
            for item in self.signature.elements:
                if isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        for name in param.names:
                            vars.append(str(name.name))
            return vars
        
        @Entity.method(zc.FunctionDef) # FunctionDef.get_params
        def get_params(self) -> List[str]:
            vars = []
            for item in self.signature.elements:
                if isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        for name in param.names:
                            vars.append(str(name.name))
            return vars
        
        @Entity.method(zc.FunctionDef) # FunctionDef.get_results
        def get_results(self) -> List[str]:
            vars = []
            if self.results is not None:
                for r in self.results.results:
                    for name in r.names:
                        vars.append(str(name.name))
            return vars
        
        @Entity.method(zc.Function) # Function.get_results
        def get_results(self) -> List[str]:
            vars = []
            if self.results is not None:
                for r in self.results.results:
                    for name in r.names:
                        vars.append(str(name.name))
            return vars
        
        @Entity.method(zc.Function) # Function.get_result_types
        def get_result_types(self) -> List[zc.Type]:
            types = []
            if self.results is not None:
                for r in self.results.results:
                    for name in r.names:
                        types.append(r.type)
            return types
        
        @Entity.method(zc.Function) # Function.make_temp_vars
        def make_temp_vars(self) -> List[str]:
            result_vars = []
            results = self.get_results()
            types = self.get_result_types()
            temp_results = []
            for r, t in zip(results, types):
                temp_var = t.add_var(r)
                result_vars.append(temp_var)
            return result_vars
        
        @Entity.method(zc.FunctionDef) # FunctionDef.emit
        def emit(self, replace):
            log(f"emit: {codegen.show(self)}, {replace}")
            fn_name = self.emit_fn_name()
            result_vars = self.get_results()
            result_vars = [codegen.try_replace(r, replace) for r in result_vars]
            params = self.get_params() 
            params = [codegen.try_replace(p, replace) for p in params]
            log(f"  fn_name: {fn_name}")
            log(f"  result_vars: {result_vars}")
            log(f"  params: {params}")
            if len(result_vars) >= 2: raise Exception(f"multiple results not supported: {result_vars}")
            codegen.output(fn_name, result_vars[0], params)
            return result_vars

        @Entity.method(zc.FunctionDef) # Function.emit_fn_name
        def emit_fn_name(self):
            out = ""
            for element in self.signature.elements:
                if hasattr(element, "name"): out += f"{element.name}_"
            if out.endswith("_"): out = out[:-1]
            return out
        
        @Entity.method(zc.Function) # Function.typed_emit_fn
        def typed_emit_fn(self):
            out = ""
            for element in self.signature.elements:
                if hasattr(element, "name"): out += f"{element.name}_"
                elif isinstance(element, zc.FunctionSignatureParams):
                    for param in element.params:
                        type = str(param.type.name)
                        for name in param.names:
                            out += f"{type}_"
            result_types = self.get_result_types()
            for t in result_types: out += f"_{str(t)}"
            return out


    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("result_vars_1", parse_code("(a, b: int, k, l: float) =", "FunctionResults", grammar), """
            FunctionResults
                results: List[ResultVariableDef]
                    ResultVariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                    ResultVariableDef
                        type: Type => float
                        names: List[NameDef]
                            NameDef
                                name: str = k
                                alias: str = None
                            NameDef
                                name: str = l
                                alias: str = None
                assign_op: str = =
            """)
    
        test("result_vars_2", parse_code("(int a, b, float k) <<", "FunctionResults", grammar), """
            FunctionResults
                results: List[ResultVariableDef]
                    ResultVariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                    ResultVariableDef
                        type: Type => float
                        names: List[NameDef]
                            NameDef
                                name: str = k
                                alias: str = None
                assign_op: str = <<
            """)
        
        test("param_group_0", parse_code("(int a, b=0, float k=0)", "FunctionSignatureParams", grammar), """
            FunctionSignatureParams
                params: List[VariableDef]
                    VariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                        value: Expression
                            Constant
                                value: str = 0
                    VariableDef
                        type: Type => float
                        names: List[NameDef]
                            NameDef
                                name: str = k
                                alias: str = None
                        value: Expression
                            Constant
                                value: str = 0
            """)
        
        test("param_group_1", parse_code("(int a, b, float k)", "FunctionSignatureParams", grammar), """
            FunctionSignatureParams
                params: List[VariableDef]
                    VariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                        value: Expression = None
                    VariableDef
                        type: Type => float
                        names: List[NameDef]
                            NameDef
                                name: str = k
                                alias: str = None
                        value: Expression = None
            """)
        
        test("param_group_2", parse_code("(a, b: int, k: float)", "FunctionSignatureParams", grammar), """
            FunctionSignatureParams
                params: List[VariableDef]
                    VariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                        value: Expression = None
                    VariableDef
                        type: Type => float
                        names: List[NameDef]
                            NameDef
                                name: str = k
                                alias: str = None
                        value: Expression = None
                    """)
        
        test("function_0", parse_code("r = a + b", "Statement", grammar), """
            Assignment
                lhs: AssignmentLhs
                    AssignmentLhs
                        results: List[ResultVariable]
                            ResultVariableRef
                                variable: VariableRef
                                    VariableRef
                                        variables: List[str]
                                            r
                        assign_op: str = =
                rhs: Expression
                    FunctionCall
                        items: List[FunctionCallItem]
                            FunctionCallWord
                                name: str = a
                            FunctionCallOperator
                                name: str = +
                            FunctionCallWord
                                name: str = b
            """)
        
        test("function_1", parse_code("on (int r) = min (int a, b) { r = if (a < b) then a else b }", "FunctionDef", grammar), """
            FunctionDef
                modifier: str = on
                results: FunctionResults
                    FunctionResults
                        results: List[ResultVariableDef]
                            ResultVariableDef
                                type: Type => int
                                names: List[NameDef]
                                    NameDef
                                        name: str = r
                                        alias: str = None
                        assign_op: str = =
                signature: FunctionSignature
                    FunctionSignature
                        elements: List[FunctionSignatureElement]
                            FunctionSignatureWord
                                name: str = min
                            FunctionSignatureParams
                                params: List[VariableDef]
                                    VariableDef
                                        type: Type => int
                                        names: List[NameDef]
                                            NameDef
                                                name: str = a
                                                alias: str = None
                                            NameDef
                                                name: str = b
                                                alias: str = None
                                        value: Expression = None
                body: FunctionBody
                    FunctionStatements
                        statements: List[Statement]
                            Assignment
                                lhs: AssignmentLhs
                                    AssignmentLhs
                                        results: List[ResultVariable]
                                            ResultVariableRef
                                                variable: VariableRef
                                                    VariableRef
                                                        variables: List[str]
                                                            r
                                        assign_op: str = =
                                rhs: Expression
                                    FunctionCall
                                        items: List[FunctionCallItem]
                                            FunctionCallWord
                                                name: str = if
                                            FunctionCallArguments
                                                arguments: List[FunctionCallArgument]
                                                    FunctionCallArgument
                                                        argument: Variable = None
                                                        value: Expression
                                                            FunctionCall
                                                                items: List[FunctionCallItem]
                                                                    FunctionCallWord
                                                                        name: str = a
                                                                    FunctionCallOperator
                                                                        name: str = <
                                                                    FunctionCallWord
                                                                        name: str = b
                                            FunctionCallWord
                                                name: str = then
                                            FunctionCallWord
                                                name: str = a
                                            FunctionCallWord
                                                name: str = else
                                            FunctionCallWord
                                                name: str = b
            """)
    
#--------------------------------------------------------------------------------------------------
# tests

class module_Tests(LanguageModule):
    def setup_grammar(self, compiler: Compiler):
        compiler.grammar.add("""
        TestDef < Component := ">" lhs:Expression ("=>" rhs:Expression)?
        """)

    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("test_0", parse_code("> a", "TestDef", grammar), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[str]
                            a
                rhs: Expression = None
        """)

        test("test_1", parse_code("> a =>", "TestDef", grammar), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[str]
                            a
                rhs: Expression
                    premature end: expected rhs:Expression
        """)

        test("test_2", parse_code("> a => b", "TestDef", grammar), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[str]
                            a
                rhs: Expression
                    VariableRef
                        variables: List[str]
                            b
        """)

