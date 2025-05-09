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
    on run()
        main()
        shutdown()
    on main() pass
""",
"""
feature Hello extends Program
    on hello()
        out$ << "ᕦ(ツ)ᕤ"
    replace main()
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
feature Backend extends Program
    type u8, u16, u32, u64
    type i8, i16, i32, i64
    type f16, f32, f64
    type int > i8, i16, i32, i64
    type uint > u8, u16, u32, u64
    type float > f32, f64
    type number > uint, int, float
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
    on (out$) << (char c)
        uart(c)
    on shutdown()
        store(0x5555, 0x100000)
    on uart(u8 c)
        store(c, 0x10000000)
    on store(uint value, address) emit
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
    compiler.set_concrete_types({ "number": "f32", "int": "i32", "char": "u8"})
    
    test_verbose(False)
    log_max_depth(200)

    compiler.setup()
    #context = Context("test_vector_math", ["Program", "Math", "VectorMath", "Backend"])
    context = Context("test_hello", ["Program", "Hello", "Backend"])
    test_context(compiler, context)
    
def test_context(compiler: Compiler,context: Context):
    program = compiler.compile(s_test_features, context)
    log(program.visual_report())
    log("--------------------------------------------------------------------")
    log("VM block:")
    log(program.vm_block)

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
    
    test("print_formatted", as_code(ast), """
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
            Program := components:FeatureDef+;
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
        @Entity.method(zc.Program) # Program.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            log(f"Program.generate: {self}")
            function_name = "run()"
            function = compiler.find_symbol(function_name, zc.Function, self, raise_errors=True, read_only=True)
            if not function:
                compiler.error(self.name, f"function not found: {function_name}")
            result= function.generate({})
            log(f"----------------------------------------------")
            log(result)
            return result

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
            return log_strip(as_code(self, compiler.grammar))
            
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
                    read_only_var = compiler.find_symbol(var, zc.Variable, scope, raise_errors=False, read_only=True)
                    if read_only == False and read_only_var is not None:
                        compiler.error(var, f"can't write to {var} ({read_only_var._owner}) from {scope}")
                    else:
                        compiler.error(var, f"variable not found in {scope}: {var}")
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
            compiler.report(self.items[i_operator].name, f"split: left = \"{log_strip(as_code(before[0], compiler.grammar))}\", right = \"{log_strip(as_code(after[0], compiler.grammar))}\"")
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
            val = str(self.value)
            type_name = "error"
            if val.startswith('"'):
                type_name = "string"
            elif "." in val:
                type_name = "number"
            else:
                val = int(val, 16) if val.startswith("0x") else int(val)
                n_bits = val.bit_length()
                sizes = [8, 16, 32, 64]
                for s in sizes:
                    if n_bits <= s:
                        type_name = "u" + str(s)
                        break
            type = compiler.find_symbol(type_name, zc.Type, scope, raise_errors=True, read_only=True)
            self._type_ref = type.type_ref()
            compiler.report(self.value, f"{self._type_ref}")
        
        @Entity.method(zc.VariableRef) # VariableRef.check_type
        def check_type(self, scope):
            if not hasattr(self, "_resolved_vars"):
                self._type_ref = None
                compiler.error(self.variables[0], f"variable not resolved")
            else:
                for i, var in enumerate(self._resolved_vars[:-1]):
                    compiler.report(self.variables[i], f"{var.type}")
                var = self._resolved_vars[-1]
                type = var.type
                self._type_ref = type.type_ref()
                rank = str(var.name).count('$')
                self._type_ref.rank = rank
                compiler.report(self.variables[-1], f"{self._type_ref}")
        
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.check_type
        def check_type(self, scope):
            self._type_ref = self.variable._type_ref
            
            if self._type_ref: compiler.report(get_first_lex(self.variable), f"{self._type_ref}")
            
        @Entity.method(zc.FunctionCall) # FunctionCall.check_type
        def check_type(self, scope):
            cf= log_strip(as_code(self, compiler.grammar))
            self._resolved_functions = []
            functions = find_functions(self, scope)
            self._resolved_functions = functions
            for f in functions:
                if not hasattr(f, "_type_ref"): f.check_type(scope)
            if len(functions) == 0:
                compiler.error(self, f"no functions found for {cf} in {scope}")
                self._type_rank = None
            elif len(functions) > 1:
                self._type_ref = zc.MaybeTypes([f._type_ref.type for f in functions]).type_ref()
            else:
                self._type_ref = functions[0]._type_ref
            first_item = next((item for item in self.items if hasattr(item, "name")), None)
            compiler.report(first_item.name, f"{self._type_ref}")

        @Entity.method(zc.Function) # Function.check_type
        def check_type(self, scope):
            result_type_refs = []
            if self.results != None:
                for frv in self.results.results:
                    result_type_refs.append(get_type(frv))
            if len(result_type_refs) == 0: self._type_ref = None
            elif len(result_type_refs) == 1: self._type_ref = result_type_refs[0]
            else: self._type_ref = zc.MultipleTypes([r.type for r in result_type_refs]).type_ref()

        def get_type(frv: zc.ResultVariable) -> zc.TypeRef:
            if isinstance(frv, zc.ResultVariableDef): return frv._type_ref
            elif isinstance(frv, zc.ResultVariableRef): return frv.variable._type_ref
            else: raise Exception(f"unknown ResultVariable: {frv}")

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
                return []
            fc._constraints = fn_types # dunno what this is really for, but we'll find out later
            return filtered_functions
        
        def get_param_types(fc: zc.FunctionCall) -> List[zc.TypeRef]:
            out = []
            for item in fc.items:
                if hasattr(item, "_type_ref"):
                    out.append(item._type_ref)
                elif isinstance(item, zc.FunctionCallArguments):
                    for arg in item.arguments:
                        if arg.value:
                            out.append(arg.value._type_ref)
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
                distance = t.type.find_relationship(fc_types[i])
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
        @Entity.method(zc.FunctionCallArguments) # FunctionCallArguments.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            log(self)
            return VmBlock.combine([arg.generate(replace) for arg in self.arguments])

        @Entity.method(zc.Constant) # Constant.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            inputs = []
            type_name = str(self._type_ref.type.name)
            if self._type_ref.rank == 0:
                outputs = [VmVar("imm", type_name)]
                instructions = [VmInstruction(opcode="imm", dests=outputs, sources=[self.value])]
                result = VmBlock([], outputs, instructions)
                return result
            else:
                type_name = compiler.get_concrete_type(str(self._type_ref.type.name))
                constant = VmConst(type_name, len(str(self.value)), [str(self.value)])
                address = VmVar("address", "u32")
                count = VmVar("count", "u32")
                instructions = [
                    VmInstruction(opcode="imm", dests=[address], sources=[hex(constant.address)], comment="load address of array"),
                    VmInstruction(opcode="imm", dests=[count], sources=[len(str(self.value))], comment="load length of array")
                ]
                return VmBlock([], [address, count], instructions)
                log_exit("zc.Constant array-gen")
        
        @Entity.method(zc.VariableRef) # VariableRef.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            var = self._resolved_vars[0]
            log(f"var: {var}, type: {var.type}")
            if var in replace:
                return VmBlock([], [replace[var]], [])
            else:
                raise Exception(f"{self} not in (replace)")

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
                var._owner = scope
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
            TypeRef := type:Type& rank:<number>
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
                if prop.value: code += f" = {as_code(prop.value, compiler.grammar)}"
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
            FunctionResults := "(" results:ResultVariable+, ")" assign_op:("=" | "<<")
            FunctionSignature := elements:FunctionSignatureElement+
            FunctionSignatureElement :=
            FunctionSignatureWord < FunctionSignatureElement := name:(<identifier> | <operator>)
            FunctionSignatureParams < FunctionSignatureElement := "(" params:VariableDef*, ")"
            FunctionBody := 
            FunctionStatements < FunctionBody := "{" statements:Statement*; "}"
            EmitFunctionBody < FunctionBody := "emit"
            PassFunctionBody < FunctionBody := "pass"
            Statement := 
            Assignment < Statement := lhs:AssignmentLhs rhs:Expression
            AssignmentLhs := results:ResultVariable+, "=" 
            ResultVariable :=
            ResultVariableDef < ResultVariable := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&))
            StreamAssignment < Statement := lhs:StreamAssignmentLhs rhs:StreamAssignmentRhs
            StreamAssignmentLhs := results:ResultVariable+, "<<"
            StreamAssignmentRhs := expressions:Expression+<< terminator:StreamTerminator? rate:StreamRate?
            StreamTerminator := op:("while" | "until") condition:Expression
            StreamRate := "," "at" rate:Expression
            VoidFunctionStatement < Statement := function_call:FunctionCall
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
            long_handle = self.signature.typed_handle()
            first_lex = self.signature.get_first_name()
            compiler.cp.st.add(long_handle, self, scope)
            compiler.report(first_lex, f"\"{long_handle}\" => {self} in {scope}")
            short_handle = self.signature.untyped_handle()
            if short_handle != long_handle:
                compiler.cp.st.add(short_handle, self, scope)
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

        @Entity.method(zc.FunctionDef) # FunctionDef.resolve
        def resolve(self, scope, type_name):
            short_handle = self.signature.untyped_handle()
            if short_handle == "◦" and str(self.results.assign_op) == "<<":
                result = self.results.results[0]
                if isinstance(result, zc.ResultVariableRef): # hook to var
                    var = result.variable._resolved_vars[0]
                    var._override_push_fn = self._function
                elif isinstance(result, zc.ResultVariableDef): # hook to type
                    type = result.type
                    type.override_push_fn = self._function

        @Entity.method(zc.FunctionSignature) # FunctionSignature.get_first_name
        def get_first_name(self) -> str:
            for e in self.elements:
                if hasattr(e, "name"): return e.name
            return get_first_lex(self)
            
        @Entity.method(zc.FunctionSignature) # FunctionSignature.typed_handle
        def typed_handle(self) -> str:
            name = ""
            for item in self.elements:
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
        
        @Entity.method(zc.FunctionSignature) # FunctionSignature.untyped_handle
        def untyped_handle(self) -> str:
            name = ""
            for item in self.elements:
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
        @Entity.method(zc.StreamAssignmentLhs) # StreamAssignmentLhs.check_types
        def check_type(self, scope):
            self._type_ref = self.results[0]._type_ref
            compiler.report(get_first_lex(self), f"{self._type_ref}")

        @Entity.method(zc.ResultVariableRef) # ResultVariableRef.check_types
        def check_type(self, scope): 
            self._type_ref = self.variable._type_ref
            compiler.report(get_first_lex(self), f"{self._type_ref}")

        @Entity.method(zc.ResultVariableDef) # ResultVariableDef.check_types
        def check_type(self, scope): 
            rank = str(self.names[0].name).count('$')
            self._type_ref = zc.TypeRef(self.type, rank)
            compiler.report(get_first_lex(self), f"{self._type_ref}")

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

        @Entity.method(zc.StreamAssignment) # StreamAssignment.check_types
        def check_type(self, scope):
            type_ref_a = self.lhs._type_ref if hasattr(self.lhs, "_type_ref") else None
            for i, type_ref_b in enumerate(self.rhs._type_refs):
                if not can_assign(type_ref_a, type_ref_b):
                    compiler.error(get_first_lex(self.rhs.expressions[i]), f"cannot assign {type_ref_b} to {type_ref_a}")
            
        @Entity.method(zc.StreamAssignmentRhs) # StreamAssignmentRhs.check_types
        def check_type(self, scope):
            self._type_refs = []
            for e in self.expressions:
                e.check_type(scope)
                self._type_refs.append(e._type_ref)

        def can_assign(to_type_ref: zc.TypeRef, from_type_ref: zc.TypeRef):
            to_type_ref = to_type_ref.type.type_ref()
            from_type_ref = from_type_ref.type.type_ref()
            to_type = to_type_ref.type
            from_type = from_type_ref.type
            distance = to_type.find_relationship(from_type)
            if distance is not None and distance <= 0: return True
            if isinstance(to_type, zc.Type) and isinstance(from_type, zc.MaybeTypes):
                distances = [to_type.find_relationship(t) for t in from_type.types]
                if any(d is None for d in distances): return False
                if all(isinstance(d, int) and d <= 0 for d in distances): return True
                log_exit("not implemented: can_assign " + str(to_type) + " = " + str(from_type))
                return True
            return False
        
        @Entity.method(zc.Type) # Type.type_ref
        def type_ref(self) -> zc.TypeRef:
            if self.alias is None: return zc.TypeRef(self, 0)
            alias = self.alias
            rank = 0
            alias = str(alias)
            if alias.endswith("$"):
                rank = alias.count('$')
                alias = alias[:-rank]
            alias_type = compiler.find_symbol(alias, zc.Type, None, raise_errors=True, read_only=True)
            if alias_type is None: compiler.error(get_first_lex(self), f"alias {self.alias} not found")
            return zc.TypeRef(alias_type, rank)

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
        @Entity.method(zc.Function) # Function.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            log(f"{self}")
            if len(self.body.statements) != 1: raise Exception(f"Function.generate: {self}")
            return self.body.statements[0].generate(replace)
        
        @Entity.method(zc.SingleFunctionDef) # SingleFunctionDef.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            result = self.func_def.generate(replace)
            return result
        
        @Entity.method(zc.SequenceFunctionDef) # SequenceFunctionDef.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            #log(f"{self}")
            result= VmBlock.combine([s.generate(replace) for s in self.comps])
            return result

        @Entity.method(zc.FunctionDef) # FunctionDef.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            log(self)
            if isinstance(self.body, zc.PassFunctionBody):
                return VmBlock()
            if isinstance(self.body, zc.EmitFunctionBody):
                inputs = [replace[p] for p in get_param_vars(self.signature)]
                outputs = [replace[p] for p in get_output_vars(self.results)]
                instruction = VmInstruction(emit_fn_name(self), outputs, inputs)
                return VmBlock([], [], [instruction])
            instructions = []
            for s in self.body.statements:
                s_vm = s.generate(replace)
                instructions += s_vm.instructions
            inputs = [replace[p] for p in get_param_vars(self.signature)]
            outputs = [replace[p] for p in get_output_vars(self.results) if p in replace]
            for i in instructions: 
                if isinstance(i, VmInstruction):
                    if i.comment is None:
                        i.comment = str(emit_fn_name(self))
            return VmBlock(inputs, outputs, instructions)
        
        @Entity.method(zc.FunctionCall) # FunctionCall.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            log(self)
            log("replace:", replace)
            args = get_arguments(self)
            arg_vms = [arg.generate(replace) for arg in args]
            fn = self._resolved_functions[0]
            arg_instructions = [instr for i in arg_vms for instr in i.instructions]
            arg_outputs = [output for vm in arg_vms for output in vm.outputs]
            fn_vm = generate_from_arg_vars(fn, arg_outputs)
            fn_vm.instructions = arg_instructions + fn_vm.instructions
            log(fn_vm)
            return fn_vm

        def generate_from_arg_vars(fn: zc.Function, arg_outputs: List[VmVar]) -> VmBlock:
            for o in arg_outputs:
                if isinstance(o, VmBlock): raise Exception("AAAAAAA")
            # this is assuming inline for now
            new_replace = {}
            inputs = get_param_vars(fn.signature)
            new_replace = { i: o for i, o in zip(inputs, arg_outputs) }
            return fn.generate(new_replace)
        
        def choose_best_function(fns: List[zc.Function]):
            return fns[0]
        
        def emit_fn_name(fd: zc.FunctionDef) -> str:
            return fd.signature.get_first_name()
        
        def get_arguments(function_call: zc.FunctionCall) -> List[zc.Expression]:
            args = []
            for item in function_call.items:
                if not isinstance(item, zc.FunctionCallArguments): continue
                for arg in item.arguments:
                    if isinstance(arg, zc.Constant): args.append(arg)
                    else: args.append(arg.value)
            return args

        def get_param_vars(signature: zc.FunctionSignature) -> List[Variable]:
            vars = []
            for e in signature.elements:
                if isinstance(e, zc.FunctionSignatureParams):
                    for p in e.params:
                        if hasattr(p, "_defined_vars"):
                            vars += p._defined_vars
            return vars

        def get_output_vars(results: zc.FunctionResults):
            outputs = []
            if results is None: return outputs
            for r in results.results:
                if isinstance(r, zc.ResultVariableDef):
                    outputs += r._defined_vars
                elif isinstance(r, zc.ResultVariableRef):
                    log(dbg_entity(r))
                    outputs += r.variable._resolved_vars
            return outputs
                    
        @Entity.method(zc.StreamAssignment) # StreamAssignment.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            log("StreamAssignment.generate:", compiler.code(self))
            instructions = []
            if len(self.rhs.expressions) > 1: raise Exception("for now, only one << supported")
            push_fn = get_override_fn(self.lhs)
            if push_fn is None: raise Exception("need to implement standard push fn!!")
            log("push_fn:", push_fn)
            log("rhs expressions:")
            for e in self.rhs.expressions:
                if e._type_ref.rank == 0:
                    e_vm = e.generate(replace)
                    vm = generate_from_arg_vars(push_fn, e_vm.outputs, replace)
                    instructions += vm.instructions
                else:
                    e_vm = e.generate(replace)
                    address_var = e_vm.outputs[0]
                    count_var = e_vm.outputs[1]
                    index_var = VmVar("index", "u32")
                    item_var = VmVar("item", compiler.get_concrete_type(str(e._type_ref.type.name)))
                    instructions = [
                        VmInstruction("var", [index_var], ["0"], "initialise loop index var"),
                        VmLabel("loop_start"),
                        VmInstruction("cmp", [], [index_var, count_var], "check loop index against count"),
                        VmInstruction("bge", [], ["loop_end"], "jump to loop end if we're done"),
                        VmInstruction("load", [item_var], [address_var, index_var], "load item from array"),
                    ]
                    fn_params = get_param_vars(push_fn.signature)
                    log("params:", fn_params)
                    fn_vm = generate_from_arg_vars(push_fn, [item_var])
                    const_instructions = [ i for i in fn_vm.instructions if isinstance(i, VmInstruction) and i.opcode == "imm" ]
                    non_const_instructions = [ i for i in fn_vm.instructions if not isinstance(i, VmInstruction) or i.opcode != "imm" ]
                    instructions = const_instructions + instructions + non_const_instructions
                    instructions += [
                        VmInstruction("add", [index_var], [index_var, 1], "increment loop index"),
                        VmInstruction("bra", [], ["loop_start"], "jump back to start of loop"),
                        VmLabel("loop_end")
                    ]
                    log("-------------- loop ----------------")
                    instructions = e_vm.instructions + instructions
                    for i in instructions: log(i)
            result = VmBlock([], [], instructions)
            return result

        def get_override_fn(lhs: zc.StreamAssignmentLhs) -> zc.Function:
            result = lhs.results[0]
            if isinstance(result, zc.ResultVariableRef):
                var = result.variable._resolved_vars[0]
                if hasattr(var, "_override_push_fn"):
                    return var._override_push_fn
                elif hasattr(var.type, "_override_push_fn"):
                    return var.type._override_push_fn
            return None

        @Entity.method(zc.TypeRef) # TypeRef.__str__
        def __str__(self):
            return f"{self.type.name}/{self.rank}"

        def generate_single(e: zc.Expression, replace: Dict[Variable, VmVar]) -> VmBlock:
            return e.generate(replace)
        
        def generate_loop(e: zc.Expression, replace: Dict[Variable, VmVar]) -> VmBlock:
            log("generate_loop")
            log(f"e = {compiler.code(e)}")
            log_exit("generate_loop")

        @Entity.method(zc.VoidFunctionStatement) # VoidFunctionStatement.generate
        def generate(self, replace: Dict[Variable, VmVar]) -> VmBlock:
            return self.function_call.generate(replace)

    def test_parser(self, compiler: Compiler):
        grammar = compiler.grammar
        test("result_vars_1", parse_code("(a, b: int, k, l: float) =", "FunctionResults", grammar), """
            FunctionResults
                results: List[ResultVariable]
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
                results: List[ResultVariable]
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
                        results: List[ResultVariable]
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

