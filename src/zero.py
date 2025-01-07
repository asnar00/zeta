# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from compiler import *
from copy import deepcopy
import zero_classes as zc
import re

#--------------------------------------------------------------------------------------------------
# main

s_test_program = """
feature Program
    type char > u8
    type string | str = char$
    string out$

feature Hello extends Program
    on hello(string name)
        string message = "hello, \\(name)!"
        out$ << message
    replace run()
        hello("world")

feature Goodbye extends Hello
    on bye()
        out$ << "kthxbye."
    after hello(string name)
        bye()
        
feature Math
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

feature VectorMath extends Math
    type vector | vec = 
        number x, y, z = 0
    on (vec v) = (vec a) + (vec b)
        v = vec(a.x + b.x, a.y + b.y, a.z + b.z)
    on (vec v) = (vec a) * (number n)
        v = vec(a.x * n, a.y * n, a.z * n)
    on (vec r) = (vec v) / (number n)
        r = vec(v.x / n, v.y / n, v.z / n)
    on (number n) = (vec a) dot (vec b)
        n = a.x * b.x + a.y * b.y + a.z * b.z
    on (number l) = length(vec v)
        l = sqrt(v dot v)
    on (vec n) = normalise(vec v)
        n = v / length(v)

feature Backend
    type bit = 0 | 1
    type u8, u16, u32, u64
    type i8, i16, i32, i64
    type f16, f32, f64
    on (f32 r) = add(f32 a, b) pass
    on (i32 r) = add(i32 a, b) pass
    on (f32 r) = sub(f32 a, b) pass
    on (i32 r) = sub(i32 a, b) pass
    on (f32 r) = mul(f32 a, b) pass
    on (i32 r) = mul(i32 a, b) pass
    on (f32 r) = div(f32 a, b) pass
    on (i32 r) = div(i32 a, b) pass
    on (f32 r) = sqrt(f32 n) pass
    on (i32 r) = sqrt(i32 n) pass

context MyContext = Program, Hello, Goodbye, Math, VectorMath, Backend
"""

# this is just scribble: but it's a nice way to define dependent types eg. i32, i64, etc
s_test_concrete = """
feature ConcreteMath extends Math
    concrete type bit = 0 | 1
    concrete type u(n) = bit[n]
    concrete type i(n) =
        bit sign
        u(n-1) val
    concrete type ieee(e, m) =
        bit sign
        u(e) exponent | exp
        u(m) mantissa | man
    concrete type f16 = ieee(5, 10)
    concrete type f32 = ieee(8, 23)
    concrete type f64 = ieee(11, 52)
"""

@this_is_the_test
def test_zero():
    log("test_zero")
    compiler = Compiler(zc)
    compiler.add_modules([module_Features(), module_Expressions(), module_Variables(), module_Types(), module_Functions(), module_Tests()])
    test_verbose(False)
    log_max_depth(12)
    compiler.setup()
    code = s_test_program
    program = compiler.compile(code)
    log_clear()
    #log(program.show_report())
    log(visual_report([ (program.reports[1].items, log_orange), (program.reports[2].items, log_green)], code))

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
                string message = "hello, \(name)!"
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
    def setup_syntax(self, compiler: Compiler):
        compiler.grammar.add("""
            NameDef := name:<identifier> ("|" alias:<identifier>)?
            FeatureDef := "feature" NameDef ("extends" parent:FeatureDef&)? "{" components:Component*; "}"
            Component :=
            ContextDef := "context" NameDef "=" feature:FeatureDef&*,
            Program := components:(FeatureDef | ContextDef)+;
            """)
        
    def setup_scope(self, compiler: Compiler):
        @Entity.method(zc.FeatureDef) # FeatureDef.get_scope
        def get_scope(self): return self

        @Entity.method(zc.FeatureDef) # FeatureDef.can_see_scope
        def can_see_scope(self, scope):
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
            
        @Entity.method(zc.ContextDef) # ContextDef.add_symbols
        def add_symbols(self, scope):
            compiler.add_symbol(self.name, self, scope, alias=self.alias)

        @Entity.method(Lex) # Lex.resolve
        def resolve(self, scope, type_name):
            cls = Entity.get_class(type_name)
            return compiler.find_symbol(self, cls, scope)

    def test_parser(self):
        test("feature_0", parse_code("", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected 'feature', got 'None' at <eof>)
                name: str = None
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = None
            """)
        
        test("feature_1", parse_code("feature", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected NameDef, got 'None' at <eof>)
                name: str = None
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = None
            """)

        test("feature_2", parse_code("feature MyFeature", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected '{', got 'None' at <eof>)
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = None
            """)

        test("feature_3", parse_code("feature MyFeature extends", "FeatureDef"), """
            FeatureDef
                !!! mismatch (expected '{', got 'extends' at :...:19)
                name: str = MyFeature
                alias: str = None
                parent => !!! premature end (expected parent:FeatureDef&, got 'None' at <eof>)
                components: List[Component] = None
            """)
        
        test("feature_4", parse_code("feature MyFeature extends Another", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected '{', got 'None' at <eof>)
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef => Another
                components: List[Component] = None        
            """)
        
        test("feature_5", parse_code("feature MyFeature {}", "FeatureDef"), """
            FeatureDef
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef = None
                components: List[Component] = []
            """)
        
        test("feature_6", parse_code("feature MyFeature extends Another {}", "FeatureDef"), """
            FeatureDef
                name: str = MyFeature
                alias: str = None
                parent: FeatureDef => Another
                components: List[Component] = []
            """)
        test("context_0", parse_code("context MyContext = Hello, Goodbye, Countdown", "ContextDef"), """
            ContextDef
                name: str = MyContext
                alias: str = None
                feature => [Hello, Goodbye, Countdown]
            """)


#--------------------------------------------------------------------------------------------------
# Expressions

class module_Expressions(LanguageModule):
    def setup_syntax(self, compiler: Compiler):
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
            return log_strip(print_code_formatted(self))
            
    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.FunctionCall) # FunctionCall.add_symbols
        def add_symbols(self, scope):
            embracket(self)

        @Entity.method(zc.VariableRef) # VariableRef.resolve
        def resolve(self, scope, type_name):
            resolved_vars = []
            for var in self.variables: # all Lex at this point
                resolved_var = compiler.find_symbol(var, zc.Variable, scope, raise_errors=False)
                if resolved_var == None:
                    compiler.error(f"variable not found: {var}")
                    resolved_vars.append(var)
                    return self
                else:
                    resolved_vars.append(resolved_var)
                    scope = resolved_var.type
                    if isinstance(scope, Lex): compiler.error(f"scope is Lex: {scope}")
            compiler.report(self.variables[0], f"var {resolved_vars}")
            self._resolved_vars = resolved_vars
            return self

        @Entity.method(zc.FunctionCall) #FunctionCall.resolve
        def resolve(self, scope, type_name):
            for i, item in enumerate(self.items):
                if isinstance(item, zc.FunctionCallWord):
                    var = compiler.find_symbol(item.name, zc.Variable, scope, raise_errors=False)
                    if var:
                        var_ref = zc.VariableRef(variables=[item.name])
                        var_ref._resolved_vars = [var]
                        self.items[i] = zc.FunctionCallVariable(variable=var_ref)
            return self

        def embracket(fc: zc.FunctionCall):
            def find_lowest_ranked_operator(items):
                i_found = -1
                lowest_rank = 10240
                for i, item in enumerate(items):
                    if isinstance(item, zc.FunctionCallOperator) and item.name != ".":
                        rank = item.name.rank
                        if rank < lowest_rank:
                            lowest_rank = rank
                            i_found = i
                return i_found
            i_operator = find_lowest_ranked_operator(fc.items)
            if i_operator == -1: return fc
            before = fc.items[0:i_operator]
            if len(before) > 1: before = [zc.FunctionCall(items=before)]
            after = fc.items[i_operator+1:]
            if len(after) > 1: after = [zc.FunctionCall(items=after)]
            compiler.report(fc.items[i_operator].name, f"split: left = \"{log_strip(print_code_formatted(before[0]))}\", right = \"{log_strip(print_code_formatted(after[0]))}\"")
            fc.items = before + [fc.items[i_operator]] + after
            return fc
            
    def setup_check_types(self, compiler: Compiler):
        @Entity.method(zc.Constant) # Constant.check_type
        def check_type(self, scope):
            type_name = "number" if self.value.type=="number" else "string"
            self._type = compiler.find_symbol(type_name, zc.Type, scope)
            compiler.report(self.value, f"{self._type}")
        
        @Entity.method(zc.VariableRef) # VariableRef.check_type
        def check_type(self, scope):
            if not hasattr(self, "_resolved_vars"):
                log(f"{self} {type(self).__name__}")
                log(Visitor.trace)
                log_exit("trace")
            self._type = self._resolved_vars[-1].type
            if self._type: compiler.report(self._resolved_vars[-1].name, f"{self._type}")
        
        @Entity.method(zc.FunctionCallVariable) # FunctionCallVariable.check_type
        def check_type(self, scope):
            self._type = self.variable._type
            if self._type: compiler.report(get_first_lex(self.variable), f"{self._type}")
            
        @Entity.method(zc.FunctionCall) # FunctionCall.check_type
        def check_type(self, scope):
            cf= log_strip(print_code_formatted(self))
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
            functions = [f.element for f in compiler.cp.st.find(short_sig, zc.Function, scope)]
            fn_types = [get_sig_param_types(f.signature) for f in functions]
            distances = [get_distances(fc_types,fn_types[i]) for i in range(len(fn_types))]
            i_best = find_best_positive_distance(distances)
            if i_best is not None: return [functions[i_best]]
            # filter out functions with None in their distances
            filtered_functions = []
            for i, dist_list in enumerate(distances):
                if None not in dist_list: filtered_functions.append(functions[i])
            if len(filtered_functions) == 0:
                compiler.error(fc, f"no function found for {log_strip(print_code_formatted(fc))} in {scope}")
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
                distance = find_type_relationship(t, fc_types[i])
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
        
        def find_type_relationship(type_a: zc.Type, type_b: zc.Type) -> int|None:
            depth = find_type_relationship_rec(type_a, type_b, 0)
            if depth != None: return depth
            depth = find_type_relationship_rec(type_b, type_a, 0)
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

    def test_parser(self):
        test("expression_0", parse_code("1", "Expression"), """
            Constant
                value: str = 1
            """)

        test("expression_1", parse_code("a", "Expression"), """
            VariableRef
                variables: List[str]
                    a
            """)
        
        test("expression_1a", parse_code("a.b", "Expression"), """
            VariableRef
                variables: List[str]
                    a
                    b
            """)

        test("expression_2", parse_code("a + b", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        name: str = a
                    FunctionCallOperator
                        name: str = +
                    FunctionCallWord
                        name: str = b
            """)
        
        test("expression_2a", parse_code("v.x + v.y", "Expression"), """
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
        
        test("parameter_0", parse_code("a = 1", "FunctionCallArgument"), """
                FunctionCallArgument
                    argument: Variable => a
                    value: Expression
                        Constant
                            value: str = 1
                """)

        test("parameter_1", parse_code("a + b", "FunctionCallArgument"), """
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

        test("brackets_0", parse_code("(a + b)", "Bracketed"), """
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
        
        test("brackets_1", parse_code("(v = a + b)", "Expression"), """
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
        
        test("expression_3", parse_code("2 - c", "Expression"), """
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

        test("expression_4", parse_code("a + (2 - c)", "Expression"), """
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

        test("expression_5", parse_code("hello(name = \"world\")", "Expression"), """
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
        
        test("expression_6", parse_code("bye()", "FunctionCall"), """
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
    def setup_syntax(self, compiler: Compiler):
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

    def test_parser(self):
        test("variable_0", parse_code("int a", "VariableDef"), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression = None
            """)
        
        test("variable_1", parse_code("a : int", "VariableDef"), """
            VariableDef
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression = None
            """)
        
        test("variable_2", parse_code("a : int = 0", "VariableDef"), """
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
        
        test("variable_3", parse_code("int a = 0", "VariableDef"), """
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
        
        test("variable_4", parse_code("r|red, g|green, b|blue: number = 0", "VariableDef"), """
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
        
        test("variable_5", parse_code("int x,y = 0", "VariableDef"), """
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
    def setup_syntax(self, compiler: Compiler):
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

    def setup_generate(self, compiler: Compiler):
        @Entity.method(zc.TypeDef)
        def generate(self):
            if not isinstance(self.rhs, zc.StructDef): return
            def make_constructor(typeDef: zc.TypeDef):
                name = typeDef.names[0].name
                result_var = "_" + str(name)[0].lower()
                code = f"on ({name} {result_var}) = {name}("
                for prop in typeDef.rhs.properties:
                    code += f"{prop.type} "
                    for var in prop.names: code += f"{var.name}, "
                    if code.endswith(", "): code = code[:-2]
                    if prop.value: code += f" = {print_code_formatted(prop.value)}"
                code += ") {"
                for prop in typeDef.rhs.properties:
                    for var in prop.names:
                        code += f"{result_var}.{var.name} = {var.name}; "
                code += "}"
                compiler.report(self.names[0].name, f"make_constructor:{code}")
                constructor = parse_simple(code, "FunctionDef")
                constructor._is_constructor_for_typedef = typeDef
                typeDef._constructor = constructor
            make_constructor(self)

    def setup_symbols(self, compiler: Compiler):
        @Entity.method(zc.TypeDef) # TypeDef.add_symbols
        def add_symbols(self, scope):
            name = self.names[0]
            for name in self.names:
                existing_type_objects = compiler.cp.st.find(name.name, zc.Type, scope)
                if len(existing_type_objects) == 0:
                    self._resolved_types = []
                    type_object = zc.Type(name=name.name, alias=name.alias)
                    compiler.add_symbol(name.name, type_object, None, alias=name.alias)
                    self._resolved_types.append(type_object)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties = self.rhs.properties
                        compiler.add_symbol(name.name, self._constructor, None, alias=name.alias)
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
            if isinstance(type, str) or isinstance(parent, str): return
            if type.parents == None: type.parents = []
            if parent not in type.parents: type.parents.append(parent)
            if parent.children == None: parent.children = []
            if type not in parent.children: parent.children.append(type)

    def test_parser(self):
        test("type_0", parse_code("type vec | vector", "TypeDef"), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = vec
                        alias: str = vector
                rhs: TypeRhs = None
            """)
    
        test("type_1", parse_code("type vec | vector = { x, y, z: number =0 }", "TypeDef"), """
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
        
        test("type_2", parse_code("type int > i8, i16", "TypeDef"), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = int
                        alias: str = None
                rhs: TypeRhs
                    TypeChildrenDef
                        children => [i8, i16]
            """)
        
        test("type_3", parse_code("type offset < vector", "TypeDef"), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = offset
                        alias: str = None
                rhs: TypeRhs
                    TypeParentDef
                        parents => [vector]
            """)
        
        test("type_4", parse_code("type evil = no | yes | maybe", "TypeDef"), """
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
        test("type_4a", parse_code("type bit = 0 | 1", "TypeDef"), """
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
        
        test("type_5", parse_code("type str | string = char$$$", "TypeDef"), """
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
    def setup_syntax(self, compiler: Compiler):
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
            EmptyFunctionBody < FunctionBody := "pass"
            Statement := 
            Assignment < Statement :=(lhs:AssignmentLhs)? rhs:Expression
            AssignmentLhs := results:ResultVariable+, assign_op:("=" | "<<") 
            ResultVariable :=
            ResultVariableDef < ResultVariable := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&))
            ResultVariableRef < ResultVariable := variable:VariableRef
            CompositeFunction < Statement:=
            SingleFunctionDef < CompositeFunction := funcDef:FunctionDef&
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
        def can_see_scope(self, scope):
            if self == scope: return True
            if not isinstance(scope, zc.FeatureDef): return False
            if self._owner == scope: return True
            if self._owner == scope: return True
            if self._owner.can_see_scope(scope): return True
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
            function = compiler.find_symbol(long_handle, zc.Function, None, raise_errors=False)
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

        def get_first_name(signature: zc.FunctionSignature) -> str:
            for e in signature.elements:
                if hasattr(e, "name"): return e.name
            return None

        def typed_handle(funcDef) -> str:
            name = ""
            for item in funcDef.signature.elements:
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
        
        def untyped_handle(funcDef) -> str:
            name = ""
            for item in funcDef.signature.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.name)
                elif isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        for param_name in param.names:
                            name += "◦"
            return name    

        def make_function(funcDef, st):
            refToFunc = zc.SingleFunctionDef(funcDef=funcDef)
            statements = zc.FunctionStatements(statements=[refToFunc])
            function = zc.Function( results=funcDef.results, signature=funcDef.signature, body=statements)
            return function
        
        def add_alias_symbol(funcDef, function, st, long_handle, short_handle):
            typedef = funcDef._is_constructor_for_typedef
            name = typedef.names[0].name
            alias = typedef.names[0].alias
            if alias:
                alias_long_handle = long_handle.replace(str(name), str(alias))
                alias_short_handle = short_handle.replace(str(name), str(alias))
                st.add(alias_long_handle, function, None)
                st.add(alias_short_handle, function, None)
                first_lex = get_first_lex(funcDef)
                compiler.report(first_lex, f"\"{alias_long_handle}\" => {function}")
                compiler.report(first_lex, f"\"{alias_short_handle}\" => {function}")
        
        def modify_function(funcDef, existing, st):
            long_handle = funcDef.signature.typed_handle()
            mod = str(funcDef.modifier)
            this_def = zc.SingleFunctionDef(funcDef=funcDef)
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
        def check_types(self, st, scope, errors): 
            self._type = self.variable._type
            compiler.report(self, f"{self._type}")

        @Entity.method(zc.ResultVariableDef) # ResultVariableDef.check_types
        def check_types(self, st, scope, errors): 
            self._type = self.type
            compiler.report(self, f"{self._type}")

        @Entity.method(zc.AssignmentLhs) # AssignmentLhs.check_types
        def check_types(self, st, scope, errors):
            if len(self.results) == 0: self._type = None
            elif len(self.results) == 1: self._type = self.results[0]._type
            else: self._type = zc.MultipleTypes([r._type for r in self.results])
            compiler.report(self, f"{self._type}")

    def test_parser(self):
        test("result_vars_1", parse_code("(a, b: int, k, l: float) =", "FunctionResults"), """
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
    
        test("result_vars_2", parse_code("(int a, b, float k) <<", "FunctionResults"), """
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
        
        test("param_group_0", parse_code("(int a, b=0, float k=0)", "FunctionSignatureParams"), """
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
        
        test("param_group_1", parse_code("(int a, b, float k)", "FunctionSignatureParams"), """
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
        
        test("param_group_2", parse_code("(a, b: int, k: float)", "FunctionSignatureParams"), """
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
        
        test("function_0", parse_code("r = a + b", "Statement"), """
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
        
        test("function_1", parse_code("on (int r) = min (int a, b) { r = if (a < b) then a else b }", "FunctionDef"), """
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
    def setup_syntax(self, compiler: Compiler):
        compiler.grammar.add("""
        TestDef < Component := ">" lhs:Expression ("=>" rhs:Expression)?
        """)

    def test_parser(self):
        test("test_0", parse_code("> a", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[str]
                            a
                rhs: Expression = None
        """)

        test("test_1", parse_code("> a =>", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[str]
                            a
                rhs: Expression
                    !!! premature end (expected rhs:Expression, got 'None' at <eof>)
        """)

        test("test_2", parse_code("> a => b", "TestDef"), """
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

