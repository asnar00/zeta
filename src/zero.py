# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from compiler import *
import zero_classes as zc

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
    on (number r) = (number a) + (number b)
        r = add(a, b)
    on (number r) = (number a) - (number b)
        r = sub(a, b)
    on (number r) = (number a) * (number b)
        r = mul(a, b)
    on (number r) = (number a) / (number b)
        r = div(a, b)

feature VectorMath extends Math
    type vector | vec = 
        number x, y, z = 0
    on (vec r) = (vec a) + (vec b)
        r = vec(a.x + b.x, a.y + b.y, a.z + b.z)
    on (vec r) = (vec a) * (number n)
        r = vec(a.x * n, a.y * n, a.z * n)
    on (vec r) = (vec v) / (number n)
        r = vec(v.x / n, v.y / n, v.z / n)
    on (number d) = (vec a) dot (vec b)
        d = a.x * b.x + a.y * b.y + a.z * b.z
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
    zero = Language(zc)
    zero.add_modules([module_Features(), module_Expressions(), module_Variables(), module_Types(), module_Functions(), module_Tests()])
    test_verbose(False)
    log_max_depth(8)
    zero.setup()
    code = s_test_program
    program = zero.compile(code)

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
    def syntax(self, grammar: Grammar):
        grammar.add("""
            NameDef := name:<identifier> ("|" alias:<identifier>)?
            FeatureDef := "feature" NameDef ("extends" parent:FeatureDef&)? "{" components:Component*; "}"
            Component :=
            ContextDef := "context" NameDef "=" feature:FeatureDef&*,
            Program := components:(FeatureDef | ContextDef)+;
            """)
        
    def methods(self, grammar: Grammar) -> bool:
        @grammar.method(zc.NameDef)
        def add_symbols(self, scope, symbol_table): pass
        
        # def add(self, name: str, element: Any, scope: Any, tag: Dict=None) -> str; return True if we should be scope
        @grammar.method(zc.FeatureDef)
        def add_symbols(self, scope, symbol_table):
            symbol_table.add(self.name, self, scope, alias=self.alias)

        @grammar.method(zc.FeatureDef)
        def can_see_scope(self, scope, symbol_table):
            log(f"can {self} see {scope}")
            featureDef = self
            while True:
                parent = featureDef.parent
                if isinstance(parent, Lex):
                    parent = symbol_table.find_single(parent, zc.FeatureDef, None, [])
                    if parent == None: return False # we'll get a resolve error later
                if parent == scope: return True
                featureDef = parent
                

        @grammar.method(zc.FeatureDef)
        def get_scope(self): return self

    def test(self):
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
    def syntax(self, grammar: Grammar):
        grammar.add("""
            Expression :=
            Constant < Expression := value:(<number> | <string>)
            VariableRef < Expression := variable:Variable& ("." property:VariableRef)?
            Bracketed < Expression := "(" expression:Expression ")"
            FunctionCall < Expression := items:FunctionCallItem+
            FunctionCallItem :=
            FunctionCallConstant < FunctionCallItem := constant:Constant
            FunctionCallOperator < FunctionCallItem := word:<operator>
            FunctionCallWord < FunctionCallItem := word:<identifier>
            FunctionCallVariable < FunctionCallItem := VariableRef
            FunctionCallArguments < FunctionCallItem := "(" arguments:FunctionCallArgument+, ")"
            FunctionCallArgument := (argument:Variable& "=")? value:Expression
                    """)
        
    def methods(self, grammar: Grammar):
        @grammar.method(zc.FunctionCall)
        def validate(self) -> str: # function call must have at least one bracketed term or operator
            n_bracketed = 0; n_operators = 0; n_words = 0
            for item in self.items:
                if isinstance(item, zc.FunctionCallArguments): n_bracketed += 1
                elif isinstance(item, zc.FunctionCallOperator): n_operators += 1
                elif isinstance(item, zc.FunctionCallWord): n_words += 1
            if n_bracketed == 0 and n_operators == 0 and (n_words < 2): return "function call must have at least one argument or operation, or be two or more words"
            return ""
    
        @grammar.method(zc.FunctionCallVariable)
        def validate(self) -> str:
            if self.property == None: return "FunctionCallVariable must have a property"
            return ""
        
        @grammar.method(zc.FunctionCall)
        def add_symbols(self, scope, symbol_table):
            embracket(self)
        
        @grammar.method(zc.VariableRef)
        def get_scope(self):
            var = self.variable
            if isinstance(var.type, Lex): return None
            return var.type

        @grammar.method(zc.FunctionCallVariable)
        def get_scope(self):
            var = self.variable
            if isinstance(var, List):
                log(log_red("var is a list"))
                log(var)
                log_exit()
            if isinstance(var.type, Lex): return None
            return var.type
    
        @grammar.method(zc.FunctionCall)
        def resolve(self, symbol_table, scope, errors, found):
            cf = print_code_formatted(self)
            log(f"fnc.resolve: {cf}")
            replace_variables(self.items, symbol_table, scope, errors, found)

        def embracket(fc: zc.FunctionCall):
            i_operator = find_lowest_ranked_operator(fc.items)
            if i_operator == -1: return fc
            #log(log_green(print_code_formatted(self)))
            before = fc.items[0:i_operator]
            if len(before) > 1: before = [zc.FunctionCall(items=before)]
            after = fc.items[i_operator+1:]
            if len(after) > 1: after = [zc.FunctionCall(items=after)]
            fc.items = before + [fc.items[i_operator]] + after
            return fc
        
        def find_lowest_ranked_operator(items):
            i_found = -1
            lowest_rank = 10240
            for i, item in enumerate(items):
                if isinstance(item, zc.FunctionCallOperator) and item.word != ".":
                    rank = item.word.rank
                    if rank < lowest_rank:
                        lowest_rank = rank
                        i_found = i
            return i_found

        def replace_variables(items, symbol_table, scope, errors, found):
            # first replace any variable names with variables
            for i, item in enumerate(items):
                if isinstance(item, zc.FunctionCallWord):
                    location = item.word.location()
                    var = symbol_table.find_single(item.word, zc.Variable, scope, [])
                    if var:
                        items[i] = zc.FunctionCallVariable(variable=var, property=None)
                        #log(log_green(f"found {var} in {scope} at {location}"))
                        found.append(f"replaced {item.word} with {var} at {location}")

        @grammar.method(zc.Constant)
        def check_type(self, symbol_table, scope, errors) -> zc.Type:
            type_name = "number" if self.value.type=="number" else "string"
            self._type = symbol_table.find_single(type_name, zc.Type, scope, errors)
            if self._type:log(log_green(f"Constant: {self.value} is {self._type}"))
            else: log(log_red(f"Constant: {self.value} is not a valid type"))
        
        @grammar.method(zc.VariableRef)
        def check_type(self, symbol_table, scope, errors) -> zc.Type:
            self._type = self.variable.type
            if self._type: log(log_green(f"VariableRef: {self.variable} is {self._type}"))
        
        @grammar.method(zc.FunctionCallVariable)
        def check_type(self, symbol_table, scope, errors) -> zc.Type:
            self._type = self.variable.type
            if self._type: log(log_green(f"FunctionCallVariable: {self.variable} is {self._type}"))


        def find_functions(fc: zc.FunctionCall, symbol_table, scope, errors, found) -> List[zc.Function]: 
            if fc._resolved_functions: return fc._resolved_functions
            short_sig = find_short_sig(fc)
            location = find_location(fc)
            fc_types = get_param_types(fc, symbol_table)
            log(f" fc_types: {fc_types}")
            for param_type in fc_types:
                if isinstance(param_type, Lex):
                    log(log_red("Flagging: fc_types contains a Lex type."))
                    break
            log(f" short_sig: {short_sig}")
            functions = [f.element for f in symbol_table.find(short_sig, zc.Function, scope)]
            log(f" functions: {functions}")
            fn_types = [f.signature.get_param_types(symbol_table) for f in functions]
            log(f" fn_types: {fn_types}")
            distances = [get_distances(fc_types,fn_types[i]) for i in range(len(fn_types))]
            log(f" distances: {distances}")
            i_best = find_best_positive_distance(distances)
            log(f" i_best: {i_best}")
            if i_best is not None: return [functions[i_best]]
            # filter out functions with None in their distances
            filtered_functions = []
            for i, dist_list in enumerate(distances):
                if None not in dist_list: filtered_functions.append(functions[i])
            log(f" filtered_functions: {filtered_functions}")
            if len(filtered_functions) == 0:
                log(log_red(f"no function found for {print_code_formatted(fc)} in {scope}"))
                errors.append(f"no function found for {print_code_formatted(fc)} in {scope}")
                return []
            fc._constraints = fn_types # dunno what this is really for, but we'll find out later
            log(f" fc._constraints: {fc._constraints}")
            return filtered_functions
        
        def get_param_types(fc: zc.FunctionCall, symbol_table: SymbolTable) -> List[zc.Type]:
            out = []
            for item in fc.items:
                if isinstance(item, zc.FunctionCallVariable):
                    out.append(item.variables[-1].type)
                elif isinstance(item, zc.FunctionCallArguments):
                    for arg in item.arguments:
                        if arg.value:
                            out.append(arg.value.type_of(symbol_table))
            return out
        
        def find_short_sig(fc: zc.FunctionCall) -> str:
            out = ""
            for item in fc.items:
                if isinstance(item, zc.FunctionCallArguments):
                    out += "◦" * len(item.arguments)
                elif isinstance(item, zc.FunctionCallVariable):
                    out += "◦"
                elif hasattr(item, "word"):
                    out += str(item.word)
            return out
        
        def find_location(fc: zc.FunctionCall) -> str:
            for item in fc.items:
                if hasattr(item, "word"):
                    return item.word.location()
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
        
        @grammar.method(zc.FunctionCall)
        def check_type(self, symbol_table, scope, errors):
            log(f"check_type: {print_code_formatted(self)}")
            

    def test(self):
        test("expression_0", parse_code("1", "Expression"), """
            Constant
                value: str = 1
            """)

        test("expression_1", parse_code("a", "Expression"), """
            VariableRef
                variable: Variable => a
                property: VariableRef = None
            """)
        
        test("expression_1a", parse_code("a.b", "Expression"), """
            VariableRef
                variable: Variable => a
                property: VariableRef
                    VariableRef
                        variable: Variable => b
                        property: VariableRef = None
            """)

        test("expression_2", parse_code("a + b", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        word: str = a
                    FunctionCallOperator
                        word: str = +
                    FunctionCallWord
                        word: str = b
            """)
        
        test("expression_2a", parse_code("v.x + v.y", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallVariable
                        variable: Variable => v
                        property: VariableRef
                            VariableRef
                                variable: Variable => x
                                property: VariableRef = None
                    FunctionCallOperator
                        word: str = +
                    FunctionCallVariable
                        variable: Variable => v
                        property: VariableRef
                            VariableRef
                                variable: Variable => y
                                property: VariableRef = None
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
                                word: str = a
                            FunctionCallOperator
                                word: str = +
                            FunctionCallWord
                                word: str = b
            """)

        test("brackets_0", parse_code("(a + b)", "Bracketed"), """
            Bracketed
                expression: Expression
                    FunctionCall
                        items: List[FunctionCallItem]
                            FunctionCallWord
                                word: str = a
                            FunctionCallOperator
                                word: str = +
                            FunctionCallWord
                                word: str = b
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
                                                word: str = a
                                            FunctionCallOperator
                                                word: str = +
                                            FunctionCallWord
                                                word: str = b
            """)
        
        test("expression_3", parse_code("2 - c", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallConstant
                        constant: Constant
                            Constant
                                value: str = 2
                    FunctionCallOperator
                        word: str = -
                    FunctionCallWord
                        word: str = c
        """)

        test("expression_4", parse_code("a + (2 - c)", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        word: str = a
                    FunctionCallOperator
                        word: str = +
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
                                                word: str = -
                                            FunctionCallWord
                                                word: str = c
        """)

        test("expression_5", parse_code("hello(name = \"world\")", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        word: str = hello
                    FunctionCallArguments
                        arguments: List[FunctionCallArgument]
                            FunctionCallArgument
                                argument: Variable => name
                                value: Expression
                                    Constant
                                        value: str = "world"
            """)
    
#--------------------------------------------------------------------------------------------------
# variables

class module_Variables(LanguageModule):
    def syntax(self, grammar: Grammar):
        grammar.add("""
            Variable := type:Type& NameDef "=" value:Expression
            VariableDef < Component := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&)) ("=" value:Expression)?
        """)

    def methods(self, grammar: Grammar):
        @grammar.method(zc.VariableDef)
        def add_symbols(self, scope, symbol_table):
            self._defined_vars = []
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias, value=self.value)
                symbol_table.add(name.name, var, scope)
                if name.alias: symbol_table.add(name.alias, var, scope)
                self._defined_vars.append(var)


    def test(self):
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
    def syntax(self, grammar: Grammar):
        grammar.add("""
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
            TypeRef < Type := type:Type& decorator:("$" | "[]")?
        """)

    def methods(self, grammar: Grammar):
        @grammar.method(zc.TypeEnumDef)
        def validate(self) -> str: 
            return "" if len(self.options) > 1 else "enum must have at least two options"
        
        @grammar.method(zc.TypeDef)
        def add_symbols(self, scope, symbol_table):
            self._resolved_types = []
            for name in self.names:
                existing_type_objects = symbol_table.find(name.name, zc.Type, scope)
                if len(existing_type_objects) == 0:
                    type_object = zc.Type(name=name.name, alias=name.alias)
                    symbol_table.add(name.name, type_object, None, alias=name.alias)
                    self._resolved_types.append(type_object)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties = self.rhs.properties
                        self._constructor = make_constructor(type_object, symbol_table)
                elif len(existing_type_objects) == 1:
                    self._resolved_types.append(existing_type_objects[0].element)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties += self.rhs.properties
                        self._constructor = make_constructor(type_object, symbol_table)
                else:
                    raise Exception(f"symbol clash in TypeDef: {self.name} => {existing_type_objects}")     
            
        @log_suppress
        def make_constructor(type_object: zc.Type, symbol_table: SymbolTable) -> zc.FunctionDef:
            log(f" make_constructor: {type_object.name} | {type_object.alias}")
            name = type_object.name
            result_var = "_" + str(name)[0].lower()
            for prop in type_object.properties:
                for var in prop.names:
                    log(f"  var: {var.name}, type: {prop.type}, default: {print_code_formatted(prop.value)}")
            code = f"on ({name} {result_var}) = {name}("
            for prop in type_object.properties:
                code += f"{prop.type} "
                for var in prop.names: code += f"{var.name}, "
                if code.endswith(", "): code = code[:-2]
                if prop.value: code += f" = {print_code_formatted(prop.value)}"
            code += ") {"
            for prop in type_object.properties:
                for var in prop.names:
                    code += f"{result_var}.{var.name} = {var.name}; "
            code += "}"
            log(code)
            constructor = parse_simple(code, "FunctionDef")
            symbol_table.add_symbols(constructor, None)
            alias_handle = constructor.signature.handle().replace(str(name), str(type_object.alias))
            symbol_table.add(alias_handle, constructor, None)
            return constructor

        @grammar.method(zc.TypeDef)    
        def get_scope(self):
            return self._resolved_types[0] or None
        
        @grammar.method(zc.Type)
        def get_scope(self):
            return self
        
        @grammar.method(zc.TypeDef)
        def disallow_resolve_children(self): 
            return isinstance(self.rhs, zc.TypeAlias)

        @grammar.method(zc.TypeDef)
        def resolve(self, symbol_table, scope, errors, found):
            log(f"typeDef.resolve:\n{print_code_formatted(self)}")
            if isinstance(self.rhs, zc.TypeAlias):
                log(" is alias")
                type_ref = str(self.rhs.type)
                location = self.rhs.type.location()
                ref = ""
                if type_ref.endswith("$"):
                    ref = "$"
                    type_ref = type_ref[:-1]
                log(f" type_ref: {type_ref}")
                type = symbol_table.find_single(type_ref, zc.Type, None, errors)
                if type:
                    self.rhs.type = type
                    self._resolved_types[0]._alias_type = type
                    self._resolved_types[0]._alias_modifier = ref
                    found.append(f"resolved alias {type_ref} to {self._resolved_types[0]._alias_type} at {location}")       
            elif isinstance(self.rhs, zc.TypeParentDef):
                #log(" is parent")
                for parent in self.rhs.parents:
                    assert_parent_type(self._resolved_types[0], parent)
            elif isinstance(self.rhs, zc.TypeChildrenDef):
                #log(" is children")
                for child in self.rhs.children:
                    assert_parent_type(child, self._resolved_types[0])
            
        def assert_parent_type(type: zc.Type, parent: zc.Type):
            if isinstance(type, str) or isinstance(parent, str): return
            log(log_green(f"asserting that {type.name} is a child of {parent.name}"))
            if type.parents == None: type.parents = []
            if parent not in type.parents: type.parents.append(parent)
            if parent.children == None: parent.children = []
            if type not in parent.children: parent.children.append(type)

        @grammar.method(zc.Type)
        def find_relationship(self, type_b: zc.Type) -> int|None:
            depth = find_relationship_rec(self, type_b, 0)
            if depth != None: return depth
            depth = find_relationship_rec(type_b, self, 0)
            if depth != None: return -depth
            return None
        
        def find_relationship_rec(child: zc.Type, parent: zc.Type, depth: int) -> int|None:
            if child == parent: return depth
            if child.parents == None: return None
            for p in child.parents:
                result = find_relationship_rec(p, parent, depth + 1)
                if result: return result
            return None

    def test(self):
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
        
        test("type_5", parse_code("type str | string = char$", "TypeDef"), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = str
                        alias: str = string
                rhs: TypeRhs
                    TypeAlias
                        type: Type => char$
            """)

#--------------------------------------------------------------------------------------------------
# functions

class module_Functions(LanguageModule):
    def syntax(self, grammar: Grammar):
        grammar.add("""
            Function := "_function" results:FunctionResults signature:FunctionSignature body:FunctionBody
            FunctionDef < Component := FunctionModifier results:FunctionResults? signature:FunctionSignature body:FunctionBody
            FunctionModifier := modifier:("on" | "before" | "after" | "replace")
            FunctionResults := "(" results:FunctionResultVariableDef+, ")" assign_op:("=" | "<<")
            FunctionResultVariableDef := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&))
            FunctionSignature := elements:FunctionSignatureElement+
            FunctionSignatureElement :=
            FunctionSignatureWord < FunctionSignatureElement := word:(<identifier> | <operator>)
            FunctionSignatureParams < FunctionSignatureElement := "(" params:VariableDef+, ")"
            FunctionBody := 
            FunctionStatements < FunctionBody := "{" statements:Statement*; "}"
            EmptyFunctionBody < FunctionBody := "pass"
            Statement := 
            Assignment < Statement :=(lhs:AssignmentLhs)? rhs:Expression
            AssignmentLhs := variables:ResultVariable+, assign_op:("=" | "<<") 
            ResultVariable :=
            ResultVariableRef < ResultVariable := VariableRef
            ResultVariableDef < ResultVariable := ((type:Type& NameDef) | (NameDef ":" type:Type&))
            CompositeFunction < Statement:=
            SingleFunctionDef < CompositeFunction := funcDef:FunctionDef&
            SequenceFunctionDef < CompositeFunction := "seq" "(" comps:CompositeFunction+, ")"
            ParallelFunctionDef < CompositeFunction := "par" "(" comps:CompositeFunction+, ")"
        """)

    def methods(self, grammar: Grammar):
        @grammar.method(zc.FunctionResultVariableDef)
        def add_symbols(self, scope, symbol_table):
            self._defined_vars = []
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias)
                symbol_table.add(name.name, var, scope, alias=name.alias)
                self._defined_vars.append(var)

        @grammar.method(zc.ResultVariableDef)
        def add_symbols(self, scope, symbol_table):
            var = zc.Variable(type=self.type, name=self.name, alias=self.alias)
            symbol_table.add(var.name, var, scope, alias=var.alias)
            self._defined_var = var

        @grammar.method(zc.ResultVariableRef)
        def get_scope(self):
            var = self.variable
            if isinstance(var.type, Lex): return None
            return var.type
        
        @grammar.method(zc.FunctionSignature)
        def handle(self) -> str:
            name = ""
            for item in self.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.word)
                elif isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        for param_name in param.names:
                            name += "◦"
            return name      
        
        @grammar.method(zc.FunctionSignature)
        def typed_handle(self) -> str:
            name = ""
            for item in self.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.word) + " "
                elif isinstance(item, zc.FunctionSignatureParams):
                    if name.endswith(" "): name = name[:-1]
                    brace = "("
                    for param in item.params:
                        for param_name in param.names:
                            brace += str(param.type).replace("Type(", "").replace(")", "")+ ", "
                    if brace.endswith(", "): brace = brace[:-2]
                    brace += ")"
                    name += brace
            return name

        @grammar.method(zc.FunctionDef)
        def short_name(self) -> str:
            out = ""
            if hasattr(self, "_owner") and self._owner: out += str(self._owner.name) + "."
            out += self.signature.typed_handle() if self.signature else ""
            return out
        
        @grammar.method(zc.FunctionDef)
        def can_see_scope(self, scope, symbol_table):
            if self == scope: return True
            if not isinstance(scope, zc.FeatureDef): return False
            if self._owner == scope: return True
            if self._owner == scope: return True
            if self._owner.can_see_scope(scope, symbol_table): return True
            return False

        @grammar.method(zc.FunctionDef)
        def add_symbols(self, scope, symbol_table):
            #log(log_green(f"functionDef.add_symbols: {self.signature.typed_handle()} in {scope}"))
            long_handle = self.signature.typed_handle()
            short_handle = self.signature.handle()
            symbol_table.add(long_handle, self, scope)
            symbol_table.add(short_handle, self, scope)
            self._owner = scope
            function = symbol_table.find_single(long_handle, zc.Function, None, [])
            if function == None:
                function = self.make_function(symbol_table)
                symbol_table.add(long_handle, function, None)
                symbol_table.add(short_handle, function, None)
            else:
                self.modify_function(function, symbol_table)

        @grammar.method(zc.FunctionDef)
        def modify_function(self, existing, symbol_table):
            long_handle = self.signature.typed_handle()
            mod = str(self.modifier)
            this_def = zc.SingleFunctionDef(funcDef=self)
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
            
        @grammar.method(zc.FunctionDef)
        def make_function(self, symbol_table: SymbolTable):
            funcDef = zc.SingleFunctionDef(funcDef=self)
            statements = zc.FunctionStatements(statements=[funcDef])
            function = zc.Function( results=self.results, signature=self.signature, body=statements)
            symbol_table.add(handle, function, None)
            return function

        @grammar.method(zc.FunctionSignature)
        def get_param_types(self, symbol_table) -> List[zc.Type]:
            out = []
            for item in self.elements:
                if isinstance(item, zc.FunctionSignatureParams):
                    for param in item.params:
                        param_type = param.type
                        if isinstance(param_type, Lex):
                            param_type = symbol_table.find_single(param_type, zc.Type, None, None)
                        for param_name in param.names:
                            out.append(param_type)
            return out

        @grammar.method(zc.FunctionDef)
        def get_scope(self): return self

        @grammar.method(zc.Function)
        def get_scope(self): return self

        @grammar.method(zc.Function)
        def short_name(self) -> str:
            return self.signature.typed_handle() if self.signature else ""
    
    def test(self):
        test("result_vars_1", parse_code("(a, b: int, k, l: float) =", "FunctionResults"), """
            FunctionResults
                results: List[FunctionResultVariableDef]
                    FunctionResultVariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                    FunctionResultVariableDef
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
                results: List[FunctionResultVariableDef]
                    FunctionResultVariableDef
                        type: Type => int
                        names: List[NameDef]
                            NameDef
                                name: str = a
                                alias: str = None
                            NameDef
                                name: str = b
                                alias: str = None
                    FunctionResultVariableDef
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
        
        test("function_0", parse_code("on (int r) = min (int a, b) { r = if (a < b) then a else b }", "FunctionDef"), """
            FunctionDef
                modifier: str = on
                results: FunctionResults
                    FunctionResults
                        results: List[FunctionResultVariableDef]
                            FunctionResultVariableDef
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
                                word: str = min
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
                                        variables: List[ResultVariable]
                                            ResultVariableRef
                                                variable: Variable => r
                                                property: VariableRef = None
                                        assign_op: str = =
                                rhs: Expression
                                    FunctionCall
                                        items: List[FunctionCallItem]
                                            FunctionCallWord
                                                word: str = if
                                            FunctionCallArguments
                                                arguments: List[FunctionCallArgument]
                                                    FunctionCallArgument
                                                        argument: Variable = None
                                                        value: Expression
                                                            FunctionCall
                                                                items: List[FunctionCallItem]
                                                                    FunctionCallWord
                                                                        word: str = a
                                                                    FunctionCallOperator
                                                                        word: str = <
                                                                    FunctionCallWord
                                                                        word: str = b
                                            FunctionCallWord
                                                word: str = then
                                            FunctionCallWord
                                                word: str = a
                                            FunctionCallWord
                                                word: str = else
                                            FunctionCallWord
                                                word: str = b
            """)
    
#--------------------------------------------------------------------------------------------------
# tests

class module_Tests(LanguageModule):
    def syntax(self, grammar: Grammar):
        grammar.add("""
        TestDef < Component := ">" lhs:Expression ("=>" rhs:Expression)?
        """)

    def test(self):
        test("test_0", parse_code("> a", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variable: Variable => a
                        property: VariableRef = None
                rhs: Expression = None
        """)

        test("test_1", parse_code("> a =>", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variable: Variable => a
                        property: VariableRef = None
                rhs: Expression
                    !!! premature end (expected rhs:Expression, got 'None' at <eof>)
        """)

        test("test_2", parse_code("> a => b", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variable: Variable => a
                        property: VariableRef = None
                rhs: Expression
                    VariableRef
                        variable: Variable => b
                        property: VariableRef = None
        """)

