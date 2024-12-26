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
    on (number r) = add(number a, b)
        nop()
    on (number r) = sub(number a, b)
        nop()
    on (number r) = mul(number a, b)
        nop()
    on (number r) = div(number a, b)
        nop()
    on (number r) = sqrt(number n)
        nop()

context MyContext = Program, Hello, Goodbye, Math, VectorMath, Backend
"""

# this is just scribble: but it's a nice way to define dependent types eg. i32, i64, etc
s_test_concrete = """
feature ConcreteMath extends Math
    type bit = 0 | 1
    type u(n) = bit[n]
    type i(n) =
        bit sign
        u(n-1) val
    type ieee(e, m) =
        bit sign
        u(e) exponent
        u(m) mantissa
    type f16 = ieee(5, 10)
    type f32 = ieee(8, 23)
    type f64 = ieee(11, 52)
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
            Feature := NameDef parent:Feature& types:Type&* variables:Variable&* functions:Function&*
            FeatureDef := "feature" NameDef ("extends" parent:Feature&)? "{" components:Component*; "}"
            Component
            ContextDef := "context" NameDef "=" feature:Feature&*,
            Program := components:(FeatureDef | ContextDef)+;
            """)
        
    def methods(self, grammar: Grammar) -> bool:
        @grammar.method(zc.NameDef)
        def add_symbols(self, scope, symbol_table): pass
        
        # def add(self, name: str, element: Any, scope: Any, tag: Dict=None) -> str; return True if we should be scope
        @grammar.method(zc.FeatureDef)
        def add_symbols(self, scope, symbol_table):
            feature = zc.Feature(name=self.name, alias=self.alias, parent=self.parent)
            symbol_table.add(self.name, feature, scope, alias=self.alias)
        @grammar.method(zc.FeatureDef)
        def get_scope(self): return self

    def test(self):
        test("feature_0", parse_code("", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected 'feature', got 'None' at <eof>)
                name: str = None
                alias: str = None
                parent: Feature = None
                components: List[Component] = None
            """)
        
        test("feature_1", parse_code("feature", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected NameDef, got 'None' at <eof>)
                name: str = None
                alias: str = None
                parent: Feature = None
                components: List[Component] = None
            """)

        test("feature_2", parse_code("feature MyFeature", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected '{', got 'None' at <eof>)
                name: str = MyFeature
                alias: str = None
                parent: Feature = None
                components: List[Component] = None
            """)

        test("feature_3", parse_code("feature MyFeature extends", "FeatureDef"), """
            FeatureDef
                !!! mismatch (expected '{', got 'extends' at :...:19)
                name: str = MyFeature
                alias: str = None
                parent: Feature
                    !!! premature end (expected parent:Feature&, got 'None' at <eof>)
                components: List[Component] = None
            """)
        
        test("feature_4", parse_code("feature MyFeature extends Another", "FeatureDef"), """
            FeatureDef
                !!! premature end (expected '{', got 'None' at <eof>)
                name: str = MyFeature
                alias: str = None
                parent: Feature => Another
                components: List[Component] = None        
            """)
        
        test("feature_5", parse_code("feature MyFeature {}", "FeatureDef"), """
            FeatureDef
                name: str = MyFeature
                alias: str = None
                parent: Feature = None
                components: List[Component] = []
            """)
        
        test("feature_6", parse_code("feature MyFeature extends Another {}", "FeatureDef"), """
            FeatureDef
                name: str = MyFeature
                alias: str = None
                parent: Feature => Another
                components: List[Component] = []
            """)
        test("context_0", parse_code("context MyContext = Hello, Goodbye, Countdown", "ContextDef"), """
            ContextDef
                name: str = MyContext
                alias: str = None
                feature: List[Feature]
                    => Hello
                    => Goodbye
                    => Countdown
             """)


#--------------------------------------------------------------------------------------------------
# Expressions

class module_Expressions(LanguageModule):
    def syntax(self, grammar: Grammar):
        grammar.add("""
            Expression :=
            Constant < Expression := value:(<number> | <string>)
            VariableRef < Expression := variables:Variable&+.
            Bracketed < Expression := "(" expression:Expression ")"
            FunctionCall < Expression := items:FunctionCallItem+
            FunctionCallItem :=
            FunctionCallConstant < FunctionCallItem := constant:Constant
            FunctionCallOperator < FunctionCallItem := word:<operator>
            FunctionCallWord < FunctionCallItem := word:<identifier>
            FunctionCallVariable < FunctionCallItem := variables:Variable&+.
            FunctionCallArguments < FunctionCallItem := "(" arguments:FunctionCallArgument+, ")"
            FunctionCallArgument := (argument:Variable& "=")? value:Expression
                    """)
        
    def methods(self, grammar: Grammar):
        # function call must have at least one bracketed term or operator
        @grammar.method(zc.FunctionCall)
        def validate(self) -> str:
            n_bracketed = 0; n_operators = 0; n_words = 0
            for item in self.items:
                if isinstance(item, zc.FunctionCallArguments): n_bracketed += 1
                elif isinstance(item, zc.FunctionCallOperator): n_operators += 1
                elif isinstance(item, zc.FunctionCallWord): n_words += 1
            if n_bracketed == 0 and n_operators == 0 and (n_words < 2): return "function call must have at least one argument or operation, or be two or more words"
            return ""
    
        @grammar.method(zc.FunctionCallVariable)
        def validate(self) -> str:
            if len(self.variables) < 2: return "function-call variable must have two or more names"
            return ""
        
        @grammar.method(zc.VariableRef)
        def resolve(self, symbol_table, scope, errors):
            log("vr.resolve", print_code_formatted(self))
            self.variables = resolve_variable_list(self.variables, symbol_table, scope, errors)
        
        @grammar.method(zc.FunctionCallVariable)
        def resolve(self, symbol_table, scope, errors):
            self.variables =resolve_variable_list(self.variables, symbol_table, scope, errors)
        
        def resolve_variable_list(variables, symbol_table, scope, errors):
            if isinstance(variables[0], zc.Variable): return variables
            resolved = []
            for name in variables:
                var = symbol_table.find_single(name, zc.Variable, scope, errors)
                if var:
                    resolved.append(var)
                    var_type = symbol_table.find_single(var.type, zc.Type, scope, errors)
                    if var_type: scope = var_type
                else:
                    log(log_red(f"no variable found for {name} in {scope}"))
            return resolved
    
        @grammar.method(zc.FunctionCall)
        def resolve(self, symbol_table, scope, errors):
            cf = print_code_formatted(self)
            log(f"fnc.resolve: {cf}")
            replace_variables(self.items, symbol_table, scope, errors)
            self.embracket()
            for item in self.items:
                if hasattr(item, "resolve"): item.resolve(symbol_table, scope, errors)
            self._resolved_function = None
            self._resolved_function = find_function(self, symbol_table, scope, errors)
            return False
        
        @grammar.method(zc.FunctionCallArguments)
        def resolve(self, symbol_table, scope, errors):
            for argument in self.arguments:
                if argument.value and hasattr(argument.value, "resolve"): argument.value.resolve(symbol_table, scope, errors)
            return False
        
        @grammar.method(zc.Bracketed)
        def resolve(self, symbol_table, scope, errors):
            if self.expression: self.expression.resolve(symbol_table, scope, errors)
            return False

        @grammar.method(zc.FunctionCall)
        def embracket(self):
            i_operator = find_lowest_ranked_operator(self.items)
            if i_operator == -1: return self
            #log(log_green(print_code_formatted(self)))
            before = self.items[0:i_operator]
            if len(before) > 1: before = [zc.FunctionCall(items=before).embracket()]
            after = self.items[i_operator+1:]
            if len(after) > 1: after = [zc.FunctionCall(items=after).embracket()]
            self.items = before + [self.items[i_operator]] + after
            return self
        
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

        def replace_variables(items, symbol_table, scope, errors):
            # first replace any variable names with variables
            for i, item in enumerate(items):
                if isinstance(item, zc.FunctionCallWord):
                    var = symbol_table.find_single(item.word, zc.Variable, scope, [])
                    if var:
                        items[i] = zc.FunctionCallVariable([var])

        @grammar.method(zc.Expression)
        def type_of(self) -> str:
            if isinstance(self, zc.Constant):
                return "number" if self.value.type=="number" else "string"
            elif isinstance(self, zc.VariableRef) or isinstance(self, zc.FunctionCallVariable):
                var = self.variables[-1]
                log(f" var: {var}, type: {var.type}")
                return str(var.type)
            elif isinstance(self, zc.Bracketed):
                return self.expression.type_of()
            elif isinstance(self, zc.FunctionCall):
                function = self._resolved_function
                if function: return function.type_of()
                else: return "unresolved"
            else: return "unknown"

        def find_function(fc: zc.FunctionCall, symbol_table, scope, errors) -> zc.Function:
            if fc._resolved_function: return fc._resolved_function
            sig = ""
            location = ""
            for item in fc.items:
                if location == "" and hasattr(item, "word"):
                    location = item.word.location()
                if hasattr(item, "type_of"): sig += "(" + item.type_of() + ")_"
                else: sig += str(item.word) + "_"
            if len(sig) > 0: sig = sig[:-1]
            log(f" sig: {sig}")
            function = find_function_from_signature(sig, symbol_table, scope, errors, location)
            return function
        
        def find_function_from_signature(sig: str, symbol_table: SymbolTable, scope: Any, errors: List[str], location) -> zc.Function:
            found = symbol_table.find(sig, zc.Function, scope)
            if len(found) == 0:
                log(log_red(f" no function found for {sig} at {location}"))
                errors.append(f"no function found for {sig} at {location}")
                return None
            elif len(found) > 1:
                log(log_red(f" multiple functions found for {sig} at {location}"))
                return None
            else:
                log(log_green(f" found function {found[0]} for {sig} at {location}"))
                return found[0].element
            
        @grammar.method(zc.FunctionCallArguments)
        def type_of(self) -> str:
            out = ""
            for argument in self.arguments:
                if argument.value and hasattr(argument.value, "type_of"):
                    out += argument.value.type_of() + "_"
            if len(out) > 0: out = out[:-1]
            return out
        
        @grammar.method(zc.FunctionCallVariable)
        def type_of(self) -> str:
            return str(self.variables[-1].type)
        
        @grammar.method(zc.FunctionCall)
        def type_of(self) -> str:
            if self._resolved_function: return self._resolved_function.type_of()
            return "unresolved"

    def test(self):
        test("expression_0", parse_code("1", "Expression"), """
            Constant
                value: str = 1
            """)

        test("expression_1", parse_code("a", "Expression"), """
            VariableRef
                variables: List[Variable]
                    => a
            """)
        
        test("expression_1a", parse_code("a.b", "Expression"), """
            VariableRef
                variables: List[Variable]
                    => a
                    => b
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
                        variables: List[Variable]
                            => v
                            => x
                    FunctionCallOperator
                        word: str = +
                    FunctionCallVariable
                        variables: List[Variable]
                            => v
                            => y
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
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias, value=self.value)
                symbol_table.add(name.name, var, scope)
                if name.alias: symbol_table.add(name.alias, var, scope)

        @grammar.method(zc.Variable)
        def print_code(self):
            return str(self.name)


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
            Type := NameDef properties:Variable&* parents:Type&* children:Type&* options:<identifier>+
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
                        make_constructor(type_object, symbol_table)
                elif len(existing_type_objects) == 1:
                    self._resolved_types.append(existing_type_objects[0].element)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties += self.rhs.properties
                        make_constructor(type_object, symbol_table)
                else:
                    raise Exception(f"symbol clash in TypeDef: {self.name} => {existing_type_objects}")     
            
        #@log_indent
        def make_constructor(type_object: zc.Type, symbol_table: SymbolTable):
            results = zc.FunctionResults(
                results = [zc.FunctionResultVariableDef(type=type_object.name, names=[zc.NameDef(name="r")])], 
                assign_op = "="
            )
            signature = zc.FunctionSignature(elements=[
                zc.FunctionSignatureWord(word=type_object.name),
                zc.FunctionSignatureParams(params=type_object.properties)
            ])
            statements = []
            for prop in type_object.properties:
                for name in prop.names:
                    #log(f"name: {name.name}")
                    out_var = zc.ResultVariableRef(variables=["r", name.name])
                    lhs = zc.StatementLhs(variables=[out_var], assign_op="=")
                    rhs = zc.VariableRef(variables=[name])
                    statements.append(zc.Statement(lhs=lhs, rhs=rhs))
            body = zc.FunctionBody(statements=statements)
            func = zc.Function(handle=str(type_object.name)+"_", results=results, signature=signature, body=body)
            #log(dbg_entity(func))
            symbol_table.replace(name=str(type_object.name), element=func, scope=None, alias=str(type_object.alias), tag={"i_word": 0})
            type_object._constructor = func
            handle = signature.handle()
            log(f"adding constructor {handle} to symbol table")
            symbol_table.add(handle, func, None)
            alias_handle = handle.replace(str(type_object.name), str(type_object.alias))
            log(f"adding alias {alias_handle} to symbol table")
            symbol_table.add(alias_handle, func, None)

        @grammar.method(zc.TypeDef)    
        def get_scope(self):
            return self._resolved_types[0] or None
        
        @grammar.method(zc.TypeDef)
        def resolve(self, symbol_table, scope, errors):
            log(f"typeDef.resolve:\n{print_code_formatted(self)}")
            #log(f"resolved types: {self._resolved_types}")
            if isinstance(self.rhs, zc.TypeAlias):
                type_ref = str(self.rhs.type)
                ref = ""
                if type_ref.endswith("$"):
                    ref = "$"
                    type_ref = type_ref[:-1]
                type = symbol_table.find_single(type_ref, zc.Type, None, errors)
                if type:
                    self.rhs.type = type
                    self._resolved_types[0]._alias_type = type
                    self._resolved_types[0]._alias_modifier = ref
                    #log(log_green(f"resolved alias {type_ref} to {self._resolved_types[0]._alias_type}"))
            elif isinstance(self.rhs, zc.StructDef):
                constructor = self._resolved_types[0]._constructor
                pass #log("struct")
            elif isinstance(self.rhs, zc.TypeParentDef):
                pass
            elif isinstance(self.rhs, zc.TypeChildrenDef):
                resolved_children = []
                for child in self.rhs.children:
                    found_type = symbol_table.find_single(child, zc.Type, None, errors)
                    if found_type:
                        resolved_children.append(found_type)
                self._resolved_types[0].children = resolved_children
            elif isinstance(self.rhs, zc.TypeEnumDef):
                if self._resolved_types[0].options == None:
                    self._resolved_types[0].options = self.rhs.options
                else:
                    self._resolved_types[0].options += self.rhs.options
            return False
    
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
                        children: List[Type]
                            => i8
                            => i16
            """)
        
        test("type_3", parse_code("type offset < vector", "TypeDef"), """
            TypeDef
                names: List[NameDef]
                    NameDef
                        name: str = offset
                        alias: str = None
                rhs: TypeRhs
                    TypeParentDef
                        parents: List[Type]
                            => vector
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
            Function := handle:<identifier> results:FunctionResults signature:FunctionSignature body:FunctionBody
            FunctionDef < Component := FunctionModifier results:FunctionResults? signature:FunctionSignature body:FunctionBody
            FunctionModifier := modifier:("on" | "before" | "after" | "replace")
            FunctionResults := "(" results:FunctionResultVariableDef+, ")" assign_op:("=" | "<<")
            FunctionResultVariableDef := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&))
            FunctionSignature := elements:FunctionSignatureElement+
            FunctionSignatureElement :=
            FunctionSignatureWord < FunctionSignatureElement := word:(<identifier> | <operator>)
            FunctionSignatureParams < FunctionSignatureElement := "(" params:VariableDef+, ")"
            FunctionBody := "{" statements:Statement*; "}"
            Statement := (lhs:StatementLhs)? rhs:Expression
            StatementLhs := variables:ResultVariable+, assign_op:("=" | "<<") 
            ResultVariable :=
            ResultVariableRef < ResultVariable := VariableRef
            ResultVariableDef < ResultVariable := ((type:Type& NameDef) | (NameDef ":" type:Type&))
        """)

    def methods(self, grammar: Grammar):
        @grammar.method(zc.FunctionResultVariableDef)
        def add_symbols(self, scope, symbol_table):
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias)
                symbol_table.add(name.name, var, scope, alias=name.alias)

        @grammar.method(zc.ResultVariableDef)
        def add_symbols(self, scope, symbol_table):
            var = zc.Variable(type=self.type, name=self.name, alias=self.alias)
            symbol_table.add(var.name, var, scope, alias=var.alias)

        @grammar.method(zc.FunctionSignature)
        def handle(self) -> str:
            name = ""
            for item in self.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.word) + "_"
                elif isinstance(item, zc.FunctionSignatureParams):
                    brace = "("
                    for param in item.params:
                        for param_name in param.names:
                            brace += str(param.type) + "_"
                    if brace[-1] == "_": brace = brace[:-1]
                    brace += ")_"
                    name += brace
            return name[:-1]
        
        @grammar.method(zc.FunctionSignature)
        def untyped_handle(self) -> str:
            name = ""
            for item in self.elements:
                if isinstance(item, zc.FunctionSignatureWord):
                    name += str(item.word)
                elif isinstance(item, zc.FunctionSignatureParams):
                    name += "◦"
            return name       
        
        @grammar.method(zc.FunctionDef)
        def short_name(self) -> str:
            return self.signature.untyped_handle() if self.signature else ""

        @grammar.method(zc.FunctionDef)
        def add_symbols(self, scope, symbol_table):
            #log(f"add_symbols: {self.signature.handle()}")
            handle = self.signature.handle()
            functions = symbol_table.find(handle, zc.Function,None)
            if len(functions) == 0:
                function = zc.Function(handle=handle, results=self.results, signature=self.signature, body=self.body)
                symbol_table.add(handle, function, None)
                i_word = 0
                for element in self.signature.elements:
                    if isinstance(element, zc.FunctionSignatureWord):
                        symbol_table.add(element.word, function, None, tag={"i_word": i_word})
                    i_word += 1
            else:
                function = functions[0]
                log(f"TODO: function {handle} exists already; extending")

        @grammar.method(zc.FunctionDef)
        def get_scope(self): return self

        @grammar.method(zc.Function)
        def short_name(self) -> str:
            return self.signature.untyped_handle() if self.signature else ""
        
        @grammar.method(zc.Function)
        def type_of(self) -> str:
            fr = self.results.results
            out = ""
            for r in fr: out += str(r.type.name) + "_"
            return out[:-1]
    
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
                    FunctionBody
                        statements: List[Statement]
                            Statement
                                lhs: StatementLhs
                                    StatementLhs
                                        variables: List[ResultVariable]
                                            ResultVariableRef
                                                variables: List[Variable]
                                                    => r
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
                        variables: List[Variable]
                            => a
                rhs: Expression = None
        """)

        test("test_1", parse_code("> a =>", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[Variable]
                            => a
                rhs: Expression
                    !!! premature end (expected rhs:Expression, got 'None' at <eof>)
        """)

        test("test_2", parse_code("> a => b", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variables: List[Variable]
                            => a
                rhs: Expression
                    VariableRef
                        variables: List[Variable]
                            => b
        """)

