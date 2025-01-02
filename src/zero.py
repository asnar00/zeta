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
        
    def scope(self):
        @self.method(zc.FeatureDef)
        def get_scope(self): return self
    

    def symbols(self):
        @self.method(zc.FeatureDef)
        def add_symbols(self, scope, st: SymbolTable):
            st.add(self.name, self, scope, alias=self.alias)

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
            FunctionCallOperator < FunctionCallItem := name:<operator>
            FunctionCallWord < FunctionCallItem := name:<identifier>
            FunctionCallVariable < FunctionCallItem := variable:VariableRef
            FunctionCallArguments < FunctionCallItem := "(" arguments:FunctionCallArgument+, ")"
            FunctionCallArgument := (argument:Variable& "=")? value:Expression
                    """)
        
    def validate(self):
        @self.method(zc.FunctionCall)
        def validate(self) -> str: # function call must have at least one bracketed term or operator
            n_bracketed = 0; n_operators = 0; n_words = 0
            for item in self.items:
                if isinstance(item, zc.FunctionCallArguments): n_bracketed += 1
                elif isinstance(item, zc.FunctionCallOperator): n_operators += 1
                elif isinstance(item, zc.FunctionCallWord): n_words += 1
            if n_bracketed == 0 and n_operators == 0 and (n_words < 2): return "function call must have at least one argument or operation, or be two or more words"
            return ""
    
        @self.method(zc.FunctionCallVariable)
        def validate(self) -> str:
            if self.variable.property == None: return "FunctionCallVariable must have a property"
            return ""
        
    def naming(self):
        @self.method(zc.VariableRef)
        def get_name(self) -> str:
            out = ""
            if isinstance(self.variable, Lex): 
                out += str(self.variable)
                if self.property: out += "." + self.property.get_name()
            elif self.variable:
                return self.variable.get_name()
            return out
        
    def scope(self):
        @self.method(zc.VariableRef)
        def get_scope(self): return self

    def symbols(self):
        @self.method(zc.FunctionCall)
        def add_symbols(self, scope, st: SymbolTable):
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
                #log(log_green(print_code_formatted(self)))
                before = fc.items[0:i_operator]
                if len(before) > 1: before = [zc.FunctionCall(items=before)]
                after = fc.items[i_operator+1:]
                if len(after) > 1: after = [zc.FunctionCall(items=after)]
                fc.items = before + [fc.items[i_operator]] + after
                return fc
            embracket(self)
            

    def test_parser(self):
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
                                variable: Variable => v
                                property: VariableRef
                                    VariableRef
                                        variable: Variable => x
                                        property: VariableRef = None
                    FunctionCallOperator
                        name: str = +
                    FunctionCallVariable
                        variable: VariableRef
                            VariableRef
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
    
#--------------------------------------------------------------------------------------------------
# variables

class module_Variables(LanguageModule):
    def syntax(self, grammar: Grammar):
        grammar.add("""
            Variable := type:Type& NameDef "=" value:Expression
            VariableDef < Component := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&)) ("=" value:Expression)?
        """)


    def symbols(self):
        @self.method(zc.VariableDef) # VariableDef.add_symbols
        def add_symbols(self, scope, st):
            self._defined_vars = []
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias, value=self.value)
                st.add(name.name, var, scope, alias=name.alias)
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

    def validate(self):
        @self.method(zc.TypeEnumDef)             # TypeEnumDef.validate
        def validate(self) -> str: 
            return "" if len(self.options) > 1 else "enum must have at least two options"
        
    def scope(self):
        @self.method(zc.TypeDef) #TypeDef.get_scope
        def get_scope(self) -> str: return self

    def generate(self):
        @self.method(zc.TypeDef)
        def generate(self):
            if not isinstance(self.rhs, zc.StructDef): return
            log(log_orange(f"TypeDef.generate: {self.names[0].name}"))
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
                constructor = parse_simple(code, "FunctionDef")
                typeDef._constructor = constructor
            make_constructor(self)

    def symbols(self):
        @self.method(zc.TypeDef)
        def add_symbols(self, scope, st):
            log(log_orange(f"TypeDef.add symbols: {self.names[0].name}"))
            name = self.names[0]
            for name in self.names:
                existing_type_objects = st.find(name.name, zc.Type, scope)
                if len(existing_type_objects) == 0:
                    self._resolved_types = []
                    type_object = zc.Type(name=name.name, alias=name.alias)
                    st.add(name.name, type_object, None, alias=name.alias)
                    self._resolved_types.append(type_object)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties = self.rhs.properties
                elif len(existing_type_objects) == 1:
                    self._resolved_types.append(existing_type_objects[0].element)
                    if isinstance(self.rhs, zc.StructDef):
                        type_object.properties += self.rhs.properties
                else:
                    raise Exception(f"symbol clash in TypeDef: {self.name} => {existing_type_objects}") 

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
            FunctionResults := "(" results:ResultVariableDef+, ")" assign_op:("=" | "<<")
            FunctionSignature := elements:FunctionSignatureElement+
            FunctionSignatureElement :=
            FunctionSignatureWord < FunctionSignatureElement := name:(<identifier> | <operator>)
            FunctionSignatureParams < FunctionSignatureElement := "(" params:VariableDef+, ")"
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

    def naming(self):
        @self.method(zc.FunctionDef)
        def get_name(self) -> str:
            return self.signature.typed_handle() if self.signature else ".."
        
        @self.method(zc.FunctionSignature)
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
        
    def scope(self):
        @self.method(zc.FunctionDef)
        def get_scope(self): return self

        @self.method(zc.FunctionDef)
        def can_see_scope(self, scope, st: SymbolTable):
            if self == scope: return True
            if not isinstance(scope, zc.FeatureDef): return False
            if self._owner == scope: return True
            if self._owner == scope: return True
            if self._owner.can_see_scope(scope, st): return True
            return False
        
    def symbols(self):
        @self.method(zc.ResultVariableDef) # ResultVariableDef.add_symbols
        def add_symbols(self, scope, st):
            self._defined_vars = []
            for name in self.names:
                var = zc.Variable(type=self.type, name=name.name, alias=name.alias)
                st.add(name.name, var, scope, alias=name.alias)
                self._defined_vars.append(var)

        @self.method(zc.FunctionDef) # FunctionDef.add_symbols
        def add_symbols(self, scope, st):
            #log(log_green(f"functionDef.add_symbols: {self.signature.typed_handle()} in {scope}"))
            long_handle = typed_handle(self)
            short_handle = untyped_handle(self)
            st.add(long_handle, self, scope)
            st.add(short_handle, self, scope)
            self._owner = scope
            function = st.find_single(long_handle, zc.Function, None, [])
            if function == None:
                function = make_function(self, st)
                self._function = function
                st.add(long_handle, function, None)
                st.add(short_handle, function, None)
            else:
                modify_function(self, function, st)

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
                            brace += str(param.type).replace("Type(", "").replace(")", "")+ ", "
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
        
        test("function_0", parse_code("on (int r) = min (int a, b) { r = if (a < b) then a else b }", "FunctionDef"), """
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
                                            ResultVariableDef
                                                type: Type => r
                                                names: List[NameDef] = []
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
    def syntax(self, grammar: Grammar):
        grammar.add("""
        TestDef < Component := ">" lhs:Expression ("=>" rhs:Expression)?
        """)

    def test_parser(self):
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

