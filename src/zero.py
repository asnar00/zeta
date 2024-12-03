# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from typing import List
from util import *
from lexer import *
from grammar import *
from parser import *
from symbols import *

#--------------------------------------------------------------------------------------------------
# main

s_test_program = """
feature Program
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
context MyContext = Program, Hello, Goodbye
"""

s_test_math = """
feature Math
    type int > i8, i16, i32, i64
    type float > f32, f64
    type number = int | float
    on (number r) = (number a) + (number b)
        emit("add r, a, b")
    on (number r) = (number a) * (number b)
        emit("mul r, a, b")

feature VectorMath extends Math
    type vector | vec = 
        number x, y, z = 0
    on (vec r) = (vec a) + (vec b)
        r = vec(a.x + b.x, a.y + b.y, a.z + b.z)
    on (number p) = (vec a) dot (vec b)
        p = a.x * b.x + a.y * b.y + a.z * b.z

context MathContext = Math, VectorMath
"""

@this_is_the_test
def test_zero():
    log("test_zero")
    zero = Language()
    zero.add_modules([module_Features(), module_Expressions(), module_Variables(), module_Types(), module_Functions(), module_Tests()])
    test_verbose(False)
    log_max_depth(8)
    zero.setup()
    code = s_test_program
    ast, st = zero.compile(code)
    log_clear()
    #test_print_code(ast)
    log(print_code_formatted(ast))
    #log(dbg_entity(ast))
    log("--------------------------------")
    log(st.dbg())

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
# modular language definition; so we can add a bit at a time

# LanguageModule collects all compilation stages (grammar, parser, resolver, etc) for some part of the language
class LanguageModule:
    def __init__(self): pass
    def define(self, grammar: Grammar): pass      # add grammar rules and validation functions
    def test(self): pass                           # test parser with some examples

# Language collects all modules into one unit
class Language:
    def __init__(self): 
        self.modules = []
        self.grammar = Grammar()
    def add_modules(self, modules: List[LanguageModule]): self.modules.extend(modules)
    def setup(self):
        for module in self.modules: 
            module.define(self.grammar)
            module.test()
    def compile(self, code: str) -> 'Program':
        ls = lexer(Source(code = code))
        reader = Reader(ls)
        rule = Grammar.current.rule_named["Program"]
        ast = parse_rule(rule, reader)
        st = SymbolTable()
        log_clear()
        st.add_symbols(ast, None)
        st.resolve_symbols(ast)
        log_exit("done resolving")

        return ast, st

#--------------------------------------------------------------------------------------------------
# features and contexts 

class module_Features(LanguageModule):
    def define(self, grammar: Grammar):
        grammar.add("""
            NameDef := name:<identifier> ("|" alias:<identifier>)?
            Feature := NameDef parent:Feature& types:Type&* variables:Variable&* functions:Function&*
            FeatureDef := "feature" NameDef ("extends" parent:Feature&)? "{" components:Component*; "}"
            Component
            ContextDef := "context" NameDef "=" feature:Feature&*,
            Program := components:(FeatureDef | ContextDef)+;
            """)
        grammar.method("NameDef", """
            def add_symbols(self, scope, symbol_table): pass
            """)
        
        # def add(self, name: str, element: Any, scope: Any, tag: Dict=None) -> str:
        grammar.method("FeatureDef", """
            def add_symbols(self, scope, symbol_table):
                feature = Feature(name=self.name, alias=self.alias, parent=self.parent)
                symbol_table.add(self.name, feature, scope, alias=self.alias)  
            """)

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
    def define(self, grammar: Grammar):
        grammar.add("""
            Expression :=
            Constant < Expression := value:(<number> | <string>)
            VariableRef < Expression := variable:Variable&
            Bracketed < Expression := "(" expression:Expression ")"
            FunctionCall < Expression := items:FunctionCallItem+
            FunctionCallItem :=
            FunctionCallConstant < FunctionCallItem := constant:Constant
            FunctionCallOperator < FunctionCallItem := operator:<operator>
            FunctionCallWord < FunctionCallItem := word:<identifier>
            FunctionCallArguments < FunctionCallItem := "(" arguments:FunctionCallArgument+, ")"
            FunctionCallArgument := (argument:Variable& "=")? value:Expression
                    """)
        
        # function call must have at least one bracketed term or operator
        grammar.method("FunctionCall", """
            def validate(self) -> str:
                n_bracketed = 0; n_operators = 0
                for item in self.items:
                    if isinstance(item, FunctionCallArguments): n_bracketed += 1
                    if isinstance(item, FunctionCallOperator): n_operators += 1
                if n_bracketed ==0 and n_operators ==0: return "function call must have at least one argument or operation"
                return ""
        """)
        grammar.method("FunctionCall", """
            def resolve(self, symbol_table) -> str:
                # first replace any variable names with variables
                for i, item in enumerate(self.items):
                    if isinstance(item, FunctionCallWord):
                        vars = symbol_table.find(item.word)
                        if len(vars)==1 and isinstance(vars[0].element, Variable):
                            log(log_red(f"replacing {item.word} with {vars[0]}"))
                            self.items[i] = FunctionCallVariable(variable=vars[0])
                # now find a single function for all word/operators
                i_word = -1
                found_function = None
                for i, item in enumerate(self.items):
                    if isinstance(item, FunctionCallWord):
                        found = symbol_table.find(item.word)
                        if len(found) == 0: return f"can't find {item.word}"
                        elif len(found) > 1: return f"symbol clash: {item.word} => {found}"
                        else:
                            func = found[0].element
                            if not isinstance(func, Function): return f"{item.word} is not a function"
                            tag = found[0].tag["i_word"]
                            log(f"found function {func.handle}, tag {tag}")
                            if found_function == None:
                                found_function = func
                                if tag != 0: return "starting with nonzero tag"
                                i_word = 0
                            else:
                                tag = found[0].tag["i_word"]
                                if found[0] != found_function or tag != (i_word+1):
                                    return "mismatched word"
                                i_word = tag
                log(log_green(f"found function {found_function.handle}"))
                return ""
        """)

    def test(self):
        test("expression_0", parse_code("1", "Expression"), """
            Constant
                value: str = 1
            """)

        test("expression_1", parse_code("a", "Expression"), """
            VariableRef
                variable: Variable => a
            """)

        test("expression_2", parse_code("a + b", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        word: str = a
                    FunctionCallOperator
                        operator: str = +
                    FunctionCallWord
                        word: str = b
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
                                operator: str = +
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
                                operator: str = +
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
                                                operator: str = +
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
                        operator: str = -
                    FunctionCallWord
                        word: str = c
        """)

        test("expression_4", parse_code("a + (2 - c)", "Expression"), """
            FunctionCall
                items: List[FunctionCallItem]
                    FunctionCallWord
                        word: str = a
                    FunctionCallOperator
                        operator: str = +
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
                                                operator: str = -
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
    def define(self, grammar: Grammar):
        grammar.add("""
            Variable := type:Type& NameDef "=" value:Expression
            VariableDef < Component := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&)) ("=" value:Expression)?
        """)
        grammar.method("VariableDef", """
            def add_symbols(self, scope, symbol_table):
                for name in self.names:
                    var = Variable(type=self.type, name=name.name, alias=name.alias, value=self.value)
                    symbol_table.add(name.name, var, scope)
                    if name.alias: symbol_table.add(name.alias, var, scope)
        """)

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
    def define(self, grammar: Grammar):
        grammar.add("""
            Type := NameDef properties:Variable&* parents:Type&* children:Type&*
            TypeDef < Component := "type" NameDef rhs:TypeRhs
            TypeRhs :=
            TypeAlias < TypeRhs := "=" type:Type&
            StructDef < TypeRhs := "=" "{" properties:VariableDef+; "}"
            TypeParentDef < TypeRhs := "<" parents:Type&+,
            TypeChildrenDef < TypeRhs := ">" children:Type&+,
            TypeEnumDef < TypeRhs := "=" options:<identifier>+|
        """)
        grammar.method("TypeEnumDef", """
            def validate(self) -> str: 
                return "" if len(self.options) > 1 else "enum must have at least two options"
        """)
        grammar.method("TypeDef", """
            def add_symbols(self, scope, symbol_table):
                type = Type(name=self.name, alias=self.alias)
                symbol_table.add(self.name, type, scope, alias=self.alias)
        """)
    
    def test(self):
        test("type_0", parse_code("type vec | vector", "TypeDef"), """
        TypeDef
            name: str = vec
            alias: str = vector
            rhs: TypeRhs
                !!! premature end (expected rhs:TypeRhs, got 'None' at <eof>) 
            """)
    
        test("type_1", parse_code("type vec | vector = { x, y, z: number =0 }", "TypeDef"), """
            TypeDef
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
                name: str = offset
                alias: str = None
                rhs: TypeRhs
                    TypeParentDef
                        parents: List[Type]
                            => vector
            """)
        
        test("type_4", parse_code("type evil = no | yes | maybe", "TypeDef"), """
            TypeDef
                name: str = evil
                alias: str = None
                rhs: TypeRhs
                    TypeEnumDef
                        options: List[str]
                            no
                            yes
                            maybe
            """)
        
        test("type_5", parse_code("type str | string = char$", "TypeDef"), """
            TypeDef
                name: str = str
                alias: str = string
                rhs: TypeRhs
                    TypeAlias
                        type: Type => char$
            """)

#--------------------------------------------------------------------------------------------------
# functions

class module_Functions(LanguageModule):
    def define(self, grammar: Grammar):
        grammar.add("""
            Function := handle:<identifier> results:FunctionResults signature:FunctionSignature body:FunctionBody
            FunctionDef < Component := FunctionModifier results:FunctionResults? signature:FunctionSignature body:FunctionBody
            FunctionModifier := modifier:("on" | "before" | "after" | "replace")
            FunctionResults := "(" results:FunctionResultVariableDef+, ")" assignOp:("=" | "<<")
            FunctionResultVariableDef := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&))
            FunctionSignature := elements:FunctionSignatureElement+
            FunctionSignatureElement :=
            FunctionSignatureWord < FunctionSignatureElement := word:(<identifier> | <operator>)
            FunctionSignatureParams < FunctionSignatureElement := "(" params:VariableDef+, ")"
            FunctionBody := "{" statements:Statement*; "}"
            Statement := (lhs:StatementLhs)? rhs:Expression
            StatementLhs := variables:ResultVariable+, assignOp:("=" | "<<") 
            ResultVariable :=
            ResultVariableRef < ResultVariable := variable:Variable&
            ResultVariableDef < ResultVariable := ((type:Type& NameDef) | (NameDef ":" type:Type&))
        """)
        grammar.method("FunctionResultVariableDef", """
            def add_symbols(self, scope, symbol_table):
                for name in self.names:
                    var = Variable(type=self.type, name=name.name, alias=name.alias)
                    symbol_table.add(name.name, var, scope, alias=name.alias)
        """)
        grammar.method("ResultVariableDef", """
            def add_symbols(self, scope, symbol_table):
                var = Variable(type = self.type, name = self.name, alias=self.alias)
                symbol_table.add(var.name, var, scope, alias=var.alias)
        """)
        grammar.method("FunctionSignature", """
            def handle(self)-> str:
                name = ""
                for item in self.elements:
                    if isinstance(item, FunctionSignatureWord): name += str(item.word) + "_"
                    elif isinstance(item, FunctionSignatureParams):
                       name += "()_"
                return name[:-1]
        """)
        grammar.method("FunctionDef", """
            def add_symbols(self, scope, symbol_table):
                handle = self.signature.handle()
                log("add_symbols: handle", handle)
                functions = symbol_table.find(handle, None)
                if len(functions) == 0:
                    log("creating function:", handle)
                    function = Function(handle=handle, results=self.results, signature=self.signature, body=self.body)
                    symbol_table.add(handle, function, None)
                    i_word = 0
                    for element in self.signature.elements:
                        if isinstance(element, FunctionSignatureWord):
                            symbol_table.add(element.word, function, None, tag={"i_word": i_word})
                            i_word += 1
                else: 
                    function = functions[0]
                    log("function exists already; extending")
                
        """)
    
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
                assignOp: str = =
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
                assignOp: str = <<
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
                        assignOp: str = =
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
                                                variable: Variable => r
                                        assignOp: str = =
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
                                                                        operator: str = <
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
    def define(self, grammar: Grammar):
        grammar.add("""
        TestDef < Component := ">" lhs:Expression ("=>" rhs:Expression)?
        """)

    def test(self):
        test("test_0", parse_code("> a", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variable: Variable => a
                rhs: Expression = None
        """)

        test("test_1", parse_code("> a =>", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variable: Variable => a
                rhs: Expression
                    !!! premature end (expected rhs:Expression, got 'None' at <eof>)
        """)

        test("test_2", parse_code("> a => b", "TestDef"), """
            TestDef
                lhs: Expression
                    VariableRef
                        variable: Variable => a
                rhs: Expression
                    VariableRef
                        variable: Variable => b
        """)

