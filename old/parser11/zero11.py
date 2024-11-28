# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from typing import List
from util import *
from lexer import *
from grammar import *
from parser import *

#--------------------------------------------------------------------------------------------------
# Zero grammar: using classes
# idea is to have a class per AST node type, but with the most economical possible specification

#--------------------------------------------------------------------------------------------------
# Root entities and Program entities

# Named is an abstract base class; we know it's abstract because it has no rule() method
class Named(Entity):
    def __init__(self): super().__init__(); self.name: NameDef = None
    
# NameDef holds name and alias; parse as <name> or <name> | <alias>
# note keywords are 'keyword'; class property names are just name; optional is (xxx)?; :<type> does lex type matching
class NameDef(Entity):
    def __init__(self): self.name: str = ""; self.alias: str = None
    def rule(self): return "name:<identifier> ('|' alias:<identifier>)?"

# Variable isn't a grammar object, but a program object; created by VariableDef (further down)
class Variable(Named):
    def __init__(self): super().__init__(); self.type : Type = None; self.value: Expression = None

# Type is also not a grammar object, but a program object; created by TypeDef (further down)
class Type(Named):
    def __init__(self): super().__init__(); self.properties: List[Variable] = None; self.parents: List[Type] = None; self.children: List[Type] = None

#--------------------------------------------------------------------------------------------------
# Feature

# Feature is the feature clause; note 'parent&' means 'reference to parent, i.e. matches a string, but looks it up to find a Feature
# components; means "component list separated by semicolon" - we know it's a list because we look at the type annotations
class Feature(Named):
    def __init__(self): super().__init__(); self.parent: Feature = None; self.components: List[Component] = None
    def rule(self): return "'feature' name ('extends' parent&)? '{' components*; '}'"

# Component is an abstract class; it gets extended by Test, TypeDef, VariableDef, FunctionDef
# we know it's abstract because it has no rule() method :-)
class Component(Named):
    def __init__(self): super().__init__()

def test_parser_feature():
    test("feature_grammar", Grammar.current.dbg(["Feature", "NameDef"]), """
        NameDef := name:<identifier> NameDef_?
        NameDef_ := '|' alias:<identifier>
        Feature := 'feature' name:NameDef Feature_? '{' components:Component*; '}'
        Feature_ := 'extends' parent:Feature&
        """)

    test("feature_0", parse_code("", "Feature"), """
        Feature
            !!! premature end (expected 'feature', got 'None' at <eof>)
            name: NameDef = None
            parent: Feature = None
            components: List[Component] = None
        """)
    
    test("feature_1", parse_code("feature", "Feature"), """
        Feature
            name: NameDef
                !!! premature end (expected name:NameDef, got 'None' at <eof>)
            parent: Feature = None
            components: List[Component] = None
        """)

    test("feature_2", parse_code("feature MyFeature", "Feature"), """
        Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature = None
            components: List[Component] = None
        """)

    test("feature_3", parse_code("feature MyFeature extends", "Feature"), """
        Feature
            !!! mismatch (expected '{', got 'extends' at :...:19)
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature
                !!! premature end (expected parent:Feature&, got 'None' at <eof>)
            components: List[Component] = None
        """)
    
    test("feature_4", parse_code("feature MyFeature extends Another", "Feature"), """
        Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature => Another
            components: List[Component] = None        
        """)
    
    test("feature_5", parse_code("feature MyFeature {}", "Feature"), """
        Feature
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature = None
            components: List[Component] = []
        """)
    
    test("feature_6", parse_code("feature MyFeature extends Another {}", "Feature"), """
        Feature
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature => Another
            components: List[Component] = []    
        """)

#--------------------------------------------------------------------------------------------------
# Tests

# Test has an expression to be evalued (lhs) and potentially a result to compare it to (rhs)
class Test(Component):
    def __init__(self): self.lhs: Expression = None; self.rhs: Expression = None
    def rule(self): return "'>' lhs ('=>' rhs)?"

def test_parser_tests():
    test("test_grammar", Grammar.current.dbg(["Test"]), """
        Test := '>' lhs:Expression Test_?
        Test_ := '=>' rhs:Expression
        """)
    
    test("test_0", parse_code("> a", "Test"), """
        Test
            lhs: Expression
                VariableRef
                    variable: Variable => a
            rhs: Expression = None
    """)

    test("test_1", parse_code("> a =>", "Test"), """
        Test
            lhs: Expression
                VariableRef
                    variable: Variable => a
            rhs: Expression
                !!! premature end (expected rhs:Expression, got 'None' at <eof>)
    """)

    test("test_2", parse_code("> a => b", "Test"), """
        Test
            lhs: Expression
                VariableRef
                    variable: Variable => a
            rhs: Expression
                VariableRef
                    variable: Variable => b
    """)


#--------------------------------------------------------------------------------------------------
# TypeDefs

# TypeDef is abstract because there are three potential declarations, '=' struct, '>' children or '<' parents
class TypeDef(Component):    
    def __init__(self): super().__init__(); self.rhs: TypeRhs = None
    def rule(self): return "'type' name rhs"

# TypeRhs is abstract: one of TypeAlias, StructDef, TypeParentDef or TypeChildrenDef
class TypeRhs(Entity):
    def __init__(self): super().__init__()

# TypeAlias assigns the type to be the same as some other type
class TypeAlias(TypeRhs):
    def __init__(self): super().__init__(); self.alias: Type = None
    def rule(self): return "'=' alias&"

# StructDef just declares a list of properties
class StructDef(TypeRhs):
    def __init__(self): super().__init__(); self.properties: List[VariableDef] = None
    def rule(self): return "'=' '{' properties*; '}'"

# ParentDef declares that type is a child of some other type(s)
class TypeParentDef(TypeRhs):
    def __init__(self): self.parents: List[Type] = None
    def rule(self): return "'<' parents&+,"

# ChildrenDef declares that type is a parent of some other type(s)
class TypeChildrenDef(TypeRhs):
    def __init__(self): self.children: List[Type] = None
    def rule(self): return "'>' children&+,"

# TypeEnumDef defines one or more options
class TypeEnumDef(TypeRhs):
    def __init__(self): super().__init__(); self.options: List[str] = None
    def rule(self): return "'=' options+|"
    def post_parse_check(self) -> str: return "" if len(self.options) > 1 else "enum must have at least two options"


def test_parser_typedefs():
    test("typedef_grammar", Grammar.current.dbg(["TypeDef", "TypeRhs", "TypeAlias", "StructDef", "TypeParentDef", "TypeChildrenDef", "VariableDef"]), """
            TypeRhs := (StructDef | TypeParentDef | TypeChildrenDef | TypeEnumDef | TypeAlias)
            TypeDef := 'type' name:NameDef rhs:TypeRhs
            VariableDef := (VariableDef__ | VariableDef_) VariableDef___?
            VariableDef___ := '=' value:Expression
            VariableDef__ := names:NameDef+, ':' type:Type&
            VariableDef_ := type:Type& names:NameDef+,
            TypeAlias := '=' alias:Type&
            StructDef := '=' '{' properties:VariableDef*; '}'
            TypeParentDef := '<' parents:Type&+,
            TypeChildrenDef := '>' children:Type&+,
        """)
    
    test("type_0", parse_code("type vec | vector", "TypeDef"), """
        TypeDef
            name: NameDef
                NameDef
                    name: str = vec
                    alias: str = vector
            rhs: TypeRhs
                !!! premature end (expected rhs:TypeRhs, got 'None' at <eof>)        
        """)

    test("type_1", parse_code("type vec | vector = { x, y, z: number =0 }", "TypeDef"), """
        TypeDef
            name: NameDef
                NameDef
                    name: str = vec
                    alias: str = vector
            rhs: TypeRhs
                StructDef
                    properties: List[VariableDef]
                        VariableDef
                            name: NameDef = None
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
            name: NameDef
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
            name: NameDef
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
            name: NameDef
                NameDef
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
            name: NameDef
                NameDef
                    name: str = str
                    alias: str = string
            rhs: TypeRhs
                TypeAlias
                    alias: Type => char$
        """)
    
#--------------------------------------------------------------------------------------------------
# Expressions

# Expression is abstract: either a constant, variable, bracketed expression, or function call
class Expression(Entity):
    def __init__(self): super().__init__()

# Constant is a literal: either number of string
class Constant(Expression):
    def __init__(self): super().__init__(); self.value: str = None
    def rule(self): return "value:(<number> | <string>)"

# VariableRef just refers to a variable; we use '&' to indicate that it's a reference
class VariableRef(Expression):
    def __init__(self): super().__init__(); self.variable: Variable = None
    def rule(self): return "variable&"

class Bracketed(Expression):
    def __init__(self): super().__init__(); self.expression: Expression = None
    def rule(self): return "'(' expression ')'"

class FunctionCall(Expression):
    def __init__(self): super().__init__(); self.items: List[FunctionCallItem] = None
    def rule(self): return "items+"
    def post_parse_check(self) -> str:
        n_bracketed = 0; n_operators = 0
        for item in self.items:
            if isinstance(item, FunctionCallArguments): n_bracketed += 1
            if isinstance(item, FunctionCallOperator): n_operators += 1
        if n_bracketed ==0 and n_operators ==0: return "function call must have at least one argument or operation"
        return ""
    
class FunctionCallItem(Entity):
    def __init__(self): super().__init__()

class FunctionCallConstant(FunctionCallItem):
    def __init__(self): super().__init__(); self.constant : Constant = None
    def rule(self): return "constant"

class FunctionCallOperator(FunctionCallItem):
    def __init__(self): super().__init__(); self.operator: str = None
    def rule(self): return "operator:<operator>"

class FunctionCallWord(FunctionCallItem):
    def __init__(self): super().__init__(); self.word: str = None
    def rule(self): return "word:<identifier>"

class FunctionCallArguments(FunctionCallItem):
    def __init__(self): super().__init__(); self.arguments: List[FunctionCallArgument] = None
    def rule(self): return "'(' arguments+, ')'"

class FunctionCallArgument(Entity):
    def __init__(self): super().__init__(); self.argument: Variable = None; self.value: Expression = None
    def rule(self): return "(argument& '=')? value"

def test_parser_expressions():
    test("expression_grammar", Grammar.current.dbg(["Expression", "Constant", "VariableRef", "Bracketed", "FunctionCall", "FunctionCallItem", 
                                                    "FunctionCallConstant", "FunctionCallOperator", "FunctionCallWord", "FunctionCallArguments", "FunctionCallArgument"]), """
            Expression := (FunctionCall | Bracketed | Constant | VariableRef)
            FunctionCallItem := (FunctionCallArguments | FunctionCallConstant | FunctionCallOperator | FunctionCallWord)
            FunctionCallArgument := FunctionCallArgument_? value:Expression
            FunctionCallArgument_ := argument:Variable& '='
            Constant := value:(<number> | <string>)
            VariableRef := variable:Variable&
            Bracketed := '(' expression:Expression ')'
            FunctionCall := items:FunctionCallItem+
            FunctionCallConstant := constant:Constant
            FunctionCallOperator := operator:<operator>
            FunctionCallWord := word:<identifier>
            FunctionCallArguments := '(' arguments:FunctionCallArgument+, ')'
        """)

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
# Variables

# VariableDef declares one or more variables with the same type and default value, using either c or ts notation
# the '+' indicates one or more, "," at the end is the separator
class VariableDef(Component):
    def __init__(self): super().__init__(); self.type : Type = None; self.names: List[NameDef] = None; self.value: Expression = None
    def rule(self): return "((type& names+,) | (names+, ':' type&)) ('=' value)?"

def test_parser_variables():
    test("variable_grammar", Grammar.current.dbg(["VariableDef", "NameDef", "Type"]), """
        NameDef := name:<identifier> NameDef_?
        NameDef_ := '|' alias:<identifier>
        Type := 
        VariableDef := (VariableDef__ | VariableDef_) VariableDef___?
        VariableDef___ := '=' value:Expression
        VariableDef__ := names:NameDef+, ':' type:Type&
        VariableDef_ := type:Type& names:NameDef+,
         """)
    
    test("variable_0", parse_code("int a", "VariableDef"), """
        VariableDef
            name: NameDef = None
            type: Type => int
            names: List[NameDef]
                NameDef
                    name: str = a
                    alias: str = None
            value: Expression = None
        """)
    
    test("variable_1", parse_code("a : int", "VariableDef"), """
            VariableDef
                name: NameDef = None
                type: Type => int
                names: List[NameDef]
                    NameDef
                        name: str = a
                        alias: str = None
                value: Expression = None
        """)

    test("variable_2", parse_code("a : int = 0", "VariableDef"), """
        VariableDef
            name: NameDef = None
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
            name: NameDef = None
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
            name: NameDef = None
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
            name: NameDef = None
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
# FunctionDef

class FunctionDef(Component):
    def __init__(self): 
        super().__init__()
        self.modifier : FunctionModifier = None
        self.results: FunctionResults = None
        self.assignOp: str = None
        self.signature: FunctionSignature = None
        self.body: FunctionBody = None
    def rule(self):
        return "modifier results? assignOp:('=' | '<<') signature '{' body '}'"
    
class FunctionModifier(Entity):
    def __init__(self): super().__init__(); self.modifier : str = None
    def rule(self): return "modifier:('on' | 'before' | 'after' | 'replace')"

class FunctionResults(Entity):
    def __init__(self): super().__init__(); self.results : List[FunctionResultVariableDef] = None
    def rule(self): return "'(' results+, ')'"

class FunctionResultVariableDef(Entity):
    def __init__(self): super().__init__(); self.names: List[NameDef] = None; self.type: Type = None
    def rule(self): return "((type& names+,) | (names+, ':' type&))"

class FunctionSignature(Entity):
    def __init__(self): super().__init__(); self.elements: List[FunctionSignatureElement] = None
    def rule(self): return "elements+"

class FunctionSignatureElement(Entity):
    def __init__(self): super().__init__()

class FunctionSignatureWord(FunctionSignatureElement):
    def __init__(self): super().__init__(); self.word : str = None
    def rule(self): return "word:(<identifier> | <operator>)"

class FunctionSignatureParams(FunctionSignatureElement):
    def __init__(self): super().__init__(); self.params : List[VariableDef] = None
    def rule(self): return "'(' params+, ')'"

class FunctionBody(Entity):
    def __init__(self): super().__init__(); self.statements: List[Statement] = None
    def rule(self): return "statements*;"

class Statement(Entity):
    def __init__(self): super().__init__(); self.lhs : StatementLhs = None; self.assignOp: ResultVariableAssign = None; self.rhs: Expression = None
    def rule(self): return "lhs assignOp rhs"

class StatementLhs(Entity):
    def __init__(self): super().__init__(); self.variables: List[ResultVariable] = None
    def rule(self): return "variables+,"

class ResultVariable(Entity):
    def __init__(self): super().__init__()

class ResultVariableDef(ResultVariable):
    def __init__(self): super().__init__(); self.name: NameDef = None; self.type: Type = None
    def rule(self): return "((type& name) | (name ':' type&))"

class ResultVariableRef(ResultVariable):
    def __init__(self): super().__init__(); self.variable: Variable = None
    def rule(self): return "variable&"

class ResultVariableAssign(Entity):
    def __init__(self): super().__init__(); self.assignOp: str = None
    def rule(self): return "assignOp:('=' | '<<')"

def test_parser_functiondef():
    test("function_grammar", Grammar.current.dbg(["FunctionDef", "FunctionModifier", "FunctionResults", "FunctionResultVariableDef", 
                                                  "FunctionSignature", "FunctionSignatureElement", "FunctionSignatureWord", "FunctionSignatureParams", 
                                                  "FunctionBody", "Statement", "StatementLhs", "ResultVariable", 
                                                  "ResultVariableDef", "ResultVariableRef", "ResultVariableAssign"]), """
        FunctionModifier := modifier:('on' | 'before' | 'after' | 'replace')
        FunctionResults := '(' results:FunctionResultVariableDef+, ')'
        FunctionResultVariableDef := (FunctionResultVariableDef__ | FunctionResultVariableDef_)
        FunctionResultVariableDef__ := names:NameDef+, ':' type:Type&
        FunctionResultVariableDef_ := type:Type& names:NameDef+,
        FunctionSignature := elements:FunctionSignatureElement+
        FunctionSignatureElement := (FunctionSignatureParams | FunctionSignatureWord)
        FunctionBody := statements:Statement*;
        Statement := lhs:StatementLhs assignOp:ResultVariableAssign rhs:Expression
        StatementLhs := variables:ResultVariable+,
        ResultVariable := (ResultVariableDef | ResultVariableRef)
        ResultVariableAssign := assignOp:('=' | '<<')
        FunctionDef := modifier:FunctionModifier results:FunctionResults? assignOp:('=' | '<<') signature:FunctionSignature '{' body:FunctionBody '}'
        FunctionSignatureWord := word:(<identifier> | <operator>)
        FunctionSignatureParams := '(' params:VariableDef+, ')'
        ResultVariableDef := (ResultVariableDef__ | ResultVariableDef_)
        ResultVariableDef__ := name:NameDef ':' type:Type&
        ResultVariableDef_ := type:Type& name:NameDef
        ResultVariableRef := variable:Variable&
        """)
    
    test("result_vars_1", parse_code("(a, b: int, k, l: float)", "FunctionResults"), """
        FunctionResults
            results: List[FunctionResultVariableDef]
                FunctionResultVariableDef
                    names: List[NameDef]
                        NameDef
                            name: str = a
                            alias: str = None
                        NameDef
                            name: str = b
                            alias: str = None
                    type: Type => int
                FunctionResultVariableDef
                    names: List[NameDef]
                        NameDef
                            name: str = k
                            alias: str = None
                        NameDef
                            name: str = l
                            alias: str = None
                    type: Type => float
        """)
    
    test("result_vars_2", parse_code("(int a, b, float k)", "FunctionResults"), """
        FunctionResults
            results: List[FunctionResultVariableDef]
                FunctionResultVariableDef
                    names: List[NameDef]
                        NameDef
                            name: str = a
                            alias: str = None
                        NameDef
                            name: str = b
                            alias: str = None
                    type: Type => int
                FunctionResultVariableDef
                    names: List[NameDef]
                        NameDef
                            name: str = k
                            alias: str = None
                    type: Type => float
        """)
    test("param_group_0", parse_code("(int a, b=0, float k=0)", "FunctionSignatureParams"), """
        FunctionSignatureParams
            params: List[VariableDef]
                VariableDef
                    name: NameDef = None
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
                    name: NameDef = None
                    type: Type => float
                    names: List[NameDef]
                        NameDef
                            name: str = k
                            alias: str = None
                    value: Expression
                        Constant
                            value: str = 0
        """)
    
    test("param_group_0", parse_code("(int a, b=0, float k=0)", "FunctionSignatureParams"), """
        FunctionSignatureParams
            params: List[VariableDef]
                VariableDef
                    name: NameDef = None
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
                    name: NameDef = None
                    type: Type => float
                    names: List[NameDef]
                        NameDef
                            name: str = k
                            alias: str = None
                    value: Expression
                        Constant
                            value: str = 0
        """)
    
    test("param_group_0", parse_code("(int a, b, float k)", "FunctionSignatureParams"), """
        FunctionSignatureParams
            params: List[VariableDef]
                VariableDef
                    name: NameDef = None
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
                    name: NameDef = None
                    type: Type => float
                    names: List[NameDef]
                        NameDef
                            name: str = k
                            alias: str = None
                    value: Expression = None
        """)
    
    test("param_group_1", parse_code("(a, b: int, k: float)", "FunctionSignatureParams"), """
        FunctionSignatureParams
            params: List[VariableDef]
                VariableDef
                    name: NameDef = None
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
                    name: NameDef = None
                    type: Type => float
                    names: List[NameDef]
                        NameDef
                            name: str = k
                            alias: str = None
                    value: Expression = None
                """)
    
    test("function_0", parse_code("on (int r) = min (int a, b) { r = if (a < b) then a else b }", "FunctionDef"), """
        FunctionDef
            name: NameDef = None
            modifier: FunctionModifier
                FunctionModifier
                    modifier: str = on
            results: FunctionResults
                FunctionResults
                    results: List[FunctionResultVariableDef]
                        FunctionResultVariableDef
                            names: List[NameDef]
                                NameDef
                                    name: str = r
                                    alias: str = None
                            type: Type => int
            assignOp: str = =
            signature: FunctionSignature
                FunctionSignature
                    elements: List[FunctionSignatureElement]
                        FunctionSignatureWord
                            word: str = min
                        FunctionSignatureParams
                            params: List[VariableDef]
                                VariableDef
                                    name: NameDef = None
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
                            assignOp: ResultVariableAssign
                                ResultVariableAssign
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
    
    test("feature_7", parse_code("feature Hello { type str | string = char$; string out$ | output$; }", "Feature"), """
        Feature
            name: NameDef
                NameDef
                    name: str = Hello
                    alias: str = None
            parent: Feature = None
            components: List[Component]
                TypeDef
                    name: NameDef
                        NameDef
                            name: str = str
                            alias: str = string
                    rhs: TypeRhs
                        TypeAlias
                            alias: Type => char$
                VariableDef
                    name: NameDef = None
                    type: Type => string
                    names: List[NameDef]
                        NameDef
                            name: str = out$
                            alias: str = output$
                    value: Expression = None
        """)
    
#--------------------------------------------------------------------------------------------------

@this_is_the_test
def test_zero_grammar():
    log("test_parser")
    if Grammar.current == None: grammar = Grammar(Entity)
    test_verbose(False)
    test_parser_feature()
    test_parser_expressions()
    test_parser_tests()
    test_parser_variables()
    test_parser_typedefs()
    test_parser_functiondef()
    log_clear()
    test_verbose(True)
    log("------------------------")
    # just these ones left and we're good to go!

    