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
# modular Zero grammar using auto-generated classes

#--------------------------------------------------------------------------------------------------
# feature clause

def define_feature(grammar: Grammar):
    grammar.add("""
        NameDef := name:<identifier> ("|" alias:<identifier>)?
        Feature := "feature" name:NameDef ("extends" parent:Feature&)? "{" components:Component*; "}"
        Component
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
# Expressions

def define_expressions(grammar):
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
    grammar.add_method("FunctionCall", """
        def post_parse_check(self) -> str:
            n_bracketed = 0; n_operators = 0
            for item in self.items:
                if isinstance(item, FunctionCallArguments): n_bracketed += 1
                if isinstance(item, FunctionCallOperator): n_operators += 1
            if n_bracketed ==0 and n_operators ==0: return "function call must have at least one argument or operation"
            return ""
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
# Tests

def define_test():
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
# variables

class Variable: pass
class Type: pass

def define_variables(grammar):
    grammar.add("""
        VariableDef := ((type:Type& names:NameDef+,) | (names:NameDef+, ":" type:Type&)) ("=" value:Expression)?
        """)
    
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
# main

@this_is_the_test
def test_zero_grammar():
    log("test_zero_grammar")
    grammar = Grammar()
    define_feature(grammar)
    define_expressions(grammar)
    define_variables(grammar)