# ᕦ(ツ)ᕤ
# grammar.py
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

#--------------------------------------------------------------------------------------------------
# Tests
# we're going to try and be modular here and express all compilation steps for tests in one place

# Test has an expression to be evalued (lhs) and potentially a result to compare it to (rhs)
class Test(Component):
    def __init__(self): self.lhs: Expression = None; self.rhs: Expression = None
    def rule(self): return "'>' lhs ('=>' rhs)?"

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
    def rule(self): return "'<' parents+,"

# ChildrenDef declares that type is a parent of some other type(s)
class TypeChildrenDef(TypeRhs):
    def __init__(self): self.children: List[Type] = None
    def rule(self): return "'>' children+,"

#--------------------------------------------------------------------------------------------------
# VariableDefs

# VariableDef declares one or more variables with the same type and default value, using either c or ts notation
# the '+' indicates one or more, "," at the end is the separator
class VariableDef(Component):
    def __init__(self): super().__init__(); self.type : Type = None; self.names: List[NameDef] = None; self.value: Expression = None
    def rule(self): return "((type& names+,) | (names+, ':' type&)) ('=' value)?"

#--------------------------------------------------------------------------------------------------
# Expressions

# Expression is abstract: either a constant, variable, bracketed expression, or function call
class Expression(Entity):
    def __init__(self): super().__init__()

# Constant is a literal: either number of string
class Constant(Expression):
    def __init__(self): super().__init__(); self.value: str = None
    def rule(self): return "(<number> | <string>)"

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

class FunctionCallItem(Entity):
    def __init__(self): super().__init__()

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
    def __init__(self): super().__init__(); self.argument: str = None; self.value: Expression = None
    def rule(self): return "(argument '=')? value"

#--------------------------------------------------------------------------------------------------
# FunctionDef

class FunctionDef(Component):
    def __init__(self): 
        super().__init__()
        self.modifier : FunctionModifier = None
        self.results: List[ResultVariableDef] = None
        self.assignOp: str = None
        self.signature: FunctionSignature = None
        self.body: FunctionBody = None
    def rule(self):
        return "modifier '(' results+, ')' assignOp:('=' | '<<') signature '{' body '}'"
    
class FunctionModifier(Entity):
    def __init__(self): super().__init__(); self.modifier : str = None
    def rule(self): return "modifier:('on' | 'before' | 'after' | 'replace')"

class ResultVariableDef(Entity):
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
    def rule(self): return "params+,"

class FunctionBody(Entity):
    def __init__(self): super().__init__(); self.statements: List[Statement] = None
    def rule(self): return "statements*;"

class Statement(Entity):
    def __init__(self): super().__init__(); self.lhs : StatementLhs = None; self.assignOp: str = None; self.rhs: Expression = None
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

class ResultVariableAssign(ResultVariable):
    def __init__(self): super().__init__(); self.assignOp: str = None
    def rule(self): return "assignOp:('=' | '<<')"

#--------------------------------------------------------------------------------------------------
# test grammar

@this_is_a_test
def test_grammar():
    log("test_grammar")
    grammar = Grammar(Entity)
    test("grammar", grammar.dbg(), """
Named := (Variable | Type | Feature | Component)
NameDef := name:<identifier> NameDef_?
NameDef_ := '|' alias:<identifier>
TypeRhs := (TypeAlias | StructDef | TypeParentDef | TypeChildrenDef)
Expression := (Constant | VariableRef | Bracketed | FunctionCall)
FunctionCallItem := (FunctionCallOperator | FunctionCallWord | FunctionCallArguments)
FunctionCallArgument := FunctionCallArgument_? value:Expression
FunctionCallArgument_ := argument:<identifier> '='
FunctionModifier := modifier:('on' | 'before' | 'after' | 'replace')
FunctionSignature := elements:FunctionSignatureElement+
FunctionSignatureElement := (FunctionSignatureWord | FunctionSignatureParams)
FunctionBody := statements:Statement*;
Statement := lhs:StatementLhs assignOp:<identifier> rhs:Expression
StatementLhs := variables:ResultVariable+,
ResultVariable := (ResultVariableDef | ResultVariableRef | ResultVariableAssign)
Variable := 
Type := 
Feature := 'feature' name:NameDef Feature_? '{' components:Component*; '}'
Feature_ := 'extends' parent:Feature&
Component := (Test | TypeDef | VariableDef | FunctionDef)
Test := '>' lhs:Expression Test_?
Test_ := '=>' rhs:Expression
TypeDef := 'type' name:NameDef rhs:TypeRhs
VariableDef := (VariableDef_ | VariableDef__) VariableDef___?
VariableDef___ := '=' value:Expression
VariableDef__ := names:NameDef+, ':' type:Type&
VariableDef_ := type:Type& names:NameDef+,
FunctionDef := modifier:FunctionModifier '(' results:ResultVariableDef+, ')' assignOp:('=' | '<<') signature:FunctionSignature '{' body:FunctionBody '}'
TypeAlias := '=' alias:Type&
StructDef := '=' '{' properties:VariableDef*; '}'
TypeParentDef := '<' parents:Type+,
TypeChildrenDef := '>' children:Type+,
Constant := (<number> | <string>)
VariableRef := variable:Variable&
Bracketed := '(' expression:Expression ')'
FunctionCall := items:FunctionCallItem+
FunctionCallOperator := operator:<operator>
FunctionCallWord := word:<identifier>
FunctionCallArguments := '(' arguments:FunctionCallArgument+, ')'
FunctionSignatureWord := word:(<identifier> | <operator>)
FunctionSignatureParams := params:VariableDef+,
ResultVariableDef := (ResultVariableDef_ | ResultVariableDef__)
ResultVariableDef__ := name:NameDef ':' type:Type&
ResultVariableDef_ := type:Type& name:NameDef
ResultVariableRef := variable:Variable&
ResultVariableAssign := assignOp:('=' | '<<')
         """)

#--------------------------------------------------------------------------------------------------
# parser testing

def test_parser_feature():
    test("feature_0", parse_code("", "Feature"), """
        Feature
            !!! premature end (expected 'feature', got 'None' at <eof>)
            name: NameDef = None
            parent: Feature = None
            components: List[Component] = None
        """)
    
    test("feature_1", parse_code("feature", "Feature"), """
        Feature
            name: NameDef =
                !!! premature end (expected name:NameDef, got 'None' at <eof>)
            parent: Feature = None
            components: List[Component] = None
        """)

    test("feature_2", parse_code("feature MyFeature", "Feature"), """
         Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef =
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature = None
            components: List[Component] = None
        """)

    test("feature_3", parse_code("feature MyFeature extends", "Feature"), """
        Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef =
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature =
                !!! premature end (expected parent:Feature&, got 'None' at <eof>)
            components: List[Component] = None
        """)
    
    test("feature_4", parse_code("feature MyFeature extends Another", "Feature"), """
        Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef =
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature => Another
            components: List[Component] = None        
        """)
    
    test("feature_5", parse_code("feature MyFeature {}", "Feature"), """
        Feature
            name: NameDef =
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature = None
            components: List[Component] = []
        """)
    
    test("feature_6", parse_code("feature MyFeature extends Another {}", "Feature"), """
        Feature
            name: NameDef =
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature => Another
            components: List[Component] = []    
        """)
#--------------------------------------------------------------------------------------------------

@this_is_the_test
def test_parser():
    log("test_parser")
    if Grammar.current == None: raise Exception("no current grammar!")
    test_parser_feature()
    log("------------------------")
    

    

