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
    def __init__(self): super().__init__(); self.properties: List[Variable] = []; self.parents: List[Type] = []; self.children: List[Type] = []

#--------------------------------------------------------------------------------------------------
# Feature

# Feature is the feature clause; note 'parent&' means 'reference to parent, i.e. matches a string, but looks it up to find a Feature
# components; means "component list separated by semicolon" - we know it's a list because we look at the type annotations
class Feature(Named):
    def __init__(self): super().__init__(); self.parent: Feature = None; self.components: List[Component] = []
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
    def __init__(self): super().__init__(); self.properties: List[VariableDef] = []
    def rule(self): return "'=' '{' properties*; '}'"

# ParentDef declares that type is a child of some other type(s)
class TypeParentDef(TypeRhs):
    def __init__(self): self.parents: List[Type] = []
    def rule(self): return "'<' parents+,"

# ChildrenDef declares that type is a parent of some other type(s)
class TypeChildrenDef(TypeRhs):
    def __init__(self): self.children: List[Type] = []
    def rule(self): return "'>' children+,"

#--------------------------------------------------------------------------------------------------
# VariableDefs

# VariableDef declares one or more variables with the same type and default value, using either c or ts notation
# the '+' indicates one or more, "," at the end is the separator
class VariableDef(Component):
    def __init__(self): super().__init__(); self.type : Type = None; self.names: List[NameDef] = []; self.value: Expression = None
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
    def __init__(self): super().__init__(); self.items: List[FunctionCallItem] = []
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
    def __init__(self): super().__init__(); self.arguments: List[FunctionCallArgument] = []
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
        self.results: List[ResultVariableDef] = []
        self.assignOp: str = None
        self.signature: FunctionSignature = None
        self.body: FunctionBody = None

    def rule(self):
        return "modifier '(' results+, ')' assignOp:('=' | '<<') signature '{' body '}'"
    
class FunctionModifier(Entity):
    def __init__(self): super().__init__(); self.modifier : str = None
    def rule(self): return "modifier:('on' | 'before' | 'after' | 'replace')"

class ResultVariableDef(Entity):
    def __init__(self): super().__init__(); self.names: List[NameDef] = []; self.type: Type = None
    def rule(self): return "((type& names+,) | (names+, ':' type&))"

class FunctionSignature(Entity):
    def __init__(self): super().__init__(); self.elements: List[FunctionSignatureElement] = []
    def rule(self): return "elements+"

class FunctionSignatureElement(Entity):
    def __init__(self): super().__init__()

class FunctionSignatureWord(FunctionSignatureElement):
    def __init__(self): super().__init__(); self.word : str = None
    def rule(self): return "word:(<identifier> | <operator>)"

class FunctionSignatureParams(FunctionSignatureElement):
    def __init__(self): super().__init__(); self.params : List[VariableDef] = []
    def rule(self): return "params+,"

class FunctionBody(Entity):
    def __init__(self): super().__init__(); self.statements: List[Statement] = []
    def rule(self): return "statements*;"

class Statement(Entity):
    def __init__(self): super().__init__(); self.lhs : StatementLhs = None; self.assignOp: str = None; self.rhs: Expression = None
    def rule(self): return "lhs assignOp rhs"

class StatementLhs(Entity):
    def __init__(self): super().__init__(); self.variables: List[ResultVariable] = []
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
# okay lets write some code

s_test_program = """
feature Hello extends Run
    type str | string = char$
    string out$ | output$
    on (string out$) << hello(string name)
        out$ << "hello \(name)"
    replace run()
        output$ << hello("world")
"""

@this_is_the_test
def test_zero():    
    log("test_zero")
    log(get_attribute_type(VariableDef, "type"))
    grammar = Grammar(Entity)
    log_clear()
    log(grammar.dbg())
    ls = lexer(Source(code= s_test_program))
    log(ls)
