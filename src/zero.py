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

class Named(Entity):
    def __init__(self): super().__init__(); self.name: str = None; self.alias: str = None
    
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
    def __init__(self): super().__init__()

# StructDef just declares a list of properties
class StructDef(TypeDef):
    def __init__(self): self.properties: List[VariableDef] = []
    def rule(self): return "'type' name '=' '{' properties*; '}'"

# ParentDef declares that type is a child of some other type(s)
class TypeParentDef(TypeDef):
    def __init__(self): self.parents: List[Type] = []
    def rule(self): return "'type' name '>' parents+,"

# ChildrenDef declares that type is a parent of some other type(s)
class TypeChildrenDef(TypeDef):
    def __init__(self): self.children: List[Type] = []
    def rule(self): return "'type' name '>' children+,"

#--------------------------------------------------------------------------------------------------
# VariableDefs

# VariableDef declares one or more variables with the same type and default value, using either c or ts notation
# the '+' indicates one or more, "," at the end is the separator
class VariableDef(Entity):
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

class FunctionCallArgument(FunctionCallItem):
    def __init__(self): super().__init__(); self.argument: str = None; self.value: Expression = None
    def rule(self): return "(argument '=')? value"


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
    grammar = Grammar()
    log_clear()
    log(grammar.dbg())
    ls = lexer(Source(code= s_test_program))
    log(ls)
