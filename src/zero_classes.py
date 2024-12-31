# ᕦ(ツ)ᕤ
# /Users/asnaroo/Desktop/experiments/zeta/zero_classes.py
# auto-generated by zeta.py
# zero to anything

from grammar import Entity
from typing import List, Dict, Type

class NameDef(Entity):
    def __init__(self, name: 'str' =None, alias: 'str' =None):
        super().__init__()
        self.name: str = name
        self.alias: str = alias

class FeatureDef(Entity):
    def __init__(self, name: 'str' =None, alias: 'str' =None, parent: 'FeatureDef' =None, components: 'List[Component]' =None):
        super().__init__()
        self.name: str = name
        self.alias: str = alias
        self.parent: FeatureDef = parent        # ref
        self.components: List[Component] = components

class Component(Entity):
    def __init__(self, ):
        super().__init__()

class ContextDef(Entity):
    def __init__(self, name: 'str' =None, alias: 'str' =None, feature: 'List[FeatureDef]' =None):
        super().__init__()
        self.name: str = name
        self.alias: str = alias
        self.feature: List[FeatureDef] = feature        # ref

class Program(Entity):
    def __init__(self, components: 'List[FeatureDef|ContextDef]' =None):
        super().__init__()
        self.components: List[FeatureDef|ContextDef] = components

class Expression(Entity):
    def __init__(self, ):
        super().__init__()

class Constant(Expression):
    def __init__(self, value: 'str' =None):
        super().__init__()
        self.value: str = value

class VariableRef(Expression):
    def __init__(self, variable: 'Variable' =None, property: 'VariableRef' =None):
        super().__init__()
        self.variable: Variable = variable        # ref
        self.property: VariableRef = property

class Bracketed(Expression):
    def __init__(self, expression: 'Expression' =None):
        super().__init__()
        self.expression: Expression = expression

class FunctionCall(Expression):
    def __init__(self, items: 'List[FunctionCallItem]' =None):
        super().__init__()
        self.items: List[FunctionCallItem] = items

class FunctionCallItem(Entity):
    def __init__(self, ):
        super().__init__()

class FunctionCallConstant(FunctionCallItem):
    def __init__(self, constant: 'Constant' =None):
        super().__init__()
        self.constant: Constant = constant

class FunctionCallOperator(FunctionCallItem):
    def __init__(self, word: 'str' =None):
        super().__init__()
        self.word: str = word

class FunctionCallWord(FunctionCallItem):
    def __init__(self, word: 'str' =None):
        super().__init__()
        self.word: str = word

class FunctionCallVariable(FunctionCallItem):
    def __init__(self, variable: 'Variable' =None, property: 'VariableRef' =None):
        super().__init__()
        self.variable: Variable = variable        # ref
        self.property: VariableRef = property

class FunctionCallArguments(FunctionCallItem):
    def __init__(self, arguments: 'List[FunctionCallArgument]' =None):
        super().__init__()
        self.arguments: List[FunctionCallArgument] = arguments

class FunctionCallArgument(Entity):
    def __init__(self, argument: 'Variable' =None, value: 'Expression' =None):
        super().__init__()
        self.argument: Variable = argument        # ref
        self.value: Expression = value

class Variable(Entity):
    def __init__(self, type: 'Type' =None, name: 'str' =None, alias: 'str' =None, value: 'Expression' =None):
        super().__init__()
        self.type: Type = type        # ref
        self.name: str = name
        self.alias: str = alias
        self.value: Expression = value

class VariableDef(Component):
    def __init__(self, type: 'Type' =None, names: 'List[NameDef]' =None, value: 'Expression' =None):
        super().__init__()
        self.type: Type = type        # ref
        self.names: List[NameDef] = names
        self.value: Expression = value

class Type(Entity):
    def __init__(self, name: 'str' =None, alias: 'str' =None, types: 'List[Type]' =None, type: 'Type' =None, decorator: 'str' =None, properties: 'List[Variable]' =None, parents: 'List[Type]' =None, children: 'List[Type]' =None, options: 'List[str]' =None):
        super().__init__()
        self.name: str = name
        self.alias: str = alias
        self.types: List[Type] = types        # ref
        self.type: Type = type        # ref
        self.decorator: str = decorator
        self.properties: List[Variable] = properties
        self.parents: List[Type] = parents        # ref
        self.children: List[Type] = children        # ref
        self.options: List[str] = options

class TypeDef(Component):
    def __init__(self, names: 'List[NameDef]' =None, rhs: 'TypeRhs' =None):
        super().__init__()
        self.names: List[NameDef] = names
        self.rhs: TypeRhs = rhs

class TypeRhs(Entity):
    def __init__(self, ):
        super().__init__()

class TypeAlias(TypeRhs):
    def __init__(self, type: 'Type' =None):
        super().__init__()
        self.type: Type = type        # ref

class StructDef(TypeRhs):
    def __init__(self, properties: 'List[VariableDef]' =None):
        super().__init__()
        self.properties: List[VariableDef] = properties

class TypeParentDef(TypeRhs):
    def __init__(self, parents: 'List[Type]' =None):
        super().__init__()
        self.parents: List[Type] = parents        # ref

class TypeChildrenDef(TypeRhs):
    def __init__(self, children: 'List[Type]' =None):
        super().__init__()
        self.children: List[Type] = children        # ref

class TypeEnumDef(TypeRhs):
    def __init__(self, options: 'List[EnumOption]' =None):
        super().__init__()
        self.options: List[EnumOption] = options

class EnumOption(Entity):
    def __init__(self, ):
        super().__init__()

class EnumOptionId(EnumOption):
    def __init__(self, val: 'str' =None):
        super().__init__()
        self.val: str = val

class EnumOptionNumber(EnumOption):
    def __init__(self, val: 'str' =None):
        super().__init__()
        self.val: str = val

class MaybeTypes(Type):
    def __init__(self, types: 'List[Type]' =None):
        super().__init__()
        self.types: List[Type] = types        # ref

class MultipleTypes(Type):
    def __init__(self, types: 'List[Type]' =None):
        super().__init__()
        self.types: List[Type] = types        # ref

class TypeRef(Type):
    def __init__(self, type: 'Type' =None, decorator: 'str' =None):
        super().__init__()
        self.type: Type = type        # ref
        self.decorator: str = decorator

class Function(Entity):
    def __init__(self, results: 'FunctionResults' =None, signature: 'FunctionSignature' =None, body: 'FunctionBody' =None):
        super().__init__()
        self.results: FunctionResults = results
        self.signature: FunctionSignature = signature
        self.body: FunctionBody = body

class FunctionDef(Component):
    def __init__(self, modifier: 'str' =None, results: 'FunctionResults' =None, signature: 'FunctionSignature' =None, body: 'FunctionBody' =None):
        super().__init__()
        self.modifier: str = modifier
        self.results: FunctionResults = results
        self.signature: FunctionSignature = signature
        self.body: FunctionBody = body

class FunctionModifier(Entity):
    def __init__(self, modifier: 'str' =None):
        super().__init__()
        self.modifier: str = modifier

class FunctionResults(Entity):
    def __init__(self, results: 'List[FunctionResultVariableDef]' =None, assign_op: 'str' =None):
        super().__init__()
        self.results: List[FunctionResultVariableDef] = results
        self.assign_op: str = assign_op

class FunctionResultVariableDef(Entity):
    def __init__(self, type: 'Type' =None, names: 'List[NameDef]' =None):
        super().__init__()
        self.type: Type = type        # ref
        self.names: List[NameDef] = names

class FunctionSignature(Entity):
    def __init__(self, elements: 'List[FunctionSignatureElement]' =None):
        super().__init__()
        self.elements: List[FunctionSignatureElement] = elements

class FunctionSignatureElement(Entity):
    def __init__(self, ):
        super().__init__()

class FunctionSignatureWord(FunctionSignatureElement):
    def __init__(self, word: 'str' =None):
        super().__init__()
        self.word: str = word

class FunctionSignatureParams(FunctionSignatureElement):
    def __init__(self, params: 'List[VariableDef]' =None):
        super().__init__()
        self.params: List[VariableDef] = params

class FunctionBody(Entity):
    def __init__(self, ):
        super().__init__()

class FunctionStatements(FunctionBody):
    def __init__(self, statements: 'List[Statement]' =None):
        super().__init__()
        self.statements: List[Statement] = statements

class EmptyFunctionBody(FunctionBody):
    def __init__(self, ):
        super().__init__()

class Statement(Entity):
    def __init__(self, lhs: 'StatementLhs' =None, rhs: 'Expression' =None):
        super().__init__()
        self.lhs: StatementLhs = lhs
        self.rhs: Expression = rhs

class StatementLhs(Entity):
    def __init__(self, variables: 'List[ResultVariable]' =None, assign_op: 'str' =None):
        super().__init__()
        self.variables: List[ResultVariable] = variables
        self.assign_op: str = assign_op

class ResultVariable(Entity):
    def __init__(self, ):
        super().__init__()

class ResultVariableRef(ResultVariable):
    def __init__(self, variable: 'Variable' =None, property: 'VariableRef' =None):
        super().__init__()
        self.variable: Variable = variable        # ref
        self.property: VariableRef = property

class ResultVariableDef(ResultVariable):
    def __init__(self, type: 'Type' =None, name: 'str' =None, alias: 'str' =None):
        super().__init__()
        self.type: Type = type        # ref
        self.name: str = name
        self.alias: str = alias

class TestDef(Component):
    def __init__(self, lhs: 'Expression' =None, rhs: 'Expression' =None):
        super().__init__()
        self.lhs: Expression = lhs
        self.rhs: Expression = rhs
