# ᕦ(ツ)ᕤ
# compiler.py
# author: asnaroo
# zero to anything

from typing import List
from util import *
from lexer import *
from grammar import *
from parser import *
from symbols import *
import importlib
import zero_classes as zc

#--------------------------------------------------------------------------------------------------
# modular language definition; so we can add a bit at a time

# LanguageModule collects all compilation stages (grammar, parser, resolver, etc) for some part of the language
class LanguageModule:
    def __init__(self): pass
    def syntax(self, grammar: Grammar): pass      # add grammar rules and validation functions
    def validate(self): pass                      # add validate methods to rule classes
    def naming(self): pass                        # add naming methods to rule classes
    def generate(self): pass                      # add generate methods to rule classes    
    def scope(self): pass                         # add get_scope methods to rule classes
    def symbols(self): pass                       # add add_symbols/resolve methods to rule classes
    def check_types(self): pass                   # add check_type methods to rule classes
    def test_parser(self): pass                   # test parser with some examples

    # add method to class (general)
    def method(self, cls: Type[T], method_name: str="") -> Callable:
        def decorator(func: Callable) -> Callable:
            class_name = cls.__name__
            
            # Convert standalone function to method
            @wraps(func)
            def method(self, *args, **kwargs):
                return func(self, *args, **kwargs)
                
            # Add the method to the class
            setattr(cls, func.__name__, method)
            
            # Important: return the original function or method
            return method
        
        return decorator

# CompiledProgram holds ast, st, all the other artefacts
class CompiledProgram:
    def __init__(self):
        self.ast = None
        self.st = None
        self.errors = []
        self.found = []

# Compiler collects all modules into one unit
class Compiler:
    def __init__(self, import_module):
        self.import_module = import_module
        self.modules = []
        self.grammar = Grammar()

    def add_modules(self, modules: List[LanguageModule]): self.modules.extend(modules)

    def setup(self):
        for module in self.modules: module.syntax(self.grammar)
        self.grammar.build_classes()
        self.grammar.write_classes(self.import_module.__file__)
        importlib.reload(self.import_module)
        self.grammar.set_rule_classes(self.import_module)
        for module in self.modules: 
            module.validate()
            module.naming()
            module.scope()
            module.generate()
            module.symbols()
            module.check_types()
            module.test_parser()

    def compile(self, code: str) -> CompiledProgram:
        cp = CompiledProgram()
        cp.ast = self.parse(code)
        if has_errors(cp.ast): return self.show_errors(cp.ast)
        self.generate_code(cp)
        self.build_symbol_table(cp)
        self.resolve_symbols(cp)
        self.check_types(cp)
        return cp
    
    def parse(self, code: str) -> Entity:
        ast = parse_simple(code, "Program")
        log(dbg_entity(ast))
        return ast
    
    def show_errors(self, ast: Entity):
        log_clear()
        log(log_red("errors in ast"))
        log(dbg_entity(ast))
        return ast
    
    def generate_code(self, cp: CompiledProgram) -> str:
        log_clear()
        log("generate code -----------------------------------------")
        visitor = Visitor(has_method="generate", is_ref=False, children_first=False)
        visitor.apply(cp.ast, lambda e, scope, type: e.generate())

    def build_symbol_table(self, cp: CompiledProgram) -> SymbolTable:
        log_clear()
        log("build_symbol_table -----------------------------------------")
        cp.st = SymbolTable()
        visitor = Visitor(has_method="add_symbols", is_ref=False, children_first=False)
        visitor.verbose(True)
        visitor.apply(cp.ast, lambda e, scope, type: e.add_symbols(scope, cp.st))
        

    def resolve_symbols(self, cp: CompiledProgram):
        log_clear()
        log("resolve_symbols -----------------------------------------")
        errors = []
        visitor = Visitor("resolve", is_ref=True, children_first=True)
        visitor.apply(cp.ast, lambda e, scope, type_name: e.resolve(scope, cp.st, type_name, errors))
        log(print_code_formatted(cp.ast))
        

    def check_types(self, cp: CompiledProgram):
        log_clear()
        log("check_types -----------------------------------------")
        visitor = Visitor("check_type", is_ref=False, children_first=True)
        errors = []
        visitor.apply(cp.ast, lambda e, scope, type_name: e.check_type(cp.st, scope, errors))
        pass

#--------------------------------------------------------------------------------------------------
# visitor runs across the tree in specified order, calling method on each matching entity

class Visitor:
    def __init__(self, has_method: str, is_ref: bool, children_first: bool):
        self.has_method = has_method                # method name to match, or None if all
        self.is_ref = is_ref                        # match only references, or None if don't care
        self.children_first = children_first        # visit children before calling fn, or not
        self.fn = None                              # function to call on each match
        self.vb = False

    def verbose(self, vb: bool): self.vb = vb

    def apply(self, e: Entity, fn: Callable):   # call this from outside
        self.fn = fn
        self.visit_rec(e, None, set(), None, None, None, 0)

    # the main visitor recursive function... 
    def visit_rec(self, e: Entity, scope: Entity, visited: Set, parent: Entity, parent_attr: str, parent_index: int, indent:int):        
        if not (isinstance(e, Entity) or isinstance(e, Lex)): return # allow addition of randomly-typed properties to ast entities

        if isinstance(e, Entity):        # don't visit the same one twice, unless we're a Lex
            if e in visited: return
            visited.add(e)
        
        entity_type_name, is_ref = self.get_type(parent, parent_attr) # find the attribute type
        
        match = self.is_match(e, is_ref) # check if this node matches the spec
        
        self.log(e, scope, match, indent, parent, parent_attr, parent_index, entity_type_name)
        
        # if we match, call the function (if children_first is False)
        if match and self.children_first == False:
            self.call_fn(e, scope, entity_type_name, parent, parent_attr, parent_index, indent)
        
        # visit children    
        if not isinstance(e, Lex) and not is_ref:
            self.visit_children(e, scope, visited, parent, indent)
        
        # if we matched, call function (if children_first is True)
        if match and self.children_first == True:
            self.call_fn(e, scope, entity_type_name, parent, parent_attr, parent_index, indent)

    #----------------------------------------------------------------------------------------------
    # below the line 

    def get_type(self, parent: Entity, parent_attr: str):
        attr_type_name = Grammar.current.get_attribute_type(parent.__class__, parent_attr) if parent else ""
        is_ref = "&" in attr_type_name
        entity_type_name = attr_type_name.replace("List[", "").replace("]", "").replace("&", "")
        return entity_type_name, is_ref
    
    def is_match(self, e: Entity|Lex, is_ref: bool):
        if self.has_method and not hasattr(e, self.has_method): return False
        if isinstance(e, Entity):
            if is_ref: return False # never traverse references!
            if self.has_method and not hasattr(e, self.has_method):
                return False
        elif isinstance(e, Lex): # safe to traverse references, but only if requested
            if self.is_ref != None:
                if self.is_ref != is_ref:
                    return False
        return True

    def call_fn(self, e: Entity, scope: Entity, entity_type_name: str, parent: Entity, parent_attr: str, parent_index: int, indent: int):
        start = " " * indent
        new_node = self.fn(e, scope, entity_type_name)
        if new_node: 
            if self.vb: log(log_green(f"{start}set {parent}.{parent_attr} ==> {new_node}"))
            self.set_node(new_node, parent, parent_attr, parent_index, indent)
           
    def visit_children(self, e: Entity, scope: Entity, visited: Set, parent: Entity, indent:int):
        start = " " * indent

        if hasattr(e, "get_scope"):
            scope = e.get_scope() or scope
            if not isinstance(scope, Entity):
                log(log_red(f"{e}.get_scope returned {scope} ({type(scope).__name__})"))
                return
            
        for attr in vars(e):
            val = getattr(e, attr)
            if val is None: continue
            type_name, is_ref = self.get_type(e, attr)
            is_list = isinstance(val, List)
            if is_list:
                if len(val) == 0: continue
                for i, val in enumerate(val):
                    if not (is_ref and isinstance(val, Entity)): # don't traverse references
                        self.visit_rec(val, scope, visited, e, attr, i, indent+1)
            else:
                if not (is_ref and isinstance(val, Entity)): # don't traverse references
                    self.visit_rec(val, scope, visited, e, attr, None, indent+1)


    def visit_attrs(self, attrs: List[Tuple[str, Entity]], e: Entity, scope: Entity, visited: Set, parent: Entity, indent:int):
        for attr, attr_val in attrs:
            if attr == "_error": continue
            is_list = isinstance(attr_val, List)
            if is_list:
                for i, val in enumerate(attr_val):
                    self.visit_rec(val, scope, visited, e, attr, i, indent+1)
            else:
                self.visit_rec(attr_val, scope, visited, e, attr, None, indent+1)

    def set_node(self, new_node: Entity, parent: Entity, attr: str, index: int, indent:int):
        start = " " * indent
        ind = f"[{index}]" if index else ""
        if not parent: return
        if index == None: setattr(parent, attr, new_node)
        else: getattr(parent, attr)[index] = new_node

    def log(self, e: Entity, scope: Entity,match: bool, indent: int, parent: Entity, parent_attr: str, parent_index: int, entity_type_name: str):
        if not self.vb: return
        start = " " * indent
        se = f"{e}" if isinstance(e, Entity) else f'"{e}"'
        log(f"{start}{se} in {scope}")

    def vblog(self, *args):
        if not self.vb: return
        log(*args)

#--------------------------------------------------------------------------------------------------
# test-test

def test_symbol_table(st):
    d = st.dbg()
    