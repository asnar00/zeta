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
    def scope(self): pass                         # add get_scope methods to rule classes
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

# FoundItem is a helper class
class FoundItem:
    def __init__(self, e: Entity, scope: Any, parent: Entity, name: str, ref: str):
        self.e = e
        self.scope = scope
        self.parent = parent
        self.name = name
        self.ref = ref

# Language collects all modules into one unit
class Language:
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
            module.test_parser()

    def compile(self, code: str) -> CompiledProgram:
        cp = CompiledProgram()
        cp.ast = self.parse(code)
        if has_errors(cp.ast): return self.show_errors(cp.ast)
        cp.st = self.build_symbol_table(cp.ast)
        self.resolve_symbols(cp)
        self.check_types(cp)
        return cp
    
    def parse(self, code: str) -> Entity:
        return parse_simple(code, "Program")
    
    def show_errors(self, ast: Entity):
        log_clear()
        log(log_red("errors in ast"))
        log(dbg_entity(ast))
        return ast
    
    def build_symbol_table(self, ast: Entity) -> SymbolTable:
        log_clear()
        log("build_symbol_table")
        self.collect_entities(ast, zc.Entity, False, True)

    def resolve_symbols(self, cp: CompiledProgram):
        pass

    def check_types(self, cp: CompiledProgram):
        pass

    def collect_entities(self, e: Entity, select_type: Any, should_be_reference: bool, children_first: bool):
        visited = set()
        scope = None
        results = []
        self.collect_entities_rec(e, scope, select_type, should_be_reference, children_first, "", None, visited, results)
        for f in results:
            log(f"{f.e} in {f.scope}, {f.parent}.{f.name} [{f.ref}]")
        
    def collect_entities_rec(self, e: Entity, scope: Any, select_type: Any, should_be_reference: bool, children_first: bool, name: str, parent: Entity, visited: Set, results: List[FoundItem]):
        if isinstance(e, Entity):
            if e in visited: return
            visited.add(e)
        type_match = False if select_type is None else isinstance(e, select_type)
        cf = f"{e}"
        if isinstance(e, Lex): cf = '"' + cf + '"'
        attr_type_in_parent = Grammar.current.get_attribute_type(parent.__class__, name) if parent else ""
        ref = attr_type_in_parent if "&" in attr_type_in_parent else ""
        is_ref = ref != ""
        if should_be_reference==True and type_match==False:
            if select_type.__name__ in ref: type_match = True
        if name: cf = f"{name}:{ref} {cf}"
        should_add_to_results = False
        if type_match and (should_be_reference is None or should_be_reference == is_ref): 
            should_add_to_results = True

        if children_first == False and should_add_to_results == True:
            results.append(FoundItem(e, scope, parent, name, ref))

        if hasattr(e, "get_scope"):
            scope = e.get_scope()
            cf += log_orange(" **")
        if not isinstance(e, Lex) and not ref=="&":
            for attr in vars(e):
                if attr == "_error": continue
                attr_val = getattr(e, attr)
                if attr_val is None: continue
                is_list = isinstance(attr_val, List)
                if is_list:
                    #log(f"{start}  {attr}")
                    for val in attr_val:
                        self.collect_entities_rec(val, scope, select_type, should_be_reference, children_first, "", e, visited, results)
                else:
                    self.collect_entities_rec(attr_val, scope, select_type, should_be_reference, children_first, attr, e, visited, results)
    
        if children_first == True and should_add_to_results == True:
            results.append(FoundItem(e, scope, parent, name, ref))