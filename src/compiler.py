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

#--------------------------------------------------------------------------------------------------
# modular language definition; so we can add a bit at a time

# LanguageModule collects all compilation stages (grammar, parser, resolver, etc) for some part of the language
class LanguageModule:
    def __init__(self): pass
    def syntax(self, grammar: Grammar): pass      # add grammar rules and validation functions
    def methods(self, grammar: Grammar): pass      # add methods to existing rule classes
    def test(self): pass                          # test parser with some examples

# CompiledProgram holds ast, st, all the other artefacts
class CompiledProgram:
    def __init__(self):
        self.ast = None
        self.st = None
        self.errors = []
        self.found = []

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
            module.methods(self.grammar)
            module.test()

    def compile(self, code: str) -> CompiledProgram:
        cp = CompiledProgram()
        cp.ast = self.parse(code)
        if has_errors(cp.ast): return self.show_errors(cp.ast)
        cp.st = self.build_symbol_table(cp.ast)
        self.resolve_symbols(cp)
        
        return cp
    
    def parse(self, code: str) -> Entity:
        return parse_simple(code, "Program")
    
    def show_errors(self, ast: Entity):
        log_clear()
        log(log_red("errors in ast"))
        log(dbg_entity(ast))
        return ast
    
    @log_suppress
    def build_symbol_table(self, ast: Entity) -> SymbolTable:
        st = SymbolTable()
        log("adding symbols for ast")
        st.add_symbols(ast, None)
        log(st.dbg())
        return st
       
    def resolve_symbols(self, cp: CompiledProgram):
        errors = []
        found = []
        visited = []
        for key, items in cp.st.symbols.items():
            for item in items:
                cp.st.resolve_symbols(item.element, item.scope, errors, found, visited)
        cp.st.resolve_symbols(cp.ast, None, errors, found, visited)
        log(print_code_formatted(cp.ast))
        log_exit("resolve_symbols")