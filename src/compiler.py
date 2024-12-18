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
    def define(self, grammar: Grammar): pass      # add grammar rules and validation functions
    def methods(self, grammar: Grammar): pass      # add methods to existing rule classes
    def test(self): pass                          # test parser with some examples

# CompiledProgram holds ast, st, all the other artefacts
class CompiledProgram:
    def __init__(self):
        self.ast = None
        self.st = None

# Language collects all modules into one unit
class Language:
    def __init__(self, import_module):
        self.import_module = import_module
        self.modules = []
        self.grammar = Grammar()
    def add_modules(self, modules: List[LanguageModule]): self.modules.extend(modules)
    def setup(self):
        for module in self.modules: module.define(self.grammar)
        self.grammar.build_classes()
        self.grammar.write_classes(self.import_module.__file__)
        importlib.reload(self.import_module)
        self.grammar.set_rule_classes(self.import_module)
        for module in self.modules: 
            module.methods(self.grammar)
            module.test()
    def compile(self, code: str) -> CompiledProgram:
        cp = CompiledProgram()
        ls = lexer(Source(code = code))
        reader = Reader(ls)
        rule = Grammar.current.rule_named["Program"]
        cp.ast = parse_rule(rule, reader)
        cp.st = SymbolTable()
        log_clear()
        cp.st.add_symbols(cp.ast, None)
        log(cp.st.dbg())
        cp.st.resolve_symbols(cp.ast)
        log_exit("done resolving")
        return cp