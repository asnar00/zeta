# ᕦ(ツ)ᕤ
# compiler.py
# author: asnaroo
# zero to anything

from typing import List
from util import *
from lexer import *
from grammar import *
from entity import *
from parser import *
from symbols import *
import zero_classes as zc

#--------------------------------------------------------------------------------------------------
# modular language definition; so we can add a bit at a time

# LanguageModule collects all compilation stages (grammar, parser, resolver, etc) for some part of the language
class LanguageModule:
    def __init__(self): pass
    def setup_syntax(self, compiler: 'Compiler'): pass            # add grammar rules and validation functions
    def setup_validate(self, compiler: 'Compiler'): pass          # add validate methods to rule classes
    def setup_naming(self, compiler: 'Compiler'): pass            # add naming methods to rule classes
    def setup_generate(self, compiler: 'Compiler'): pass          # add generate methods to rule classes    
    def setup_scope(self, compiler: 'Compiler'): pass             # add get_scope methods to rule classes
    def setup_symbols(self, compiler: 'Compiler'): pass           # add add_symbols/resolve methods to rule classes
    def setup_check_types(self, compiler: 'Compiler'): pass       # add check_type methods to rule classes
    def test_parser(self): pass                             # test parser with some examples

    def setup(self, compiler: 'Compiler'):
        self.setup_validate(compiler)
        self.setup_naming(compiler)
        self.setup_scope(compiler)
        self.setup_generate(compiler)
        self.setup_symbols(compiler)
        self.setup_check_types(compiler)
        self.test_parser()

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

#--------------------------------------------------------------------------------------------------
# Report holds log of everything that happened in a compilation stage
class Report:
    def __init__(self, name: str):
        self.name = name                                    # stage name
        self.report : List[Tuple[Entity|Lex,str]] = []      # log of all things done (for testing/debug): location, msg
        self.errors : List[Tuple[Entity|Lex,str]] = []      # all errors : location, msg
    def show(self) -> str:
        width = 80
        out = f"{self.name} {"-"*(width-len(self.name))}\n"
        for r in self.report:
            lex = get_first_lex(r[0])
            if lex: out += f"{log_grey(lex.location() if lex else "")}: \"{lex}\" => {r[1]}\n"
        if len(self.errors) > 0:
            out += "\n" + log_red(f"{len(self.errors)} errors\n")
            for e in self.errors:
                lex = get_first_lex(e[0])
                if lex: out += f"{log_grey(lex.location() if lex else "")}: \"{lex}\" => {e[1]}\n"
        return out

#--------------------------------------------------------------------------------------------------
# CompiledProgram holds ast, st, all the other artefacts
class CompiledProgram:
    def __init__(self):
        self.ast : Entity = None                            # abstract syntax tree: a tree of Entity objects
        self.st : SymbolTable = None                        # symbol table; maps name => {object, scope, tag}
        self.reports : List[Report] = []                     # all reports from all stages
    def show_report(self)->str:
        out = ""
        for report in self.reports:
            out += report.show()
        return out

#--------------------------------------------------------------------------------------------------
# Compiler collects all modules into one unit, and does the work

class Compiler:
    def __init__(self, import_module):
        self.import_module = import_module
        self.modules : List[LanguageModule] = []
        self.grammar : Grammar= Grammar(import_module)
        self.cp = CompiledProgram()

    def add_modules(self, modules: List[LanguageModule]): self.modules.extend(modules)
    
    def setup(self):
        for module in self.modules: module.setup_syntax(self)
        self.grammar.build_classes()
        for module in self.modules: module.setup(self)

    def compile(self, code: str) -> CompiledProgram:
        self.cp.ast = self.parse(code)
        if has_errors(self.cp.ast): return self.show_errors(self.cp.ast)
        self.generate_code(self.cp)
        self.add_symbols(self.cp)
        self.resolve_symbols(self.cp)
        self.check_types(self.cp)
        return self.cp
    
    def parse(self, code: str) -> Entity:
        ast = parse_simple(code, "Program")
        return ast
    
    def show_errors(self, ast: Entity):
        log_clear()
        log(log_red("errors in ast"))
        log(dbg_entity(ast))
        return ast
    
    def generate_code(self, cp: CompiledProgram) -> str:
        self.stage("generate code")
        visitor = Visitor(has_method="generate", is_ref=False, children_first=False)
        visitor.apply(cp.ast, lambda e, scope, type_name: e.generate())

    def add_symbols(self, cp: CompiledProgram) -> SymbolTable:
        self.stage("add symbols")
        cp.st = SymbolTable()
        visitor = Visitor(has_method="add_symbols", is_ref=False, children_first=False)
        visitor.apply(cp.ast, lambda e, scope, type_name: e.add_symbols(scope))

    def resolve_symbols(self, cp: CompiledProgram):
        self.stage("resolve symbols")
        errors = []
        visitor = Visitor("resolve", is_ref=True, children_first=True)
        visitor.apply(cp.ast, lambda e, scope, type_name: e.resolve(scope, type_name)) 

    def check_types(self, cp: CompiledProgram):
        self.stage("check types")
        visitor = Visitor("check_type", is_ref=False, children_first=True)
        errors = []
        visitor.apply(cp.ast, lambda e, scope, type_name: e.check_type(scope))
        pass

    #--------------------------------------------------------------------
    # below the line

    def stage(self, name: str): self.cp.reports.append(Report(name))
    def report(self, entity: Entity|Lex, msg: str): self.cp.reports[-1].report.append((entity, msg))
    def error(self, entity: Entity|Lex, msg: str): self.cp.reports[-1].errors.append((entity, msg))

    #--------------------------------------------------------------------
    # called by Entity methods
    
    # add symbol and report name and alias (if any); called by add_symbols
    def add_symbol(self, name: Lex, e: Entity, scope: Entity, alias: Lex=None):
        self.cp.st.add(name, e, scope, alias=alias)
        self.report(name, f"{e} in {scope}")
        if alias: self.report(alias, f"{e} in {scope}")

    # resolve a symbol name; called by resolve
    def find_symbol(self, name: Lex, of_type: Any, scope: Any, raise_errors: bool=True) -> Any:
        if isinstance(name, of_type): return name # already matched
        found = self.cp.st.find(name, of_type, scope)
        if len(found) == 1:
            self.report(name, f"{found[0].element} in {scope}")
            return found[0].element
        elif len(found) > 1:
            if raise_errors: self.error(name, f"multiple matches in {scope}")
        else:
            if raise_errors: self.error(name, f"no {of_type.__name__} in {scope}, {caller()}")
        return None
