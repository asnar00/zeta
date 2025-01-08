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
    def setup_constructors(self, compiler: 'Compiler'): pass      # add make_constructor methods to rule classes    
    def setup_scope(self, compiler: 'Compiler'): pass             # add get_scope methods to rule classes
    def setup_symbols(self, compiler: 'Compiler'): pass           # add add_symbols/resolve methods to rule classes
    def setup_check_types(self, compiler: 'Compiler'): pass       # add check_type methods to rule classes
    def test_parser(self, compiler: 'Compiler'): pass             # test parser with some examples

    def setup(self, compiler: 'Compiler'):
        self.setup_validate(compiler)
        self.setup_naming(compiler)
        self.setup_scope(compiler)
        self.setup_constructors(compiler)
        self.setup_symbols(compiler)
        self.setup_check_types(compiler)
        self.test_parser(compiler)

#--------------------------------------------------------------------------------------------------
# CompiledProgram holds ast, st, all the other artefacts

class CompiledProgram:
    def __init__(self, code: str):
        self.code = code
        self.ast : Entity = None                            # abstract syntax tree: a tree of Entity objects
        self.st : SymbolTable = None                        # symbol table; maps name => {object, scope, tag}
        self.reports : List[Report] = []                    # all reports from all stages

    def show_report(self) -> str:
        out = ""
        for i, report in enumerate(self.reports):
            out += self.reports[i].show(self.code)
        return out
    
    def is_ok(self) -> bool:
        return len(self.reports[-1].errors) == 0

#--------------------------------------------------------------------------------------------------
# Compiler collects all modules into one unit, and does the work

class Compiler:
    def __init__(self, import_module):
        self.import_module = import_module
        self.modules : List[LanguageModule] = []
        self.grammar : Grammar= Grammar(import_module)
        self.cp = None

    def add_modules(self, modules: List[LanguageModule]): self.modules.extend(modules)
    
    def setup(self):
        for module in self.modules: module.setup_syntax(self)
        self.grammar.build_classes()
        for module in self.modules: module.setup(self)

    def compile(self, code: str) -> CompiledProgram:
        code = code.strip()
        self.cp = CompiledProgram(code)
        self.cp.ast = self.parse(code)
        if has_errors(self.cp.ast): return self.show_errors(self.cp.ast)
        success = self.run_stages()
        for report in self.cp.reports: report.process()
        return self.cp
    
    def parse(self, code: str) -> Entity:
        ast = parse_simple(code, "Program", self.grammar)
        return ast
    
    def show_errors(self, ast: Entity):
        log(log_red("errors in ast"))
        log(dbg_entity(ast))
        return ast
    
    def run_stages(self) -> bool:
        if not self.constructors(): return False
        if not self.add_symbols(): return False
        if not self.resolve_symbols(): return False
        if not self.check_types(): return False
        return True
    
    def constructors(self) -> bool:
        self.stage("constructors")
        visitor = Visitor(has_method="make_constructor", is_ref=False, children_first=False)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.make_constructor())
        return self.cp.is_ok()

    def add_symbols(self) -> bool:
        self.stage("add symbols")
        self.cp.st = SymbolTable()
        visitor = Visitor(has_method="add_symbols", is_ref=False, children_first=False)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.add_symbols(scope))
        return self.cp.is_ok()
    
    def resolve_symbols(self) -> bool:
        self.stage("resolve symbols")
        visitor = Visitor("resolve", is_ref=True, children_first=True)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.resolve(scope, type_name)) 
        return self.cp.is_ok()
    
    def check_types(self) -> bool:
        self.stage("check types")
        visitor = Visitor(has_method="embracket", is_ref=False, children_first=False)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.embracket())
        visitor = Visitor("check_type", is_ref=False, children_first=True)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.check_type(scope))
        return self.cp.is_ok()

    #--------------------------------------------------------------------
    # below the line

    def stage(self, name: str): 
        self.cp.reports.append(Report(name))
    def report(self, lex: Lex, msg: str): 
        if not isinstance(lex, Lex): raise ValueError(f"lex is not a Lex: {lex}")
        self.cp.reports[-1].items.append(ReportItem(lex, msg))

    def error(self, lex: Lex, msg: str):
        if not isinstance(lex, Lex): raise ValueError(f"lex is not a Lex: {lex}")
        self.cp.reports[-1].errors.append(ReportItem(lex, msg))

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
            if isinstance(name, Lex):self.report(name, f"{found[0].element} in {scope}")
            return found[0].element
        elif len(found) > 1:
            if raise_errors: self.error(name, f"multiple matches in {scope}")
        else:
            if raise_errors: self.error(name, f"no {of_type.__name__} in {scope}, {caller()}")
        return None

#--------------------------------------------------------------------------------------------------
# Report holds log of everything that happened in a compilation stage

class ReportItem:
    def __init__(self, lex: Lex, msg: str):
        self.lex = lex
        self.msg = msg

class Report:
    def __init__(self, name: str):
        self.name : str = name
        self.items : List[ReportItem] = []
        self.errors : List[ReportItem] = []
    def process(self):
        self.sort_items(self.items)
        self.sort_items(self.errors)
        self.check_errors()
    def sort_items(self, items: List[ReportItem]):
        items.sort(key=lambda x: x.lex.location())
    def check_errors(self):
        self.errors = [item for item in self.errors if not self.error_was_overridden(item)]
    def error_was_overridden(self, item: ReportItem) -> bool:
        return any(item.lex == report_item.lex for report_item in self.items)
    def show(self, code: str) -> str:
        out = ""
        width = 80
        out += f"{self.name} {'-' * (width - len(self.name))}\n"
        if len(self.errors) > 0:
            out += log_red(f"{len(self.errors)} errors\n")
            out += self.show_simple(self.errors)
        else:
            out += log_green("no errors\n")
        out += self.show_simple(self.items)
        return out
    def show_simple(self, items: List[ReportItem]) -> str:
        out = ""
        for item in items:
            loc = log_grey(item.lex.location())
            out += f"{loc} \"{item.lex}\" {item.msg}\n"
        return out

#--------------------------------------------------------------------------------------------------
# visual output of errors/reports

# given pairs of (report, log_colour), return highlighted code

@log_suppress
def visual_report(reports: List[Tuple[List[ReportItem], Callable]], code: str) -> str:
    lines = code.strip().split("\n")
    n_digits = len(str(len(lines)))
    out = ""
    for i, line in enumerate(lines):
        log("------------------------------------------------")
        line = highlight_line(line, i+1, reports)
        log(line)
        out += f"{i+1:>{n_digits}} {line}\n"
    return out

def highlight_line(line: str, i_line: int, reports: List[Tuple[List[ReportItem], Callable]]) -> str:
    matching = []
    for items, colour in reports:
        for item in items:
            loc = item.lex.location()
            if loc.i_line == i_line and item.lex.val in line:
                matching.append((item.lex.val, loc.i_col-1, loc.i_col-1 + len(item.lex.val), colour))
    matching.sort(key=lambda x: x[1])
    unique = []
    for i, (val, i_col, j_col, colour) in enumerate(matching):
        log(val, i_col, j_col, colour.__name__)
        if i == 0 or i_col >= unique[-1][2]:
            unique.append((val, i_col, j_col, colour))
        else: 
            log("removed overlapping", val, i_col, j_col, colour.__name__)
    unique.sort(key=lambda x: x[1], reverse=True)
    for i, (val, i_col, j_col, colour) in enumerate(unique):
        log(val, i_col, j_col, colour.__name__)
        line = line[:i_col] + colour(line[i_col:j_col]) + line[j_col:]
    return line
        
        