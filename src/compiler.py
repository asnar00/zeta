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
from codegen import *
import zero_classes as zc

#--------------------------------------------------------------------------------------------------
# modular language definition; so we can add a bit at a time

# LanguageModule collects all compilation stages (grammar, parser, resolver, etc) for some part of the language
class LanguageModule:
    def __init__(self): pass
    def setup_grammar(self, compiler: 'Compiler'): pass            # add grammar rules and validation functions
    def setup_validate(self, compiler: 'Compiler'): pass          # add validate methods to rule classes
    def setup_naming(self, compiler: 'Compiler'): pass            # add naming methods to rule classes
    def setup_constructors(self, compiler: 'Compiler'): pass      # add make_constructor methods to rule classes    
    def setup_scope(self, compiler: 'Compiler'): pass             # add get_scope methods to rule classes
    def setup_symbols(self, compiler: 'Compiler'): pass           # add add_symbols/resolve methods to rule classes
    def setup_check_types(self, compiler: 'Compiler'): pass       # add check_type methods to rule classes
    def setup_generate(self, compiler: 'Compiler'): pass          # add generate methods to rule classes
    def test_parser(self, compiler: 'Compiler'): pass             # test parser with some examples

    def setup(self, compiler: 'Compiler'):
        self.setup_validate(compiler)
        self.setup_naming(compiler)
        self.setup_scope(compiler)
        self.setup_constructors(compiler)
        self.setup_symbols(compiler)
        self.setup_check_types(compiler)
        self.setup_generate(compiler)
        self.test_parser(compiler)

#--------------------------------------------------------------------------------------------------
# CompiledProgram holds ast, st, all the other artefacts

class CompiledProgram:
    def __init__(self, code: str):
        self.code = code
        self.ast : Entity = None                            # abstract syntax tree: a tree of Entity objects
        self.st : SymbolTable = None                        # symbol table; maps name => {object, scope, tag}
        self.assembly = None                                # assembly code (in our own vm isa)
        self.reports : List[Report] = []                    # all reports from all stages

    def show_report(self) -> str:
        out = ""
        for i, report in enumerate(self.reports):
            out += self.reports[i].show(self.code)
        return out
    
    def is_ok(self) -> bool:
        return len(self.reports) > 0 and len(self.reports[-1].errors) == 0

#--------------------------------------------------------------------------------------------------
# Compiler collects all modules into one unit, and does the work

class Compiler:
    def __init__(self, import_module):
        self.import_module = import_module
        self.modules : List[LanguageModule] = []
        self.grammar : Grammar= Grammar(import_module)
        self.cp = None
        self.config = CodegenConfig()
        self.codegen = CodeGenerator()

    def add_modules(self, modules: List[LanguageModule]): self.modules.extend(modules)
    def set_concrete_types(self, types: Dict[str, str]): self.config.concrete_types(types)

    def setup(self):
        for module in self.modules: module.setup_grammar(self)
        self.grammar.build_classes()
        for module in self.modules: module.setup(self)

    def compile(self, code: str) -> CompiledProgram:
        code = code.strip()
        self.cp = CompiledProgram(code)
        if not self.parse(code): return self.cp
        success = self.run_stages()
        for report in self.cp.reports: report.process()
        return self.cp
    
    def parse(self, code: str) -> bool: # returns True if ok
        self.stage("parse")
        self.cp.ast = parse_simple(code, "Program", self.grammar)
        if has_errors(self.cp.ast):
            errors = get_error_list(self.cp.ast)
            log(errors)
            for error in errors:
                self.error(error.lex, f"{error.message}: expected {error.expected}")
            return False
        return True
    
    def show_errors(self, ast: Entity):
        log(log_red("errors in ast"))
        log(dbg_entity(ast))
        
        return ast
    
    def run_stages(self) -> bool:
        if not self.constructors(): return False
        if not self.add_symbols(): return False
        if not self.resolve_symbols(): return False
        if not self.embracket(): return False
        if not self.check_types(): return False
        if not self.generate(): return False
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
    
    def embracket(self) -> bool:
        self.stage("embracket")
        visitor = Visitor(has_method="embracket", is_ref=False, children_first=False)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.embracket())
        return self.cp.is_ok()
    
    def check_types(self) -> bool:
        self.stage("check types")
        visitor = Visitor("check_type", is_ref=False, children_first=True)
        visitor.apply(self.cp.ast, lambda e, scope, type_name: e.check_type(scope))
        return self.cp.is_ok()
    
    def generate(self) -> bool:
        self.stage("codegen")
        self.codegen.setup(self.config, self.cp.st, self.grammar)
        ast = self.cp.ast
        if hasattr(ast, "generate"): ast.generate()
        else: log_exit("no generate method in ast")
        optimiser = Optimiser(self.codegen.block)
        optimiser.optimise()
        self.cp.assembly = self.codegen.block
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
    # called by Entity methods in add_symbols, resolve, type
    
    # add symbol and report name and alias (if any); called by add_symbols
    def add_symbol(self, name: Lex, e: Entity, scope: Entity, alias: Lex=None):
        self.cp.st.add(name, e, scope, alias=alias)
        self.report(name, f"{e} in {scope}")
        if alias: self.report(alias, f"{e} in {scope}")

    # resolve a symbol name; called by resolve
    def find_symbol(self, name: Lex, of_type: Any, scope: Any, raise_errors: bool, read_only: bool) -> Any:
        if isinstance(name, of_type): return name # already matched
        found = self.cp.st.find(name, of_type, scope, read_only)
        if len(found) == 1:
            if isinstance(name, Lex):
                self.report(name, f"{found[0].element} in {found[0].scope}")
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
        #self.check_errors()
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

def visual_report_from_stage(stage: Report, code: str) -> str:
    out = "------------------------------------------------------------------------------------\n"
    out += "output from stage: \"" + stage.name + "\"\n"
    if len(stage.errors) > 0: out += log_red(f"({len(stage.errors)} errors)")
    else: out += log_green("(no errors)")
    out += "\n\n"
    out += visual_report([(stage.items, log_green), (stage.errors, log_red)], code)
    return out

# given pairs of (report, log_colour), return highlighted code
@log_suppress
def visual_report(reports: List[Tuple[List[ReportItem], Callable]], code: str) -> str:
    lines = code.strip().split("\n")
    n_digits = len(str(len(lines)))
    out = ""
    for i, line in enumerate(lines):
        log("------------------------------------------------")
        line = highlight_line(line, i+1, reports, n_digits)
        log(line)
        out += log_grey(f"{i+1:>{n_digits}} ") + f"{line}\n"
    return out

def highlight_line(line: str, i_line: int, reports: List[Tuple[List[ReportItem], Callable]], n_digits: int) -> str:
    matching = []
    for items, colour in reports:
        for item in items:
            loc = item.lex.location()
            if loc.i_line == i_line and item.lex.val in line:
                matching.append((item, loc.i_col-1, colour))
    matching.sort(key=lambda x: x[1])
    unique = []
    preamble = ""
    for i, (item, i_col, colour) in enumerate(matching):
        j_col = i_col + len(item.lex.val)
        log(item.lex.val, i_col, j_col, colour.__name__)
        if i == 0 or i_col >= unique[-1][2]:
            unique.append((item, i_col, j_col, colour))
            n_chars_add = i_col - len(preamble)
            preamble += (" " * (n_chars_add-1)) + "⏐"
        else: 
            log("removed overlapping", item.lex.val, i_col, j_col, colour.__name__)
    unique.sort(key=lambda x: x[1], reverse=True)
    if len(unique) == 0: return line

    tags = []
    start = " "*(n_digits+2)
    for i, (item, i_col, j_col, colour) in enumerate(unique):
        log(item.lex.val, i_col, j_col, colour.__name__)
        line = line[:i_col] + colour(line[i_col:j_col]) + line[j_col:]
        msg = item.msg
        if colour.__name__ == "log_red": msg = log_red(msg) 
        else: msg = log_grey(msg)
        tags.append(start + log_grey(preamble[0:i_col-1] + "+- ") + msg)
    if len(tags) > 0: line = line + "\n" + "\n".join(tags) + "\n"
    return line
        
        