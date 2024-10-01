# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from util import *
from typing import List, Dict, Tuple, Union

#--------------------------------------------------------------------------------------------------
# General design principles:

# keep classes small: do computation in functions wherever possible
# keep functions small: half a screenful is a good limit

# as a general guide, if you have to scroll to understand something, it's too big

#--------------------------------------------------------------------------------------------------
# Source management

# markdown files contain text and code snippets
# Source extracts the code snippets into a single piece of text
# and maintains a map to let us find line numbers in the original text

@this_is_a_test
def test_source():
    log("test_source")

    # you can create a Source from code directly
    source = Source(code = "a b c")
    test("source", source.code, "a b c")

    # or from markdown text directly
    source = Source(text = "some code:\n    a b c\n    d e f\n\nsome text.")
    test("source", source.code, "a b c\n    d e f")

    # or from a markdown file path
    source = Source(path = 'src/test/Hello.zero.md')
    test("source", source.code, """
        feature Hello extends Main
            > hello()
            => "hello world"
            > hello()
            on (out$: string) << hello()
                out$ << "hello world"
        on (string out$) << hello() {
            out$ << "hello world";
        }
        on (out$ : string) << hello() {
            out$ << "hello world";
        }
        on (out$ : string) << hello():
            out$ << "hello world"
        on (string out$) << hello()
            out$ << "hello world"
         """)
    
    # in all cases, source location should map back to the right place
    test("source_location", source.location(0), "src/test/Hello.zero.md:22:1")

class Source:
    s_cwd = os.getcwd()
    def __init__(self, path=None, text=None, code=None):
        self.path = path
        self.text = read_file(path) if path else text
        self.code = code
        self.map = []
        if self.text and not self.code: self.extract_code()
        elif self.code and not self.text: self.setup_map()

    def extract_code(self):
        log("extracting code")
        lines = self.text.split('\n')
        if lines[-1] == '': lines.pop()
        self.map = [] # line number to line in original text
        self.code = ""
        for i, line in enumerate(lines):
            if line.startswith("    "):
                self.code += line[4:] + "\n"
                self.map.append(i)
        self.code = self.code.strip()

    def setup_map(self):
        self.code = self.code.strip()
        self.text = self.code
        for i, line in enumerate(self.code.split('\n')):
            self.map.append(i)

    def location(self, pos: int) -> str:
        line = self.code[:pos].count("\n")              # first, find line number in code
        original_line = self.map[line]                  # then, find the line number in the original text using the map:
        column = pos - self.code[:pos].rfind("\n") - 1  # count backwards from pos to the start of the line it's on
        path = self.path.replace(Source.s_cwd, "") if self.path else ""
        return f"{path}:{original_line+1}:{column+1}"

#--------------------------------------------------------------------------------------------------
# Lexer

# Lexer turns source code into a list of lexemes (Lex)
# each has type, value, and position in source
# Lexer is indent-agnostic: it handles python-style or cpp-style indents
# producing a "clean" lex-list with only newlines, indents and undents
# the test shows how all three inputs (blended, python, typescript) produce the same lex-string

@this_is_a_test
def test_lexer():
    log("test_lexer")
    lexer_result = """[on, (, out$, :, string, ), <<, hello, (, ), :indent, out$, <<, "hello world", :undent, next]"""
    test("lexer_simple", lexer(Source(code = """
on (out$ : string) << hello()
    out$ << "hello world"
next   
""")), lexer_result)
    test("lexer_py", lexer(Source(code = """
on (out$ : string) << hello():
    out$ << "hello world"
next   
""")), lexer_result)
    test("lexer_ts", lexer(Source(code = """
on (out$ : string) << hello() {
    out$ << "hello world";
}
next   
""")), lexer_result)
    ls = lexer(Source(code="(a + b) * c"))
    test("lexer_jump", ls[0].jump, 4)

# lexeme: stores value and type, and also position within the source
# we're loading quite a bit of computed stuff onto it, but that speeds up parsing later
class Lex:
    def __init__(self, source: Source, pos: int, val: str, type: str):
        self.source = source;   # source object we're in
        self.pos = pos;         # character position in the source
        self.val = val;         # "value" - the text (might be different from source, if indent/etc)
        self.type = type;       # type - identifier, number, operator, etc
        self.rank = 0           # rank of the lexeme if an operator (for precedence)
        self.index = None       # index in the original lex list (doesn't change if we take slices etc)
        self.jump = None        # increment to jump to next/prev matching bracket ("()", "[]", "indent/undent")
    def __str__(self):
        return self.val
    def __repr__(self):
        val = str(self)
        if self.type == "newline" : return log_grey(val)
        else: return val
    def dbg(self):
        return f"<{self.type}> {self.val} {self.jump or ""}"
    def location(self):
        return self.source.location(self.pos)
    
# lexer: reads source and produces lexemes
def lexer(source: Source) -> List[Lex]:
    ls = naive_lexer(source)
    ls = insert_ws_indents(ls)
    ls = handle_braces(ls)
    ls = finalise_indents(ls)
    ls = filter_newlines(ls)
    ls = compute_jumps(ls)
    ls = compute_ranks(ls)
    for i, lex in enumerate(ls): lex.index = i
    return ls

# naive lexer just does a straight lex
def naive_lexer(source: Source) -> List[Lex]:
    ls = []
    specs = [ ('number', r'\d+(\.\d*)?'),                           # integer or decimal number
                ('identifier', r'[A-Za-z_][A-Za-z0-9_$\[\]]*'),     # identifiers
                ('string', r'"(?:\\.|[^"\\])*"'),                   # string literals with support for escaped quotes
                ('operator', r'[-+=%^<>*/?!|&#.\\]{1,2}'),          # operators, and double-operators
                ('punctuation', r'[(){}\[\],;:]'),                  # punctuation
                ('newline', r'(^[ ]+)|(\n[ ]*)'),                   # line-start plus 0 or more spaces
                ('whitespace', r'[ ]+')]                            # spaces
    patterns = '|'.join('(?P<%s>%s)' % pair for pair in specs)
    regex = re.compile(patterns)
    pos = 0
    while pos < len(source.code):
        m = regex.match(source.code, pos)
        if not m: raise Exception(f'unexpected character "{source.code[pos]}" at {log_short(source.code[pos:])}')
        if len(m.group()) == 0: raise Exception(f'empty match at {log_short(source.code[pos:])}')
        type = m.lastgroup
        val = m.group()
        if (type != 'whitespace'):
            if type == 'newline':
                ls.append(Lex(source, pos, val.replace("\n", "↩︎\n").replace(" ", "_"), type))
            else:
                ls.append(Lex(source, pos, val, type))
        pos += len(val)
    return ls

# replaces all 'newline' lexemes with the appropriate ws-indent/undent/newline lexemes
def insert_ws_indents(ls: List[Lex]) -> List[Lex]:
    ols = []
    last_indent = 0
    for lex in ls:
        if lex.type == "newline":
            indent = len(lex.val) // 4
            if indent > last_indent:
                if len(ols) > 0 and ols[-1].val == ":": ols.pop()
                for i in range(indent - last_indent):
                    ols.append(Lex(lex.source, lex.pos, "ws-indent", "indent"))
            elif indent < last_indent:
                for i in range(last_indent - indent):
                    ols.append(Lex(lex.source, lex.pos, "ws-undent", "undent"))
            else:
                ols.append(Lex(lex.source, lex.pos, "ws-newline", "newline"))
            last_indent = indent
        else:
            ols.append(lex)
    return ols

# ensures that any braces / semicolons are handled properly for cpp/ts/etc
def handle_braces(ls: List[Lex]) -> List[Lex]:
    ols = []
    i = 0
    while i < len(ls):
        lex = ls[i]
        if lex.val == "{":      # add an indent, remove all subsequent ws-indents
            ols.append(Lex(lex.source, lex.pos, ":indent", "indent"))
            i += 1
            while i < len(ls) and ls[i].val.startswith("ws-") : i += 1
            i -= 1
        elif lex.val == "}":    # add an undent, remove all previous ws-undents
            while len(ols) > 0 and ols[-1].val.startswith("ws-"): ols.pop()
            ols.append(Lex(lex.source, lex.pos, ":undent", "undent"))
        elif lex.val == ";":
            ols.append(Lex(lex.source, lex.pos, ":newline", "newline"))
        else:
            ols.append(lex)
        i += 1
    return ols

# replace all 'ws-' tags with normal tags
def finalise_indents(ls: List[Lex]) -> List[Lex]:
    for lex in ls:
        if lex.val.startswith("ws-"):
            lex.val = ":" + lex.val[3:]
    return ls

# get rid of any newlines that sit next to indents or undents
def filter_newlines(ls: List[Lex]) -> List[Lex]:
    ols = []
    i = 0
    while i < len(ls):
        lex = ls[i]
        if lex.val == ":newline":
            preceding_is_ndent = (i > 0 and ls[i-1].val in [":indent", ":undent"])
            following_is_ndent = (i < (len(ls)-1) and ls[i+1].val in [":indent", ":undent"])
            if preceding_is_ndent or following_is_ndent: pass
            else:
                ols.append(lex)
        else:
            ols.append(lex)
        i += 1
    return ols

# each open or close bracket ("(", "[", indent) should store a jump its matching bracket
def compute_jumps(ls: List[Lex]) -> List[Lex]:
    bracket_level = 0
    open = ["(", "[", ":indent"]
    close = [")", "]", ":undent"]
    i_open_lex : List[int] = []
    for i, lex in enumerate(ls):
        if lex.val in open:
            i_open_lex.append(i)
        elif lex.val in close:
            if len(i_open_lex) == 0: raise Exception(f"unmatched bracket at {lex.location()}")
            i_open = i_open_lex.pop()
            ls[i_open].jump = i - i_open    # forward jump
            lex.jump = i_open - i           # backward jump
    return ls

def compute_ranks(ls: List[Lex]) -> List[Lex]:
    for lex in ls:
        if lex.val in ["*", "/", "**", "//"]: lex.rank = 2
        elif lex.val in ["+", "-", "++", "--"]: lex.rank = 1
        elif lex.val in ["=", "<<"]: lex.rank = 0
    return ls

#--------------------------------------------------------------------------------------------------
# the complete grammar spec for the zero programming language

zero_grammar = """
    feature_decl := "feature" name:<identifier> ("extends" parent:<identifier>)? <indent> body:component* <undent>
    component := (type_decl | variable_decl | function_decl)

    type_decl := "type" name_decl (type_decl_rhs)
    type_decl_rhs := (type_structure_decl | type_relation_decl)
    type_structure_decl := "=" <indent> properties:variable_decl+ <undent>
    type_relation_decl := "<" parent:<identifier>

    variable_decl := variable_type_decl ("=" default:expression)?
    variable_type_decl := (c_style_var_type_decl | ts_style_var_type_decl)

    c_style_var_type_decl := type:<identifier> variable_names:name_decl+,
    ts_style_var_type_decl := variable_names:name_decl+, ":" type:<identifier>
    name_decl := short:<identifier> ("|" long:<identifier>)?

    function_decl := modifier:function_modifier "(" result:variable_type_decl ")" assign:("=" | "<<") signature:signature <indent> body:function_body <undent>
    function_modifier := ("on" | "after" | "before" | "replace")
    signature := short:signature_single ("|" long:signature_single)?
    signature_single := components:signature_component+
    signature_component := (signature_word | signature_parameter_group)
    signature_word := word:(<identifier> | <operator>)
    signature_parameter_group := "(" parameters:variable_decl*, ")"

    function_body := statement*
    statement := lhs:variable_assign op:("=" | "<<") rhs:expression
    variable_assign := (variable_decl | variable)
    variable := name:<identifier>

    expression := (constant | variable | brackets | function | operation)
    constant := (<number> | <string>)
    brackets := "(" expression ")"

    function := (function_call_word | function_call_params)+
    function_call_word := word:<identifier>
    function_call_params := "(" params:function_call_param*, ")"
    function_call_param := function_call_var? value:expression
    function_call_var := name:<identifier> ":"

    operation := (infix | prefix | postfix)
    infix := lhs:expression op:<operator> rhs:expression
    prefix := op:<operator> rhs:expression
    postfix := lhs:expression op:<operator>
"""

#--------------------------------------------------------------------------------------------------
# Grammar: derived from the grammar spec

class Term:
    def __init__(self, var: str, vals: List[str], decorator: str=None, separator: str=None):
        self.var = var                                  # variable name to bind the result to
        self.vals = vals                                # list of possible values (<identifier>, "keyword", rule)
        self.rules = [s_grammar.rule(val) for val in vals if not is_terminal(val)]                                 # list of rules that this term "points to"
        self.decorator = decorator                      # optional decorator ("*", "+", "?")
        self.separator = separator                      # if a list, the separator ("" or "," or "|") usually
        self.is_terminal = self.compute_is_terminal()   # if all values are <identifier> or "keyword", we're a terminal
    def __str__(self):
        vals_str = " | ".join(self.vals)
        if len(self.vals) > 1: vals_str = f"({vals_str})"
        if self.decorator: vals_str += self.decorator
        if self.separator: vals_str += self.separator
        if self.var: vals_str = f"{self.var}:{vals_str}"
        return vals_str
    def __repr__(self): return str(self)
    def dbg(self): return str(self)
    def compute_is_terminal(self) -> bool:
        for val in self.vals: 
            if not (val.startswith("<") or val.startswith('"')): return False
        return True
    def terminals(self) -> List[str]:
        return self.vals if self.is_terminal else []
    
def is_terminal(val: str) -> bool:
    return val.startswith("<") or val.startswith('"')

class Rule:
    def __init__(self, name: str, terms: List[Term] = None):
        self.name = name
        self.terms = terms or []
        self.parent = None
        self.children = []
    def __str__(self): return self.name
    def __repr__(self): return str(self)
    def dbg(self): return f"{self.name} := {' '.join([str(t) for t in self.terms])}"

s_grammar = None        # global singleton for convenience

class Grammar:
    def __init__(self, spec: str):
        self.rules = []             # array of rules
        self.rule_dict = {}         # map name => rule
        global s_grammar            # global singleton
        s_grammar = self            # for convenience
        parse_grammar(self, spec)   # do all hefty work in functions to avoid statefulmess

    def add_rule(self, rule: Rule):
        self.rules.append(rule)
        self.rule_dict[rule.name] = rule

    def rule(self, name: str) -> Rule:
        return self.rule_dict[name]
    
    def __str__(self):
        return "\n".join([rule.dbg() for rule in self.rules])
    def __repr__(self): return str(self)

# parse grammar spec into rules
@log_indent
def parse_grammar(grammar: Grammar, spec: str):
    lines = spec.strip().split("\n")
    lines = [line.strip() for line in lines if line.strip() != ""]
    # first add all rules to the grammar, so we can refer to Rules directly
    for line in lines:
        parts = line.split(":=")
        rule_name = parts[0].strip()
        grammar.add_rule(Rule(rule_name))
    # now parse each line
    for line in lines:
        log("\n\n", line)
        parts = line.split(":=")
        rule_name = parts[0].strip()
        rhs = parts[1].strip()
        parse_rule(rule_name, rhs)

# parses a single line of the grammar into one, possibly more, rules
def parse_rule(rule_name: str, rhs: str) -> List[Rule]:
    term_strs = split_terms(rhs)
    for i_term, term_str in enumerate(term_strs):
        parse_term(rule_name, term_str, i_term)

# splits a rule into term strings
@log_indent
def split_terms(terms_str: str) -> List[str]:
    # split by spaces, but keep things in brackets together
    # e.g. "a b (c d) e" -> ["a", "b", "(c d)", "e"]
    terms = []
    term = ""
    in_brackets = False
    for c in terms_str:
        if c == " " and not in_brackets:
            terms.append(term)
            term = ""
        elif c == "(":
            in_brackets = True
            term += c
        elif c == ")":
            in_brackets = False
            term += c
        else:
            term += c
    terms.append(term)
    return terms

@log_indent
def parse_term(rule_name: str, term_str: str, i_term: int):
    rule = s_grammar.rule(rule_name)
    # if the term has the form xxx:yyy, extract both
    parts = re.match(r"(\w+):(.+)", term_str)
    var = parts.group(1) if parts else None
    term_str = parts.group(2) if parts else term_str
    # if the term ends with a decorator (*, +, ?) optionally followed by a separator, extract both
    parts = re.match(r"(.+)([*+?])([,|]?)", term_str)
    separator = parts.group(3) if parts else None
    decorator = parts.group(2) if parts else None
    term_str = parts.group(1) if parts else term_str
    if term_str.startswith("(") and term_str.endswith(")"):
        term = parse_bracketed_term(rule, var, term_str, decorator, separator, i_term)
    else:
        term = Term(var, [term_str], decorator, separator)
    rule.terms.append(term)

@log_indent
def parse_bracketed_term(rule, var, term_str, decorator, separator, i_term):
    term_str = term_str[1:-1] # remove brackets
    or_list = [t.strip() for t in term_str.split(" | ")]
    # if you can split it using " | " , it's an "or" list:
    if len(or_list) > 1: return Term(var, or_list, decorator, separator)
    # otherwise, it's a sub-sequence, make a sub-rule for it and return that
    new_rule_name = f"{rule.name}_{i_term}"
    new_rule = Rule(new_rule_name)
    s_grammar.add_rule(new_rule)
    parse_rule(new_rule_name, term_str)
    return Term(var, [new_rule_name], decorator, separator)

#--------------------------------------------------------------------------------------------------
@this_is_the_test
def test_grammar():
    log("test_grammar")
    grammar = Grammar(zero_grammar)
    test("grammar", grammar, """
feature_decl := "feature" name:<identifier> feature_decl_2? <indent> body:component* <undent>
component := (type_decl | variable_decl | function_decl)
type_decl := "type" name_decl type_decl_2
type_decl_rhs := (type_structure_decl | type_relation_decl)
type_structure_decl := "=" <indent> properties:variable_decl+ <undent>
type_relation_decl := "<" parent:<identifier>
variable_decl := variable_type_decl variable_decl_1?
variable_type_decl := (c_style_var_type_decl | ts_style_var_type_decl)
c_style_var_type_decl := type:<identifier> variable_names:name_decl+,
ts_style_var_type_decl := variable_names:name_decl+, ":" type:<identifier>
name_decl := short:<identifier> name_decl_1?
function_decl := modifier:function_modifier "(" result:variable_type_decl ")" assign:("=" | "<<") signature:signature <indent> body:function_body <undent>
function_modifier := ("on" | "after" | "before" | "replace")
signature := short:signature_single signature_1?
signature_single := components:signature_component+
signature_component := (signature_word | signature_parameter_group)
signature_word := word:(<identifier> | <operator>)
signature_parameter_group := "(" parameters:variable_decl*,
function_body := statement*
statement := lhs:variable_assign op:("=" | "<<") rhs:expression
variable_assign := (variable_decl | variable)
variable := name:<identifier>
expression := (constant | variable | brackets | function | operation)
constant := (<number> | <string>)
brackets := "(" expression ")"
function := (function_call_word | function_call_params)+
function_call_word := word:<identifier>
function_call_params := "(" params:function_call_param*,
function_call_param := function_call_var? value:expression
function_call_var := name:<identifier> ":"
operation := (infix | prefix | postfix)
infix := lhs:expression op:<operator> rhs:expression
prefix := op:<operator> rhs:expression
postfix := lhs:expression op:<operator>
feature_decl_2 := "extends" parent:<identifier>
type_decl_2 := type_decl_rhs
variable_decl_1 := "=" default:expression
name_decl_1 := "|" long:<identifier>
signature_1 := "|" long:signature_single
         """)
    

    


