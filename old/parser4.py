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
        self.jump = 1           # increment to jump to next/prev matching bracket ("()", "[]", "indent/undent")
    def __str__(self):
        return self.val
    def __repr__(self):
        val = str(self)
        if self.type == "newline" : return log_grey(val)
        else: return val
    def dbg(self):
        return f"<{self.type}> {self.val} {self.jump}"
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
    type_decl_rhs := (type_structure_decl | type_relation_decl | type_union_decl)
    type_structure_decl := "=" <indent> properties:variable_decl+ <undent>
    type_relation_decl := "<" parent:<identifier>
    type_union_decl := "=" children:<identifier>+|

    variable_decl := variable_type_decl ("=" default:expression)?
    variable_type_decl := (c_style_var_type_decl | ts_style_var_type_decl)

    c_style_var_type_decl := type:<identifier> variable_names:name_decl+,
    ts_style_var_type_decl := variable_names:name_decl+, ":" type:<identifier>
    name_decl := short:<identifier> ("|" long:<identifier>)?

    function_decl := modifier:("on" | "after" | "before" | "replace") "(" result:variable_type_decl ")" assign:("=" | "<<") signature:signature <indent> body:function_body <undent>
    signature := short:signature_single ("|" long:signature_single)?
    signature_single := components:signature_element+
    signature_element := (signature_word | signature_parameter_group)
    signature_word := word:(<identifier> | <operator>)
    signature_parameter_group := "(" parameters:variable_decl*, ")"

    function_body := statement*
    statement := lhs:variable_assign op:("=" | "<<") rhs:expression
    variable_assign := (variable_decl | variable)
    variable := <identifier>

    expression := elements:(constant | operator | word | brackets)+
    constant := (<number> | <string>)
    operator := <operator>
    word := <identifier>
    brackets := "(" bracket_element*, ")"
    bracket_element := (param_assign)? expression
    param_assign := variable+, "="

"""

#--------------------------------------------------------------------------------------------------
# Grammar: derived from the grammar spec

class Term:
    def __init__(self, var: str, vals: List[str], decorator: str=None, separator: str=None):
        self.var = var                                  # variable name to bind the result to
        self.vals = vals                                # list of possible values (<identifier>, "keyword", rule)
        self.decorator = decorator                      # optional decorator ("*", "+", "?")
        self.separator = separator                      # if a list, the separator ("" or "," or "|") usually
        self.rules = [s_grammar.rule(val) for val in vals if not (val.startswith("<") or val.startswith('"'))]    # list of rules that this term "points to"
    def __str__(self):
        vals_str = " | ".join(self.vals)
        if len(self.vals) > 1: vals_str = f"({vals_str})"
        if self.decorator: vals_str += self.decorator
        if self.separator: vals_str += self.separator
        if self.var: vals_str = f"{self.var}:{vals_str}"
        return vals_str
    def __repr__(self): return str(self)
    def dbg(self): 
        out = str(self)
        if is_terminal(self):
            out = log_grey_background(out)
        return out

class Rule:
    def __init__(self, name: str, terms: List[Term] = None):
        self.name = name
        self.terms = terms or []
        self.parent = None
        self.children = []
    def __str__(self): return self.name
    def __repr__(self): return str(self)
    def dbg(self): return f"{self.name} := {' '.join([t.dbg() for t in self.terms])}"

s_grammar = None        # global singleton for convenience

class Grammar:
    def __init__(self, spec: str):
        self.rules = []             # array of rules
        self.rule_named = {}        # map name => rule
        global s_grammar            # global singleton
        s_grammar = self            # for convenience
        build_grammar(self, spec)   # do all hefty work in functions to avoid statefulmess

    def add_rule(self, rule: Rule):
        self.rules.append(rule)
        self.rule_named[rule.name] = rule

    def add_rule_after(self, rule: Rule, after: Rule):
        i = self.rules.index(after)
        self.rules.insert(i+1, rule)
        self.rule_named[rule.name] = rule

    def rule(self, name: str) -> Rule:
        return self.rule_named[name]
    
    def __str__(self):
        return "\n".join([rule.dbg() for rule in self.rules])
    def __repr__(self): return str(self)

# parse grammar spec into rules
def build_grammar(grammar: Grammar, spec: str):
    lines = spec.strip().split("\n")
    lines = [line.strip() for line in lines if line.strip() != ""]
    # first add all rules to the grammar, so we can refer to Rules directly
    for line in lines:
        parts = line.split(":=")
        rule_name = parts[0].strip()
        grammar.add_rule(Rule(rule_name))
    # now parse each line
    for line in lines:
        parts = line.split(":=")
        rule_name = parts[0].strip()
        rhs = parts[1].strip()
        build_rule(rule_name, rhs)
    # finally sort all rules within terms by complexity
    for rule in grammar.rules:
        for term in rule.terms:
            if len(term.rules) > 0:
                term.rules.sort(key=lambda r: complexity(r), reverse=True)     # sort rules by number of terms, so we can match the longest first

# parses a single line of the grammar into one, possibly more, rules
def build_rule(rule_name: str, rhs: str) -> List[Rule]:
    term_strs = split_terms(rhs)
    for i_term, term_str in enumerate(term_strs):
        build_term(rule_name, term_str, i_term)

# splits a rule into term strings
def split_terms(terms_str: str) -> List[str]:
    # split by spaces, but keep things in brackets together
    # e.g. "a b (c d) e" -> ["a", "b", "(c d)", "e"]
    terms = []
    term = ""
    in_brackets = False
    in_quotes = False
    for c in terms_str:
        if not in_quotes:
            if c == " " and not in_brackets:
                terms.append(term)
                term = ""
            elif c == "(":
                in_brackets = True
                term += c
            elif c == ")":
                in_brackets = False
                term += c
            elif c == '"':
                in_quotes = True
                term += c
            else:
                term += c
        else:
            term += c
            if c == '"': in_quotes = False
    terms.append(term)
    return terms

def build_term(rule_name: str, term_str: str, i_term: int):
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
        term = build_bracketed_term(rule, var, term_str, decorator, separator, i_term)
    else:
        term = Term(var, [term_str], decorator, separator)
    rule.terms.append(term)

def build_bracketed_term(rule, var, term_str, decorator, separator, i_term):
    term_str = term_str[1:-1] # remove brackets
    or_list = [t.strip() for t in term_str.split(" | ")]
    # if you can split it using " | " , it's an "or" list:
    if len(or_list) > 1: return Term(var, or_list, decorator, separator)
    # otherwise, it's a sub-sequence, make a sub-rule for it and return that
    new_rule_name = f"{rule.name}_{i_term}"
    new_rule = Rule(new_rule_name)
    s_grammar.add_rule_after(new_rule, rule)
    build_rule(new_rule_name, term_str)
    return Term(var, [new_rule_name], decorator, separator)

#--------------------------------------------------------------------------------------------------
@this_is_a_test
def test_grammar():
    log("test_grammar")
    grammar = Grammar(zero_grammar)
    test("grammar", grammar)
    
#--------------------------------------------------------------------------------------------------
# Parser!

# an AST node
class Node:
    def __init__(self, ls: List[Lex] = None, rule: Rule=None, children: List['Node'] =None, list:str=None):
        self.rule = rule
        self.ls = ls if ls else []
        self.children = children if children else []
        self.list = list
    def __str__(self): 
        if self.list: return f"[" + ", ".join(str(c) for c in self.children) + "]"
        return f"{self.rule_name()+": " if self.rule else ""}{self.ls_str()}"
    def __repr__(self): return str(self)
    def ls_str(self): return '"' + " ".join(str(lex) for lex in self.ls) + '"'
    def rule_name(self): return self.rule.name if self.rule else ""
    def length(self) -> int: return len(self.ls)
    def is_empty(self) -> bool: return len(self.ls) == 0
    def is_lex(self) -> bool: return len(self.ls)==1 and not self.rule and not self.list
    def pos(self) -> int: return self.ls[0].index if len(self.ls) > 0 else None
    def count_layers(self, i_layer:int = 1) -> int:
        if len(self.children) == 0: return i_layer
        return max([c.count_layers(i_layer+1) for c in self.children if c])
    def get_layer(self, i_layer:int, this_layer:int=0) -> List['Node']:
        if this_layer == i_layer: return [self]
        return [c for c in self.children for c in c.get_layer(i_layer, this_layer+1)]
    s_dw = 16       # display width
    def display(self):
        n_layers = self.count_layers()
        out = ""
        for i_layer in range(0, n_layers):
            nodes = self.get_layer(n_layers - 1 - i_layer)
            pos = 0
            for node in nodes:
                node_pos = node.pos() if node.length() > 0 else pos
                while pos < node_pos: out += self.pad("", 1); pos += 1
                bg_fn = log_green_background if node.rule else log_grey_background
                out += self.pad(str(node), node.length(), bg_fn)
                pos += node.length()
            out += "\n"
        log(out)
    def pad(self, s: str, n: int, bg_fn=None) -> str:
        padding = ((n * (Node.s_dw-1)) + (n-1)) - len(s)
        p0 = padding // 2
        p1 = padding - p0
        str = (" " * p0) + s + (" "*p1)
        if bg_fn: str = bg_fn(str)
        return str + " "
    @log_indent
    def get_ast(self):
        log("self.rule", self.rule)
        if len(self.children) == 0: 
            ast = self.ls[0] if len(self.ls) == 1 else self.ls
            return  { "_" + self.rule.name: ast } if self.rule else ast
        if self.list:
            ast = [c.get_ast() for c in self.children if not c.is_lex()]
            var = self.rule.terms[0].var
            ast = { var : ast } if var else ast
            return { "_" + self.rule.name: ast }
        if not self.rule: raise Exception("node has no rule, and is not a terminal or list")
        if len(self.rule.terms) != len(self.children): raise Exception("node has wrong number of children")
        ast = {}
        if len(self.rule.terms) == 1: # singular, eg. variable or constant; unnamed is OK
            term = self.rule.terms[0]
            if term.var: ast[term.var] = self.children[0].get_ast()
            else: ast = self.children[0].get_ast()
        else: # multiple terms, so each item needs a var, otherwise discard
            for i, term in enumerate(self.rule.terms):
                if not is_keyword(term):
                    if term.var: ast[term.var] = self.children[i].get_ast()
                    else:
                        child_ast = self.children[i].get_ast()
                        if isinstance(child_ast, dict):
                            ast.update(list(child_ast.values())[0])
                        elif term.decorator == "?" and len(child_ast) == 1:
                            ast.update(list(child_ast[0].values())[0])
                        else: log("warning: discarding", child_ast)
        return { "_" + self.rule.name: ast }

#--------------------------------------------------------------------------------
# memoisation: eventually move this down to utils
from functools import wraps

def memoise(func):
    cache = {}
    
    @wraps(func)
    def wrapper(obj):
        # Use the object's id as the key in our cache dictionary
        obj_id = id(obj)
        
        if obj_id in cache:
            return cache[obj_id]
        else:
            cache[obj_id] = None    # stops infinite recursion
            result = func(obj)
            cache[obj_id] = result
            return result
    
    return wrapper

#--------------------------------------------------------------------------------
# grammar invariants: memoise these

# a rule is abstract if it has only one term, with one or more rules
@memoise
def is_abstract(rule: Rule) -> bool:
    if len(rule.terms) != 1: return False
    term = rule.terms[0]
    return len(term.vals) == len(term.rules)

# a term is "terminal" if it's a keyword or type, and has no decorator
@memoise
def is_terminal(term: Term) -> bool:
    result= len(term.rules) == 0 and not(term.decorator)
    return result

# a term is a keyword if there's no rules, no decorator, and the value starts with a quote
@memoise
def is_keyword(term: Term) -> bool:
    return len(term.rules) == 0 and not(term.decorator) and term.vals[0].startswith('"')

# complexity roughly estimates how much effort it is to parse something
@memoise
def complexity(rule_or_term: Union[Rule, Term]) -> int:
    if isinstance(rule_or_term, Rule):
        return sum([complexity(term) for term in rule_or_term.terms]) + len(rule_or_term.terms)
    if isinstance(rule_or_term, Term):
        term = rule_or_term
        if len(term.rules) > 0: return sum([complexity(rule) or 0 for rule in term.rules])
        else: return len(term.vals)

# terminators are any "" or <> terminal that appears after a rule
@memoise
def terminators(rule: Rule) -> List[str]:
    result = []
    for check_rule in s_grammar.rules:
        for i_term, term in enumerate(check_rule.terms):
            if rule in term.rules:
                # if the term after this is a terminal, add its vals to the list
                if i_term < len(check_rule.terms)-1:
                    next_term = check_rule.terms[i_term+1]
                    if is_terminal(next_term): result += next_term.vals
                # if the term is optional, add the vals of the next term after that
                if term.decorator == "?":
                    if i_term < len(check_rule.terms)-2:
                        next_term = check_rule.terms[i_term+2]
                        if is_terminal(next_term): result += next_term.vals
                # if the term is a decorator, add the separator
                elif term.decorator in '+*': 
                    if term.separator: result.append(term.separator)
                # if this is the last term, add <eof> and the terminators of the check_rule
                if i_term == len(check_rule.terms)-1:
                    check_rule_terminators = terminators(check_rule)
                    if check_rule_terminators != None: result += check_rule_terminators
                    result += ["<eof>"]
    return result

#--------------------------------------------------------------------------------
# Parser

# step forward one lexeme, skipping over bracketed-sections
def step_forward(ls: List[Lex], i: int) -> int:
    return i + ls[i].jump

# check if a lexeme matches a terminal term (list of keywords or list of types)
def lex_matches_terminal(lex: Lex, term: Term) -> bool:
    if f'"{lex.val}"' in term.vals: return True
    elif f'<{lex.type}>' in term.vals: return True
    return False

# scan forward to find the next lexeme that matches a terminal term
@log_disable
def scan_match_terminal(ls: List[Lex], i_lex: int, term: Term) -> int:
    while i_lex < len(ls):
        if lex_matches_terminal(ls[i_lex], term):
            return i_lex
        i_lex = step_forward(ls, i_lex)
    return i_lex

# scans forward through each terminal term, trying to match with a lex
# returns success/failure and the list of indexes into the lexes
@log_indent
def find_rule_terminals(ls: List[Lex], rule: Rule) -> Tuple[bool, List[int]]:
    i_lex_result = []
    i_lex = 0
    for i_term, term in enumerate(rule.terms):
        log(i_term, ":", term)
        if is_terminal(term):
            i_lex_next = scan_match_terminal(ls, i_lex, term)
            if i_lex_next == len(ls): return False, i_lex_result
            if i_lex_next > i_lex and i_term ==0: return False, i_lex_result
            i_lex_result.append(i_lex_next)
            i_lex = step_forward(ls, i_lex_next)
    return True, i_lex_result

# parse a single abstract term (one or more rules), ignoring decorator
@log_indent
def parse_simple_abstract_term(ls: List[Lex], rules: List[Rule]) -> Node:
    for rule in rules:
        node = parse_rule(ls, rule)
        if node: return node
    return None

# parse a single abstract term (one or more rules), not ignoring decorator
@log_indent
def parse_abstract_rule(ls: List[Lex], rule: Rule) -> Node:
    term = rule.terms[0]
    if term.decorator: return parse_decorated_nonterminal(ls, term, rule)
    else: return parse_simple_abstract_term(ls, term.rules)

# parse a single term, ignoring the decorator
@log_indent
def parse_simple_nonterminal(ls: List[Lex], term: Term) -> Node:
    if len(term.rules) == 0: raise Exception("parse_simple_nonterminal only supports non-terminals")
    if len(term.rules) > 1: return parse_simple_abstract_term(ls, term.rules)
    return parse_rule(ls, term.rules[0])

# parse a single term, with a decorator, returning a single (possibly empty) node
@log_indent
def parse_decorated_nonterminal(ls: List[Lex], term: Term, rule: Rule) -> Node:
    min_count = 0 if term.decorator != "+" else 1
    max_count = None if term.decorator != "?" else 1
    nodes = []
    i_lex = 0
    safe_count = 100
    while i_lex < len(ls) and (max_count is None or len(nodes) < max_count):
        safe_count -= 1
        if safe_count == 0: raise Exception("infinite loop in parse_decorated_nonterminal")
        node = parse_simple_nonterminal(ls[i_lex:], term)
        if not node: break
        nodes.append(node)
        i_lex += node.length()
        if term.separator and i_lex < len(ls): # match separator
            if ls[i_lex].val == term.separator: i_lex += 1
    if len(nodes) < min_count: return None
    return Node(ls[:i_lex], rule=rule, children=nodes, list=term.decorator)

# parses a single term from a lex-range
@log_indent
def parse_nonterminal(ls: List[Lex], term: Term) -> Node:
    if term.decorator: # ?/+/*
        log("decorator:", term.decorator)
        return parse_decorated_nonterminal(ls, term, None)
    return parse_simple_nonterminal(ls, term)
    
# parses one or more terms from a lexeme range
@log_indent
def parse_nonterminals(ls: List[Lex], terms: List[Term]) -> List[Node]:
    if len(terms) != 1: raise Exception("parse_terms only supports one term")
    return [parse_nonterminal(ls, terms[0])]

# parse a rule 
@log_indent
def parse_rule(ls: List[Lex], rule: Rule) -> Node:
    if is_abstract(rule):
        return parse_abstract_rule(ls, rule)
    success, i_lex_terminals = find_rule_terminals(ls, rule)
    if not success: return None
    # run through and generate sub-nodes for all terms
    log("i_lex_terminals:", i_lex_terminals)
    nodes = []
    i_term_terminals = [i for i, term in enumerate(rule.terms) if is_terminal(term)]
    i_last_term = -1
    i_last_lex = -1
    for i_terminal in range(len(i_lex_terminals)):
        i_term = i_term_terminals[i_terminal]
        prev_terms = rule.terms[i_last_term+1:i_term]   # terms between last terminal and this terminal
        if len(prev_terms) > 0 and i_terminal > 0:
            prev_ls = ls[i_lex_terminals[i_terminal-1]+1:i_lex_terminals[i_terminal]]
            nodes += parse_nonterminals(prev_ls, prev_terms)
        i_last_lex = i_lex_terminals[i_terminal]
        terminal_node = Node(ls[i_last_lex:i_last_lex+1])
        nodes.append(terminal_node)
        i_last_term = i_term
    result = Node(ls[0:i_last_lex+1], rule, nodes)
    return result

@log_indent
def parse(code: str, rule_name: str=None) -> Dict:
    source = Source(code=code)
    rule = s_grammar.rule(rule_name) if rule_name else s_grammar.rules[0]
    ls = lexer(source)
    node = parse_rule(ls, rule)
    node.display()
    return node.get_ast()

@this_is_the_test
def test_parser():
    log("test_parser")
    grammar = Grammar(zero_grammar)
    terms = grammar.rule("expression").terms[0].rules
    log(parse("a", "bracket_element"))


    log_flush()
    #test("parse_variable", parse("a", "expression"), """{'_variable': a}""")
    #test("parse_constant", parse("1.25", "expression"), """{'_constant': 1.25}""")
    #test("parse_empty_feature_no_parent", parse("feature Hello { }"), """{'_feature_decl': {'name': Hello, 'body': []}}""")
    #test("parse_empty_feature_parent", parse("feature Hello extends Main { }"), """{'_feature_decl': {'name': Hello, 'parent': Main, 'body': []}}""")
    

