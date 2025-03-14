# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from util import *
from typing import List, Dict, Tuple, Union
import json

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
    test("source_direct", source.code, "a b c")

    # or from markdown text directly
    source = Source(text = "some code:\n    a b c\n    d e f\n\nsome text.")
    test("source_markdown", source.code, "a b c\n    d e f")

    # or from a markdown file path
    source = Source(path = 'src/test/Hello.zero.md')
    test("source_mdfile", source.code, """
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
    lexer_result = """[on, (, out$, :, string, ), <<, hello, (, ), {, bingo, ;, out$, <<, "hello world", }, next]"""
    test("lexer_simple", lexer(Source(code = """
on (out$ : string) << hello()
    bingo
    out$ << "hello world"
next   
""")), lexer_result)
    test("lexer_py", lexer(Source(code = """
on (out$ : string) << hello():
    bingo
    out$ << "hello world"
next   
""")), lexer_result)
    test("lexer_ts", lexer(Source(code = """
on (out$ : string) << hello() {
    bingo;
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
        self.jump = 0           # increment to jump to next/prev matching bracket ("()", "[]", "indent/undent")
    def __str__(self):
        return self.val
    def __repr__(self):
        val = str(self)
        if self.type == "newline" : return log_grey(val)
        else: return val
    def dbg(self):
        return f"Lex <{self.type}> '{self.val}' @ {self.location()}"
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
            ols.append(Lex(lex.source, lex.pos, "{", "indent"))
            i += 1
            while i < len(ls) and ls[i].val.startswith("ws-") : i += 1
            i -= 1
        elif lex.val == "}":    # add an undent, remove all previous ws-undents
            while len(ols) > 0 and ols[-1].val.startswith("ws-"): ols.pop()
            ols.append(Lex(lex.source, lex.pos, "}", "undent"))
        elif lex.val == ";":
            ols.append(Lex(lex.source, lex.pos, ";", "newline"))
        else:
            ols.append(lex)
        i += 1
    return ols

# replace all 'ws-' tags with normal tags; add missing undents to the end
def finalise_indents(ls: List[Lex]) -> List[Lex]:
    for lex in ls:
        if lex.val.startswith("ws-"):
            lex.val = "{" if lex.val == "ws-indent" else "}" if lex.val == "ws-undent" else ";"
    indent_level = 0
    for lex in ls:
        if lex.val == "{": indent_level += 1
        elif lex.val == "}": indent_level -= 1
    if indent_level >0:
        lex = ls[-1]
        for i in range(indent_level):
            ls.append(Lex(lex.source, len(ls), "}", "undent"))
    return ls

# get rid of any newlines that sit next to indents, undents, or other newlines
def filter_newlines(ls: List[Lex]) -> List[Lex]:
    ols = []
    i = 0
    while i < len(ls):
        lex = ls[i]
        if lex.val == ";":
            preceding_is_ndent = (i > 0 and ls[i-1].val in ["{", "}", ";"])
            following_is_ndent = (i < (len(ls)-1) and ls[i+1].val in ["{", "}"])
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
    open = ["(", "[", "{"]
    close = [")", "]", "}"]
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
# Grammar
#--------------------------------------------------------------------------------------------------
# Term: a single term of a rule

class Term:
    def __init__(self, var: str, vals: List[str], dec:str ="", sep:str =""):
        self.var = var      # variable to assign in the AST, if any
        self.vals = vals    # a set of keywords ("word"), types (<type>), or rules (Rule)
        self.dec = dec      # one of "?" (zero or one), "*" (zero or more) or "+" (one or more)
        self.sep = sep      # separator if we are "+" or "*"
        self.initials = []  # keywords that can initiate this term
        self.followers = [] # keywords that can follow this term
        self.leaves = {}    # maps match => rule-names
        self.index = 0      # index in the original term list
        self.rule = None    # the rule we're in, if any
    def __str__(self):
        out = " | ".join(self.vals)
        if len(self.vals) > 1: out = f"({out})"
        if self.dec: out += self.dec
        if self.sep: out += self.sep
        if self.var: out = f"{self.var}:{out}"
        return out
    def __repr__(self): return self.__str__()
    def is_keyword(self): return self.vals[0][0] == '"'
    def is_type(self): return self.vals[0][0] == '<'
    def is_terminal(self): return (self.is_keyword() or self.is_type())
    def is_rule(self): return not (self.is_keyword() or self.is_type())
    def rules(self): return [s_grammar.rule_named[r] for r in self.vals]


#--------------------------------------------------------------------------------------------------
# Rule: a name and a list of terms

class Rule:
    def __init__(self, name: str):
        self.name = name
        self.terms = []
        self.initials = []      # keywords that can start this rule
        self.followers = []     # keywords that can follow this rule
        self.leaves = {}        # maps match => rule-names
    def __str__(self):
        return self.name
    def __repr__(self): return self.__str__()
    def dbg(self):
        out = self.name + " := " + " ".join([str(term) for term in self.terms])
        return out
    def is_abstract(self):
        return len(self.terms)==1 and self.terms[0].is_rule() and not self.terms[0].dec

#--------------------------------------------------------------------------------------------------
# Grammar: a list of rules

s_grammar = None

class Grammar:
    def __init__(self):
        self.rules = []
        self.rule_named = {}
        global s_grammar
        s_grammar = self
    def dbg(self):
        out = ""
        for rule in self.rules:
            out += rule.dbg() + "\n"
        return out

#--------------------------------------------------------------------------------------------------
# build a grammar from a spec

def build_grammar(spec: str) -> Grammar:
    grammar = Grammar()
    lines = [line.strip() for line in spec.split("\n")]
    lines = [line for line in lines if line != ""]
    for line in lines:
        name, terms_str = map(str.strip, line.split(':='))
        build_rule(name, split_terms(terms_str))
    compute_meta_stuff()
    return grammar

def build_rule(rule_name: str, term_strs: List[str], after_rule_name: str=None) -> Rule:
    new_rule = add_rule(rule_name, after_rule_name)
    for i_term, term_str in enumerate(term_strs):
        term = build_term(term_str, i_term, rule_name)
        new_rule.terms.append(term)
    return new_rule

#@log_indent
def add_rule(rule_name: str, after_rule_name: str=None) -> Rule:
    global s_grammar
    new_rule = Rule(rule_name)
    if after_rule_name:
        after = s_grammar.rule_named[after_rule_name]
        i = s_grammar.rules.index(after)
        s_grammar.rules.insert(i+1, new_rule)
    else:
        s_grammar.rules.append(new_rule)
    s_grammar.rule_named[rule_name] = new_rule
    return new_rule

#@log_indent
def build_term(term_str: str, i_term: int, rule_name: str) -> Term:
    parts = re.match(r"(\w+):(.+)", term_str)
    var = parts.group(1) if parts else None
    term_str = parts.group(2) if parts else term_str
    parts = re.match(r"(.+)([*+?])([,;|]?)", term_str)
    sep = parts.group(3) if parts else ""
    dec = parts.group(2) if parts else ""
    term_str = parts.group(1) if parts else term_str
    vals = []
    if term_str.startswith("(") and term_str.endswith(")"):
        term_str = term_str[1:-1]
        if " | " in term_str: 
            vals = [val.strip() for val in term_str.split("|")]
        else:
            sub_rule = build_rule(rule_name + "_" + str(i_term), split_terms(term_str), rule_name)
            vals = [ sub_rule.name ]
    else:
        vals = [term_str]
    return Term(var, vals, dec, sep)

def split_terms(rhs: str) -> str:
    terms = []
    current_term = ""
    bracket_depth = 0
    in_quotes = False
    for char in rhs:
        if char == ' ' and bracket_depth == 0 and not in_quotes:
            if current_term:
                terms.append(current_term)
                current_term = ""
        else:
            if char == '(' and not in_quotes:
                bracket_depth += 1
            elif char == ')' and not in_quotes:
                bracket_depth -= 1
            elif char == '"':
                in_quotes = not in_quotes
            
            current_term += char
    if current_term:
        terms.append(current_term)
    return terms

#--------------------------------------------------------------------------------------------------
# second-order grammar properties

# check that the following assumptions made by the parser are true:
# 1- all referred to rules actually exist
# 2- any unbound list terms are the only variable term in the rule
# 3- a term's values are all the same type ("keyword", <type>, or rule)
def get_errors(grammar: Grammar) -> str:
    out = ""
    for rule in grammar.rules:
        unbound_list_terms = []
        variable_terms = []
        for i_term, term in enumerate(rule.terms):
            # 1- check referred-to-rules exist
            for val in term.vals:
                if val[0] and not (val[0] in '"<'):
                    if not (val in s_grammar.rule_named):
                        out += f"term {i_term} of {rule.name}: can't find referred-to rule '{val}'\n"
            # 2- is the term unbound, and has '+' or '*'?
            if term.dec and (term.dec in "*+") and not term.var:
                unbound_list_terms.append(term)
            # 2.1- is the term a variable term? (i.e. contains identifier or rule)
            if len(term.vals) > 0 and term.vals[0][0] != '"':
                variable_terms.append(term)
            # 3- are all the term's values the same type?
            n_keywords = 0; n_types =0; n_rules =0
            for val in term.vals:
                if val[0] == '"': n_keywords += 1
                elif val[0] == "<": n_types += 1
                else: n_rules += 1
            n_vals = len(term.vals)
            ok = (n_keywords == n_vals) or (n_types == n_vals) or (n_rules == n_vals)
            if not ok:
                out += f"term {i_term} of {rule.name}: values are mixed types\n"
        if len(unbound_list_terms) > 0 and len(variable_terms) > 1:
            out += f"rule {rule.name} has unbound list terms: {str(unbound_list_terms).replace("[", "").replace("]", "")}\n"
    if out == "":
        out = "no errors"
    return out

# computes the initiators and followers for each rule and term
def compute_meta_stuff():
    done = False
    while not done: done = not (compute_initials())
    done = False
    while not done: done = not (compute_followers())
    done = False
    while not done: done = not (compute_leaves())
    compute_indices()

def compute_initials() -> bool:
    changed = False
    # all keyword terms get their initials = vals
    for rule in s_grammar.rules:
        for term in rule.terms:
            if term.is_keyword(): term.initials = term.vals
    # first find the first keyword in each rule
    for rule in s_grammar.rules:
        for term in rule.terms:
            if term.is_keyword():
                changed = merge_arrays(rule.initials, term.vals) or changed
            if not term.dec or term.dec == '+':
                break
    # now apply this transitively to the terms:
    # if a term is a bunch of rules, that term's initials come from those rules
    for rule in s_grammar.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    changed = merge_arrays(term.initials, sub_rule.initials) or changed
    # and finally skim the first term's initials into the parent rule
    for rule in s_grammar.rules:
        for term in rule.terms:
            changed = merge_arrays(rule.initials, term.initials) or changed
            if not term.dec or term.dec == '+':
                break
    return changed

s_trace = ""

def push_followers_downwards(term, followers, changed) -> bool:
    if not term.is_rule(): return changed
    for sub_rule in term.rules():
        vb = (sub_rule.name == s_trace)
        if vb: log(caller())
        if vb: log("trace:", sub_rule.name)
        if vb: log("before:", sub_rule.followers)
        filtered_followers = [f for f in followers if f not in sub_rule.initials]
        changed = merge_arrays(sub_rule.followers, filtered_followers) or changed
        if vb: log("after:", sub_rule.followers)
    return changed

def compute_followers():
    changed = False
    # set term and rule followers using initials of the next term
    check_term = s_grammar.rule_named["parameter_0"].terms[0]
    for rule in s_grammar.rules:
        for i_term, term in enumerate(rule.terms):
            if term.is_terminal() and term.dec == "": continue
            if (i_term + 1) < len(rule.terms):
                next_term = rule.terms[i_term+1]
                next_initials = next_term.initials
                changed = merge_arrays(term.followers, next_initials) or changed
                changed = push_followers_downwards(term, next_initials, changed)
                if next_term.dec and (i_term + 2) < len(rule.terms):
                    next_next_initials = rule.terms[i_term+2].initials
                    changed = merge_arrays(term.followers, next_next_initials) or changed
                    changed = push_followers_downwards(term, next_next_initials, changed)

    # for the last term in each rule, set its followers to the followers of its rule
    for rule in s_grammar.rules:
        last_term = rule.terms[-1]
        changed = merge_arrays(last_term.followers, rule.followers)
        if last_term.is_rule():
            changed = push_followers_downwards(last_term, rule.followers, changed)
        if last_term.dec and last_term.dec in '?*' and len(rule.terms)>1:
            second_last_term = rule.terms[-2]
            changed = merge_arrays(second_last_term.followers, rule.followers)
            if second_last_term.is_rule():
                changed = push_followers_downwards(second_last_term, rule.followers, changed)    
    return changed

# compute leaves: for each rule, find {terminal => [rule]}
def compute_leaves() -> bool:
    changed = False
    for rule in s_grammar.rules:
        term = rule.terms[0]
        if term.is_terminal():
            for val in term.vals:
                changed = merge_dicts(rule.leaves, { val : [rule] }) or changed
    # now transfer those to terms
    for rule in s_grammar.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    if sub_rule.is_abstract():
                        changed = merge_dicts(term.leaves, sub_rule.terms[0].leaves) or changed
                    else:
                        for val in sub_rule.leaves.keys():
                            changed = merge_dicts(term.leaves, { val : [sub_rule] }) or changed
    # and then back to the rules
    for rule in s_grammar.rules:
        term = rule.terms[0]
        if term.is_rule():
            changed = merge_dicts(rule.leaves, term.leaves) or changed
    return changed

# compute indices
def compute_indices():
    for rule in s_grammar.rules:
        for i_term, term in enumerate(rule.terms):
            term.rule = rule
            term.index = i_term

# merge two dicts (name => [vals]): return true if d1 changed
def merge_dicts(d1: Dict, d2: Dict) -> bool:
    changed = False
    for k, v in d2.items():
        if not k in d1:
            d1[k] = v
            changed = True
        else:
            changed = merge_arrays(d1[k], v) or changed
    return changed

# merge two arrays: return true if a1 changed
def merge_arrays(a1, a2)->bool:
    changed = False
    for v in a2:
        if not v in a1:
            a1.append(v)
            changed = True
    return changed

#--------------------------------------------------------------------------------------------------
# Zero grammar spec

s_zero_grammar_spec : str = """
    feature := "feature" name:<identifier> ("extends" parent:<identifier>)? "{" body:component*; "}"
    component := (test | type | variable | function)

    test := ">" lhs:expression ("=>" rhs:expression)?

    type := "type" name_decl (struct | type_relation | enum)
    name_decl := name:<identifier> ("|" alias:<identifier>)?
    struct := "=" "{" properties:variable* "}"
    type_relation := (child_type | parent_type)
    child_type := "<" parent:<identifier>
    parent_type := ">" children:<indentifier>*|
    enum := "=" values:<identifier>*|

    variable := name_type ("=" default:expression)?
    name_type := (c_name_type | ts_name_type)
    c_name_type := type:<identifier> names:name_decl+,
    ts_name_type := names:name_decl+, ":" type:<identifier>

    function := modifier:("on" | "replace" | "after" | "before") function_result? signature:signature "{" body:function_body "}"
    function_result := result:result_vars assign:("=" | "<<")
    result_vars := "(" name_type*, ")"
    signature := (word | operator | param_group)+
    word := <identifier>
    operator := <operator>
    param_group := "(" variable*, ")"
    function_body := statement*;
    statement := statement_lhs? rhs:expression
    statement_lhs := lhs:statement_dest assign:("=" | "<<")
    statement_dest := (name_type | variable_ref)
    variable_ref := <identifier>

    expression := (constant | operator | word | brackets)+
    constant := (<number> | <string>)
    brackets := "(" parameter*, ")"
    parameter := (names:<identifier>+, "=")? value:expression

    """

#--------------------------------------------------------------------------------------------------
@this_is_a_test
def test_grammar():
    log("test_grammar")
    grammar = build_grammar(s_zero_grammar_spec)
    test("test_grammar", grammar.dbg(), """
feature := "feature" name:<identifier> feature_2? "{" body:component*; "}"
feature_2 := "extends" parent:<identifier>
component := (test | type | variable | function)
test := ">" lhs:expression test_2?
test_2 := "=>" rhs:expression
type := "type" name_decl (struct | type_relation | enum)
name_decl := name:<identifier> name_decl_1?
name_decl_1 := "|" alias:<identifier>
struct := "=" "{" properties:variable* "}"
type_relation := (child_type | parent_type)
child_type := "<" parent:<identifier>
parent_type := ">" children:<indentifier>*|
enum := "=" values:<identifier>*|
variable := name_type variable_1?
variable_1 := "=" default:expression
name_type := (c_name_type | ts_name_type)
c_name_type := type:<identifier> names:name_decl+,
ts_name_type := names:name_decl+, ":" type:<identifier>
function := modifier:("on" | "replace" | "after" | "before") function_result? signature:signature "{" body:function_body "}"
function_result := result:result_vars assign:("=" | "<<")
result_vars := "(" name_type*, ")"
signature := (word | operator | param_group)+
word := <identifier>
operator := <operator>
param_group := "(" variable*, ")"
function_body := statement*;
statement := statement_lhs? rhs:expression
statement_lhs := lhs:statement_dest assign:("=" | "<<")
statement_dest := (name_type | variable_ref)
variable_ref := <identifier>
expression := (constant | operator | word | brackets)+
constant := (<number> | <string>)
brackets := "(" parameter*, ")"
parameter := parameter_0? value:expression
parameter_0 := names:<identifier>+, "="
         """)
    
#--------------------------------------------------------------------------------------------------
# Parser helpers

# format: prints an AST in a nice indented manner
def format(ast) -> str:
    json_string = json.dumps(ast, default=lambda x: x.dbg() if isinstance(x, Lex) else str(x), indent=4)
    return json_string

# readable error message
def error_message(term: Term, rule: Rule, ls: List[Lex], i_lex: int) -> str:
    location = ls[i_lex].location() if i_lex < len(ls) else "<eof>"
    term_str = f"{rule.name}"
    term_str += f".{term.var or "term" + str(term.index)}"
    return f"expected {cleanup(term.vals)} ({term_str}) at {location} (\"{' '.join([str(lex) for lex in ls[i_lex:i_lex+4]])}\")"

# cleans up a string - removes all quotes/braces
def cleanup(obj) -> str:
    s = str(obj)
    return s.replace("'", "").replace("[", "").replace("]", "").replace(",", " |")

# returns true if a lex matches a set of terminals
def lex_matches(lex: Lex, matches: List[str]) -> bool:
    if lex==None or len(matches)==0: return False
    if matches[0][0]=='"': return f'"{lex.val}"' in matches
    elif matches[0][0]=='<': return f'<{lex.type}>' in matches
    else: return False

# reduce a term to a list of rules
def reduce_rules(term: Term, lex: Lex) -> List[Rule]:
    rules = term.rules()
    if len(rules) == 1: return rules
    leaf_rules = []
    for key, rules in term.leaves.items():
        if lex_matches(lex, [key]):
            leaf_rules += rules
            if key.startswith('"'): break   # i.e. a keyword match prevents all others
    return leaf_rules

#--------------------------------------------------------------------------------------------------
# Reader: parser helper, feeds in lexemes one at a time, does matching and scanning

class Reader:
    def __init__(self, ls: List[Lex]):
        self.ls = ls
        self.pos = 0
    def __str__(self):
        return '"' + " ".join([str(lex) for lex in self.ls[self.pos:]]) + '"' if self.pos < len(self.ls) else "<eof>"
    def __repr__(self): return self.__str__()
    def copy(self): return Reader(self.ls, self.pos)
    def restore(self, pos):
        self.pos = pos
    def peek(self, end: int) -> Lex:
        return self.ls[self.pos] if self.pos < min(end, len(self.ls)) else None
    def next(self, end: int):
        result = self.peek(end)
        self.pos += 1
        return result
    def eof(self, end: int) -> bool: 
        return self.pos >= end
    def matches(self, term: Term, end: int) -> Lex:
        result = lex_matches(self.peek(end), term.vals)
        return result
    def scan(self, end: int, terminals: List[str]) -> int:
        if len(terminals)==0: return end
        i_lex = self.pos
        while i_lex < end:
            lex = self.ls[i_lex]
            if lex_matches(lex, terminals): return i_lex
            i_lex += self.ls[i_lex].jump + 1
        return i_lex

#--------------------------------------------------------------------------------------------------
# Parser helpers

# One Node per term in the grammar
class Node:
    def __init__(self, name: str, lex: Lex = None, children: List['Node'] = None, errors: List['Error'] = None):
        self.name = name
        self.lex = lex
        self.children = children or []
        self.errors = errors or []
    def __str__(self):
        out = ""
        out += f"{self.name}: "
        if self.lex: out += f"'{self.lex}'" 
        else: out += f"[{len(self.children)}]"
        if self.errors: out += f" !! {self.errors}"
        return out
    def __repr__(self): return self.__str__()
    def add(self, child: 'Node'):
        self.children.append(child)
    def in_error(self) -> bool:
        if len(self.errors) > 0: return True
        for child in self.children:
            if child.in_error(): return True
        return False
    
# an error is not a node; but it contains all properties needed to generate an error message
class Error:
    def __init__(self, term: Term, rule: Rule, ls: List[Lex], i_lex: int):
        self.term = term
        self.rule = rule
        self.ls = ls
        self.i_lex = i_lex
    def __str__(self):
        return "'" + error_message(self.term, self.rule, self.ls, self.i_lex) + "'"
    def __repr__(self): return self.__str__()
    
def readout(node: Node, indent=0) -> str:
    out = "  " * indent + str(node) + "\n"
    for child in node.children:
        out += readout(child, indent+1)
    return out

# convert a node tree into an AST dictionary
@log_indent
def get_ast(node: Node) -> Dict:
    if node.lex: return node.lex
    ast = {}
    errors = node.errors
    if node.name.startswith("_"):
        ast["_type"] = node.name[1:]
        rule = s_grammar.rule_named[ast["_type"]]
        for i_term, term in enumerate(rule.terms):
            if i_term >= len(node.children): break
            child_ast = get_ast(node.children[i_term])
            if term.var: ast[term.var] = child_ast
            else:
                if isinstance(child_ast, Lex): log("skipping lex", child_ast)
                elif isinstance(child_ast, Dict):
                    for k, v in child_ast.items():
                        if k != "_type" and k != "_errors": ast[k] = v
            if isinstance(child_ast, Dict) and "_errors" in child_ast:
                errors += child_ast["_errors"]
    if node.errors: ast["_errors"] = node.errors
    return ast


#--------------------------------------------------------------------------------------------------
# Parser

# generate one node per term and pack them into a rule node
@log_indent
def parse_rule(rule: Rule, reader: Reader, end: int) -> Node:
    node = Node(name = "_" + rule.name)
    n_errors = 0
    for term in rule.terms:
        if reader.eof(end): 
            node.errors.append(Error(term, rule, reader.ls, reader.pos))
            break
        pos = reader.pos
        child = parse_term(term, reader, end)
        node.add(child)
        if child.in_error():
            reader.restore(pos)
            n_errors += 1
            if term.dec == "" or n_errors > 1: break
        else:
            n_errors = 0
    return node

# generages a single term node; if it's a list node, it will contain zero or more children
@log_indent
def parse_term(term: Term, reader: Reader, end: int) -> Node:
    if term.dec == "": return parse_single_term(term, reader, end)
    min = 1 if term.dec == "+" else 0
    max = 1 if term.dec == "?" else None
    log(f"scanning for {term.followers}")
    end = reader.scan(end, term.followers)
    node = Node(name = term.var)
    while not reader.eof(end) and (max==None or len(node.children) < max):
        pos = reader.pos
        child = parse_single_term(term, reader, end)
        child.name = ""
        node.add(child)
        if child.in_error(): 
            reader.restore(pos)
            log(f"child has error; restoring ({reader})")
            break
    if len(node.children) < min:
        node.errors.append(error_message(term, term.rule, reader.ls, reader.pos))
    return node

# singular-term, either a terminal or a list of rules (ignores decorator)
@log_indent
def parse_single_term(term: Term, reader: Reader, end: int) -> Node:
    if term.is_terminal():
        if reader.matches(term, end):
            return Node(name = term.var, lex=reader.next(end))
        else:
            return parse_error(term, reader, end)
    rules = reduce_rules(term, reader.peek(end))
    child = None
    for rule in rules:
        pos = reader.pos
        child = parse_rule(rule, reader, end)
        if not child.in_error(): return child
        reader.restore(pos)
    return child or parse_error(term, reader, end)

# term is in error, so return a readable message for it
def parse_error(term: Term, reader: Reader, end: int) -> Node:
    return Node(term.var, errors=[Error(term, term.rule, reader.ls, reader.pos)])

# parses a separator if appropriate
def parse_separator(term: Term, reader: Reader, end: int):
    if term.sep:
        if reader.matches(f'"{term.sep}"'): reader.next(end)

#--------------------------------------------------------------------------------------------------
# Parser test

@log_indent
def parse(code: str, rule_name ="feature") -> Node:
    ls = lexer(Source(code=code))
    reader = Reader(ls)
    rule = s_grammar.rule_named[rule_name]
    node = parse_rule(rule, reader, len(ls))
    log("------------------------------------------------------------")
    log("readout:")
    log(readout(node))
    ast = get_ast(node)
    log("------------------------------------------------------------")
    log("AST:")
    log(format(ast))
    return ast

@this_is_the_test
def test_parser():
    log("test_parser")
    grammar = build_grammar(s_zero_grammar_spec)
    # simplest: check the 
    test("feature_0", parse(""), """{'_type': 'feature', '_errors': ['expected "feature" (feature.term0) at <eof> ("")']}""")
    test("feature_1", parse("feature"), """{'_type': 'feature', '_errors': ['expected <identifier> (feature.name) at <eof> ("")']}""")
    test("feature_2", parse("feature MyFeature"), """{'_type': 'feature', 'name': MyFeature, '_errors': ['expected feature_2 (feature.term2) at <eof> ("")']}""")
    test("feature_3", parse("feature MyFeature extrnds"), """{'_type': 'feature', 'name': MyFeature, '_errors': [expected 'extends' (feature_2.term0) at :...:19 ('extrnds'), expected '{' (feature.term3) at :...:19 ('extrnds')]}""")
    test("feature_4", parse("feature MyFeature extends"), """{'_type': 'feature', 'name': MyFeature, '_errors': ['expected <identifier> (feature_2.parent) at <eof> ("")', expected '{' (feature.term3) at :...:19 ('extends')]}""")
    test("feature_5", parse("feature MyFeature extends Another"), """{'_type': 'feature', 'name': MyFeature, 'parent': Another, '_errors': ['expected "{" (feature.term3) at <eof> ("")']}""")
    test("feature_6", parse("feature MyFeature {}"), """{'_type': 'feature', 'name': MyFeature, 'body': {'_list': []}}""")
    test("feature_7", parse("feature MyFeature extends Another {}"), """{'_type': 'feature', 'name': MyFeature, 'parent': Another, 'body': {'_list': []}}""")
    # ok cool first set done; now we look at the rest.
    # expressions; declarations; types; structures; and functions
    # then get on with semantic analysis and code generation
    test("expression_0", parse("1", "expression"), """{'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 1}]}""")
    test("expression_1", parse("a", "expression"), """{'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}]}""")
    test("expression_2", parse("a + b", "expression"), """{'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}, {'_type': 'operator', '_lex': +}, {'_type': 'word', '_lex': b}]}""")
    test("expression_3", parse("v = a + b", "parameter"))