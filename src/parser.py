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
        return out.replace('"', "'")
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
            d1[k] = v.copy()
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
    parent_type := ">" children:<identifier>*,
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
feature := 'feature' name:<identifier> feature_2? '{' body:component*; '}'
feature_2 := 'extends' parent:<identifier>
component := (test | type | variable | function)
test := '>' lhs:expression test_2?
test_2 := '=>' rhs:expression
type := 'type' name_decl (struct | type_relation | enum)
name_decl := name:<identifier> name_decl_1?
name_decl_1 := '|' alias:<identifier>
struct := '=' '{' properties:variable* '}'
type_relation := (child_type | parent_type)
child_type := '<' parent:<identifier>
parent_type := '>' children:<identifier>*,
enum := '=' values:<identifier>*|
variable := name_type variable_1?
variable_1 := '=' default:expression
name_type := (c_name_type | ts_name_type)
c_name_type := type:<identifier> names:name_decl+,
ts_name_type := names:name_decl+, ':' type:<identifier>
function := modifier:('on' | 'replace' | 'after' | 'before') function_result? signature:signature '{' body:function_body '}'
function_result := result:result_vars assign:('=' | '<<')
result_vars := '(' name_type*, ')'
signature := (word | operator | param_group)+
word := <identifier>
operator := <operator>
param_group := '(' variable*, ')'
function_body := statement*;
statement := statement_lhs? rhs:expression
statement_lhs := lhs:statement_dest assign:('=' | '<<')
statement_dest := (name_type | variable_ref)
variable_ref := <identifier>
expression := (constant | operator | word | brackets)+
constant := (<number> | <string>)
brackets := '(' parameter*, ')'
parameter := parameter_0? value:expression
parameter_0 := names:<identifier>+, '='
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
    def peek(self, end: int = None) -> Lex:
        end = end or len(self.ls)
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
    def location(self) -> str:
        return self.ls[self.pos].location() if self.pos < len(self.ls) else "<eof>"

#--------------------------------------------------------------------------------------------------
# Parser helpers: small functions that make the main three functions more readable

# if there are errors in child_ast, raise them up into ast; return True if so
def merge_errors(ast, child_ast) -> bool:
    if not isinstance(child_ast, dict): return False
    if not "_errors" in child_ast: return False
    if not "_errors" in ast: ast["_errors"] = []
    ast["_errors"] += child_ast["_errors"]
    return True

# given the term (which specifies a variable name, or doesn't), merge the child_ast into the ast
def merge_ast(term: Term, ast: Dict, child_ast: Dict) -> Dict:
    if term.var:
        if isinstance(child_ast, list):
            child_ast = { "_list" : child_ast }
        ast[term.var] = child_ast
        return ast
    merge_errors(ast, child_ast)
    if term.is_type(): ast["_lex"] =  child_ast
    if isinstance(child_ast, list):
        if term.dec == "?":
            if len(child_ast) == 0: return ast
            child_ast = child_ast[0]
        else:
            ast["_list"] = child_ast
            for item in child_ast: merge_errors(ast, item)
    if isinstance(child_ast, dict):
        for key, val in child_ast.items():
            if key not in ["_type", "_errors"]: 
                if not merge_errors(ast, val):
                        ast[key] = val
    return ast

# does (ast) contain errors?
def has_errors(ast: Dict|List) -> bool:
    if isinstance(ast, dict):
        if "_errors" in ast: return True
        for key, val in ast.items():
            if has_errors(val): return True
    elif isinstance(ast, list):
        for item in ast:
            if has_errors(item): return True
    return False

def should_stop(term: Term, ast: Dict, child_ast: Dict) -> bool:
    if has_errors(child_ast): return True
    if "_errors" in ast: return True
    return False

def should_stop_list(term: Term, ast: Dict) -> bool:
    return isinstance(ast, dict) and has_errors(ast)

def should_retry(term: Term, child_ast: Dict) -> bool:
    if term.dec == "?": return True
    return False

#--------------------------------------------------------------------------------------------------
# Parser

# top-level function, from code string to ast
@log_indent
def parse(code: str, rule_name: str = "feature") -> str:
    ls = lexer(Source(code = code))
    reader = Reader(ls)
    ast = parse_rule(s_grammar.rule_named[rule_name], reader, len(ls))
    return str(ast)

# parse a single rule; return a Dict
@log_indent
def parse_rule(rule: Rule, reader: Reader, end: int) -> Dict:

    # phase 1 : parse all the terms, stop if there's two errors in a row
    child_asts = []
    n_errors = 0
    for i_term, term in enumerate(rule.terms):
        pos = reader.pos
        #log_msg = f"term {i_term} ({str(term)}) : {reader} => "
        child_ast = parse_term(term, reader, end)
        #log(log_msg + str(child_ast))
        child_asts.append(child_ast)
        if has_errors(child_ast):
            #log("has_errors!")
            n_errors += 1
            if term.dec != "?" or n_errors > 1: 
                #log("break!")
                break
            else: 
                #log("restoring...")
                reader.restore(pos)
        else:
            n_errors = 0
            
    # phase 2: merge children into the top-level ast
    #log("--------------------------------")
    ast = { "_type": rule.name }
    for i_term, child_ast in enumerate(child_asts):
        term = rule.terms[i_term]
        #log(f"term {i_term} ({str(term)}) : {child_ast}")
        should_merge = True
        if term.dec == "?" and has_errors(child_ast):
            if i_term < len(child_asts)-1 and not has_errors(child_asts[i_term+1]):
                should_merge = False
        if should_merge:
            #log("merging...")
            merge_ast(term, ast, child_ast)
    
    return ast

#@log_indent
def parse_term(term: Term, reader: Reader, end: int) -> Dict|List|Lex:
    if term.dec == "": return parse_single_term(term, reader, end)
    min = 1 if term.dec == "+" else 0
    max = 1 if term.dec == "?" else None
    #log(f"scanning for {term.followers}")
    end = reader.scan(end, term.followers)
    items = []
    safe_count = 4
    while not reader.eof(end) and (max==None or len(items) < max):
        #log(f"---------- item {len(items)} ------------")
        item_ast = parse_single_term(term, reader, end)
        items.append(item_ast)
        if should_stop_list(term, item_ast):
            return item_ast["_errors"] if "_errors" in item_ast else items
        safe_count -= 1
        if safe_count == 0:
            raise Exception("safe count hit")
            break
        if not parse_separator(term, reader, end):
            return parse_error_separator(term, reader)
    if len(items) < min: return parse_error(term, reader)
    return items

#@log_indent
def parse_single_term(term: Term, reader: Reader, end: int) -> Dict|Lex:
    if term.is_terminal():
        if reader.matches(term, end): return reader.next(end)
        else: return parse_error(term, reader)
    if reader.eof(end): return parse_error(term, reader)
    rules = reduce_rules(term, reader.peek(end))
    errors = []
    for rule in rules:
        #log("trying rule " + rule.name + "...")
        pos = reader.pos
        child_ast = parse_rule(rule, reader, end)
        if not has_errors(child_ast): 
            #log("success!")
            return child_ast
        #log("failed!")
        reader.restore(pos)
        errors.append(child_ast["_errors"] if "_errors" in child_ast else child_ast)
    return { "_errors" : errors }

# return True if separator matched, False if error
#@log_indent
def parse_separator(term: Term, reader: Reader, end: int) -> bool:
    if term.sep == "": return True
    if reader.eof(end): return True # don't require a trailing separator
    if lex_matches(reader.peek(end), '"' + term.sep + '"'):
        reader.next(end)
        return True
    return False

def parse_error(term: Term, reader: Reader) -> Dict:
    return { "_errors" : [ { "_term" : term, "_loc" : reader.location() } ] }

@log_indent
def parse_error_separator(term: Term, reader: Reader) -> Dict:
    return { "_errors" : [ { "_term" : term, "_sep" : term.sep,"_loc" : reader.location() } ] }
#--------------------------------------------------------------------------------------------------

@this_is_the_test
def test_parser():
    log("test_parser")
    grammar = build_grammar(s_zero_grammar_spec)
    test("feature_0", parse(""), """{'_type': 'feature', '_errors': [{'_term': 'feature', '_loc': '<eof>'}]}""")
    test("feature_1", parse("feature"), """{'_type': 'feature', 'name': {'_errors': [{'_term': name:<identifier>, '_loc': '<eof>'}]}}""")
    test("feature_2", parse("feature MyFeature"), """{'_type': 'feature', 'name': MyFeature, '_errors': [{'_term': '{', '_loc': '<eof>'}]}""")
    test("feature_3", parse("feature MyFeature extends"), """{'_type': 'feature', 'name': MyFeature, '_errors': [{'_term': parent:<identifier>, '_loc': '<eof>'}, {'_term': '{', '_loc': ':...:19'}]}""")
    test("feature_4", parse("feature MyFeature extends Another"), """{'_type': 'feature', 'name': MyFeature, 'parent': Another, '_errors': [{'_term': '{', '_loc': '<eof>'}]}""")
    test("feature_5", parse("feature MyFeature {}"), """{'_type': 'feature', 'name': MyFeature, 'body': {'_list': []}}""")
    test("feature_6", parse("feature MyFeature extends Another {}"), """{'_type': 'feature', 'name': MyFeature, 'parent': Another, 'body': {'_list': []}}""")
    test("expression_0", parse("1", "expression"), """{'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 1}]}""")
    test("expression_1", parse("a", "expression"), """{'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}]}""")
    test("expression_2", parse("a + b", "expression"), """{'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}, {'_type': 'operator', '_lex': +}, {'_type': 'word', '_lex': b}]}""")
    test("parameter_0", parse("a = 1", "parameter"), """{'_type': 'parameter', 'names': {'_list': [a]}, 'value': {'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 1}]}}""")
    test("parameter_1", parse("a + b", "parameter"), """{'_type': 'parameter', 'value': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}, {'_type': 'operator', '_lex': +}, {'_type': 'word', '_lex': b}]}}""")
    test("brackets_0", parse("(a + b)", "brackets"), """{'_type': 'brackets', '_list': [{'_type': 'parameter', 'value': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}, {'_type': 'operator', '_lex': +}, {'_type': 'word', '_lex': b}]}}]}""")
    test("brackets_1", parse("(v = a + b)", "expression"), """{'_type': 'expression', '_list': [{'_type': 'brackets', '_list': [{'_type': 'parameter', 'names': {'_list': [v]}, 'value': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}, {'_type': 'operator', '_lex': +}, {'_type': 'word', '_lex': b}]}}]}]}""")
    test("expression_3", parse("a + (2 - c)", "expression"), """{'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}, {'_type': 'operator', '_lex': +}, {'_type': 'brackets', '_list': [{'_type': 'parameter', 'value': {'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 2}, {'_type': 'operator', '_lex': -}, {'_type': 'word', '_lex': c}]}}]}]}""")
    test("test_0", parse("> a", "test"), """{'_type': 'test', 'lhs': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}]}}""")
    test("test_1", parse("> a =>", "test"), """{'_type': 'test', 'lhs': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}]}, '_errors': [{'_term': rhs:expression, '_loc': '<eof>'}]}""")
    test("test_2", parse("> a => b", "test"), """{'_type': 'test', 'lhs': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': a}]}, 'rhs': {'_type': 'expression', '_list': [{'_type': 'word', '_lex': b}]}}""")
    test("variable_0", parse("int a", "name_type"), """{'_type': 'name_type', 'type': int, 'names': {'_list': [{'_type': 'name_decl', 'name': a}]}}""")
    test("variable_1", parse("a : int", "name_type"), """{'_type': 'name_type', 'names': {'_list': [{'_type': 'name_decl', 'name': a}]}, 'type': int}""")
    test("variable_2", parse("a : int = 0", "variable"), """{'_type': 'variable', 'names': {'_list': [{'_type': 'name_decl', 'name': a}]}, 'type': int, 'default': {'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 0}]}}""")
    test("variable_3", parse("int a = 0", "variable"), """{'_type': 'variable', 'type': int, 'names': {'_list': [{'_type': 'name_decl', 'name': a}]}, 'default': {'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 0}]}}""")
    test("type_0", parse("type vec", "type"), """{'_type': 'type', 'name': vec, '_errors': [{'_term': (struct | type_relation | enum), '_loc': '<eof>'}]}""")
    test("type_1", parse("type vec | vector = { x, y, z: number =0 }", "type"), """{'_type': 'type', 'name': vec, 'alias': vector, 'properties': {'_list': [{'_type': 'variable', 'names': {'_list': [{'_type': 'name_decl', 'name': x}, {'_type': 'name_decl', 'name': y}, {'_type': 'name_decl', 'name': z}]}, 'type': number, 'default': {'_type': 'expression', '_list': [{'_type': 'constant', '_lex': 0}]}}]}}""")
    test("type_2", parse("type int > i8, i16", "type"), """{'_type': 'type', 'name': int, 'children': {'_list': [i8, i16]}}""")
    test("type_3", parse("type distance < vector", "type"), """{'_type': 'type', 'name': distance, 'parent': vector}""")
    test("type_4", parse("type evil = no | yes | maybe", "type"), """{'_type': 'type', 'name': evil, 'values': {'_list': [no, yes, maybe]}}""")