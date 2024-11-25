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
# Zero grammar spec

s_zero_grammar_spec : str = """
    feature := "feature" name:<identifier> ("extends" parent:<identifier>)? "{" body:component*; "}"
    component := (test | type | variable | function)

    test := ">" lhs:expression ("=>" rhs:expression)?

    type := "type" name (struct | type_relation | enum)
    name := name:<identifier> ("|" alias:<identifier>)?
    struct := "=" "{" properties:variable* "}"
    type_relation := (child_type | parent_type)
    child_type := "<" parent:<identifier>
    parent_type := ">" children:<identifier>*,
    enum := "=" options:<identifier>*|

    variable := name_type ("=" default:expression)?
    name_type := (c_type_name | ts_name_type)
    c_type_name := type:<identifier> names:name+,
    ts_name_type := names:name+, ":" type:<identifier>?

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
    parameter := (name:<identifier> "=")? value:expression

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
type := 'type' name (struct | type_relation | enum)
name := name:<identifier> name_1?
name_1 := '|' alias:<identifier>
struct := '=' '{' properties:variable* '}'
type_relation := (child_type | parent_type)
child_type := '<' parent:<identifier>
parent_type := '>' children:<identifier>*,
enum := '=' options:<identifier>*|
variable := name_type variable_1?
variable_1 := '=' default:expression
name_type := (c_type_name | ts_name_type)
c_type_name := type:<identifier> names:name+,
ts_name_type := names:name+, ':' type:<identifier>?
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
parameter_0 := name:<identifier> '='
         """)
    
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
        self.contains_nested_sep = False     # True if this term points to sub-rules with the same separator
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
    def is_singular(self): return self.dec == ""
    def is_optional(self): return self.dec == "?"
    def is_list(self): return self.dec and self.dec in "*+"
    def rules(self): return [Grammar.current.rule_named[r] for r in self.vals]


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

class Grammar:
    current = None
    def __init__(self):
        self.rules = []
        self.rule_named = {}
        Grammar.current = self
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
    err = get_errors(grammar)
    if err != "":
        log("grammar errors found:")
        log(err)
        #log_exit()
    return grammar

def build_rule(rule_name: str, term_strs: List[str], after_rule_name: str=None) -> Rule:
    new_rule = add_rule(rule_name, after_rule_name)
    for i_term, term_str in enumerate(term_strs):
        term = build_term(term_str, i_term, rule_name)
        new_rule.terms.append(term)
    return new_rule

#@log_indent
def add_rule(rule_name: str, after_rule_name: str=None) -> Rule:
    new_rule = Rule(rule_name)
    if after_rule_name:
        after = Grammar.current.rule_named[after_rule_name]
        i = Grammar.current.rules.index(after)
        Grammar.current.rules.insert(i+1, new_rule)
    else:
        Grammar.current.rules.append(new_rule)
    Grammar.current.rule_named[rule_name] = new_rule
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
                    if not (val in Grammar.current.rule_named):
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
    return out

# computes all nested-separators
def compute_nested_separators():
    for rule in Grammar.current.rules:
        for term in rule.terms:
            found_terms =contains_nested_separator(term)
            if len(found_terms) > 0:
                #log(f"rule {rule.name}: term {term.index} has nested separator {term.sep}")
                term.contains_nested_sep = True

# checks a term to see if it has one or more nested separators
def contains_nested_separator(term: Term) -> List[Term]:
    if term.sep == "": return []
    visited = {}   # map Rule.name => bool
    if not term.is_rule(): return []
    # returns True if the rule, or any reachable sub-rule, contains the separator outside of braces ("{}()")
    def check_nested_separator(rule: Rule, visited: Dict[Rule, bool], sep: str) -> List[Term]:
        if rule.name in visited: return visited[rule.name]
        visited[rule.name] = False
        in_braces = False
        found_terms = []
        for term in rule.terms:
            if not in_braces:
                if term.sep == sep:
                    found_terms.append(term)
                if term.is_rule():
                    for sub_rule in term.rules():
                        new_terms= check_nested_separator(sub_rule, visited, sep)
                        found_terms += new_terms
            if term.is_keyword() and term.is_singular():
                val = term.vals[0][1:-1]
                if val in "{(" : in_braces = True
                elif val in "})": in_braces = False

        visited[rule.name] = found_terms
        return found_terms

    all_sub_terms_found = []
    for sub_rule in term.rules():
        if sub_rule.name in visited: continue
        sub_terms_found = check_nested_separator(sub_rule, visited, term.sep)
        all_sub_terms_found += sub_terms_found
    return all_sub_terms_found
    

# computes the initiators and followers for each rule and term
def compute_meta_stuff():
    done = False
    while not done: done = not (compute_initials())
    done = False
    while not done: done = not (compute_followers())
    finish_compute_followers()
    done = False
    while not done: done = not (compute_leaves())
    compute_indices()
    compute_nested_separators()

def compute_initials() -> bool:
    changed = False
    # all keyword terms get their initials = vals
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_keyword(): term.initials = term.vals
    # first find the first keyword in each rule
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_keyword():
                changed = merge_arrays(rule.initials, term.vals) or changed
            if not term.dec or term.dec == '+':
                break
    # now apply this transitively to the terms:
    # if a term is a bunch of rules, that term's initials come from those rules
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    changed = merge_arrays(term.initials, sub_rule.initials) or changed
    # and finally skim the first term's initials into the parent rule
    for rule in Grammar.current.rules:
        for term in rule.terms:
            changed = merge_arrays(rule.initials, term.initials) or changed
            if not term.dec or term.dec == '+':
                break
    return changed

s_trace = ""

def push_followers_downwards(term, followers, changed) -> bool:
    if not term.is_rule(): return changed
    for sub_rule in term.rules():
        filtered_followers = [f for f in followers if f not in sub_rule.initials]
        changed = merge_arrays(sub_rule.followers, filtered_followers) or changed
    return changed

def compute_followers():
    changed = False
    # set term and rule followers using initials of the next term
    check_term = None
    for rule in Grammar.current.rules:
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
    for rule in Grammar.current.rules:
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

def finish_compute_followers():
    for rule in Grammar.current.rules:
        for term in rule.terms:
            term.followers = [f for f in term.followers if f not in term.initials]

# compute leaves: for each rule, find {terminal => [rule]}
def compute_leaves() -> bool:
    changed = False
    for rule in Grammar.current.rules:
        term = rule.terms[0]
        if term.is_terminal():
            for val in term.vals:
                changed = merge_dicts(rule.leaves, { val : [rule] }) or changed
    # now transfer those to terms
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    if sub_rule.is_abstract():
                        changed = merge_dicts(term.leaves, sub_rule.terms[0].leaves) or changed
                    else:
                        for val in sub_rule.leaves.keys():
                            changed = merge_dicts(term.leaves, { val : [sub_rule] }) or changed             
    # and then back to the rules
    for rule in Grammar.current.rules:
        term = rule.terms[0]
        if term.is_rule():
            changed = merge_dicts(rule.leaves, term.leaves) or changed
    return changed

# compute indices
def compute_indices():
    for rule in Grammar.current.rules:
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
# Parser test

@this_is_a_test
def test_parser():
    log("test_parser")
    grammar = build_grammar(s_zero_grammar_spec)
    log_max_depth(14)
    test_verbose(False)
    test("feature_0", parse_code(""), """{'_rule': 'feature', '_errors': [{'_err': 'mismatch', '_expected': "'feature'", '_got': None, '_at': '<eof>'}]}""")
    test("feature_1", parse_code("feature"), """{'_rule': 'feature', 'name': {'_errors': [{'_err': 'mismatch', '_expected': 'name:<identifier>', '_got': None, '_at': '<eof>'}]}}""")
    test("feature_2", parse_code("feature MyFeature"), """{'_rule': 'feature', 'name': MyFeature, '_errors': [{'_err': 'mismatch', '_expected': "'{'", '_got': None, '_at': '<eof>'}]}""")
    test("feature_3", parse_code("feature MyFeature extends"), """{'_rule': 'feature', 'name': MyFeature, 'parent': {'_errors': [{'_err': 'mismatch', '_expected': 'parent:<identifier>', '_got': None, '_at': '<eof>'}]}, '_errors': [{'_err': 'mismatch', '_expected': "'{'", '_got': extends, '_at': ':1:19'}]}""")
    test("feature_4", parse_code("feature MyFeature extends Another"), """{'_rule': 'feature', 'name': MyFeature, 'parent': Another, '_errors': [{'_err': 'mismatch', '_expected': "'{'", '_got': None, '_at': '<eof>'}]}""")
    test("feature_5", parse_code("feature MyFeature {}"), """{'_rule': 'feature', 'name': MyFeature, 'body': {'_list': []}}""")
    test("feature_6", parse_code("feature MyFeature extends Another {}"), """{'_rule': 'feature', 'name': MyFeature, 'parent': Another, 'body': {'_list': []}}""")
    test("expression_0", parse_code("1", "expression"), """{'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 1}]}""")
    test("expression_1", parse_code("a", "expression"), """{'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}]}""")
    test("expression_2", parse_code("a + b", "expression"), """{'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}, {'_rule': 'operator', '_val': +}, {'_rule': 'word', '_val': b}]}""")
    test("parameter_0", parse_code("a = 1", "parameter"), """{'_rule': 'parameter', 'name': a, 'value': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 1}]}}""")
    test("parameter_1", parse_code("a + b", "parameter"), """{'_rule': 'parameter', 'value': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}, {'_rule': 'operator', '_val': +}, {'_rule': 'word', '_val': b}]}}""")
    test("brackets_0", parse_code("(a + b)", "brackets"), """{'_rule': 'brackets', '_list': [{'_rule': 'parameter', 'value': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}, {'_rule': 'operator', '_val': +}, {'_rule': 'word', '_val': b}]}}]}""")
    test("brackets_1", parse_code("(v = a + b)", "expression"), """{'_rule': 'expression', '_list': [{'_rule': 'brackets', '_list': [{'_rule': 'parameter', 'name': v, 'value': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}, {'_rule': 'operator', '_val': +}, {'_rule': 'word', '_val': b}]}}]}]}""")
    test("expression_3", parse_code("a + (2 - c)", "expression"), """{'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}, {'_rule': 'operator', '_val': +}, {'_rule': 'brackets', '_list': [{'_rule': 'parameter', 'value': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 2}, {'_rule': 'operator', '_val': -}, {'_rule': 'word', '_val': c}]}}]}]}""")
    test("test_0", parse_code("> a", "test"), """{'_rule': 'test', 'lhs': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}]}}""")
    test("test_1", parse_code("> a =>", "test"), """{'_rule': 'test', 'lhs': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}]}, 'rhs': {'_errors': [{'_err': 'premature end', '_expected': 'rhs:expression', '_got': None, '_at': '<eof>'}]}}""")
    test("test_2", parse_code("> a => b", "test"), """{'_rule': 'test', 'lhs': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}]}, 'rhs': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': b}]}}""")
    test("variable_0", parse_code("int a", "name_type"), """{'_rule': 'c_type_name', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': a}]}}""")
    test("variable_1", parse_code("a : int", "name_type"), """{'_rule': 'ts_name_type', 'names': {'_list': [{'_rule': 'name', 'name': a}]}, 'type': int}""")
    test("variable_2", parse_code("a : int = 0", "variable"), """{'_rule': 'variable', 'names': {'_list': [{'_rule': 'name', 'name': a}]}, 'type': int, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}""")
    test("variable_3", parse_code("int a = 0", "variable"), """{'_rule': 'variable', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': a}]}, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}""")
    test("variable_4", parse_code("r|red, g|green, b|blue: number = 0", "variable"), """{'_rule': 'variable', 'names': {'_list': [{'_rule': 'name', 'name': r, 'alias': red}, {'_rule': 'name', 'name': g, 'alias': green}, {'_rule': 'name', 'name': b, 'alias': blue}]}, 'type': number, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}""")
    test("variable_5", parse_code("int x,y = 0", "variable"), """{'_rule': 'variable', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': x}, {'_rule': 'name', 'name': y}]}, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}""")
    test("result_vars_1", parse_code("(a, b: int, k, l: float)", "result_vars"), """{'_rule': 'result_vars', '_list': [{'_rule': 'ts_name_type', 'names': {'_list': [{'_rule': 'name', 'name': a}, {'_rule': 'name', 'name': b}]}, 'type': int}, {'_rule': 'ts_name_type', 'names': {'_list': [{'_rule': 'name', 'name': k}, {'_rule': 'name', 'name': l}]}, 'type': float}]}""")
    test("type_0", parse_code("type vec", "type"), """{'_rule': 'type', 'name': vec, '_errors': [{'_err': 'premature end', '_expected': '(struct | type_relation | enum)', '_got': None, '_at': '<eof>'}]}""")
    test("type_1", parse_code("type vec | vector = { x, y, z: number =0 }", "type"), """{'_rule': 'type', 'name': vec, 'alias': vector, 'properties': {'_list': [{'_rule': 'variable', 'names': {'_list': [{'_rule': 'name', 'name': x}, {'_rule': 'name', 'name': y}, {'_rule': 'name', 'name': z}]}, 'type': number, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}]}}""")
    test("type_2", parse_code("type int > i8, i16", "type"), """{'_rule': 'type', 'name': int, 'children': {'_list': [i8, i16]}}""")
    test("type_3", parse_code("type distance < vector", "type"), """{'_rule': 'type', 'name': distance, 'parent': vector}""")
    test("type_4", parse_code("type evil = no | yes | maybe", "type"), """{'_rule': 'type', 'name': evil, 'options': {'_list': [no, yes, maybe]}}""")
    test("result_vars_2", parse_code("(int a, b, float k)", "result_vars"), """{'_rule': 'result_vars', '_list': [{'_rule': 'c_type_name', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': a}, {'_rule': 'name', 'name': b}]}}, {'_rule': 'c_type_name', 'type': float, 'names': {'_list': [{'_rule': 'name', 'name': k}]}}]}""")
    test("param_group_0", parse_code("(int a, b=0, float k=0)", "param_group"), """{'_rule': 'param_group', '_list': [{'_rule': 'variable', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': a}, {'_rule': 'name', 'name': b}]}, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}, {'_rule': 'variable', 'type': float, 'names': {'_list': [{'_rule': 'name', 'name': k}]}, 'default': {'_rule': 'expression', '_list': [{'_rule': 'constant', '_val': 0}]}}]}""")
    test("param_group_0", parse_code("(int a, b, float k)", "param_group"), """{'_rule': 'param_group', '_list': [{'_rule': 'variable', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': a}, {'_rule': 'name', 'name': b}]}}, {'_rule': 'variable', 'type': float, 'names': {'_list': [{'_rule': 'name', 'name': k}]}}]}""")
    test("param_group_1", parse_code("(a, b: int, k: float)", "param_group"), """{'_rule': 'param_group', '_list': [{'_rule': 'variable', 'names': {'_list': [{'_rule': 'name', 'name': a}, {'_rule': 'name', 'name': b}]}, 'type': int}, {'_rule': 'variable', 'names': {'_list': [{'_rule': 'name', 'name': k}]}, 'type': float}]}""")
    test("function_0", parse_code("on (int r) = min (int a, b) { r = if (a < b) then a else b }", "function"), """{'_rule': 'function', 'modifier': on, 'result': {'_rule': 'result_vars', '_list': [{'_rule': 'c_type_name', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': r}]}}]}, 'assign': =, 'signature': {'_rule': 'signature', '_list': [{'_rule': 'word', '_val': min}, {'_rule': 'param_group', '_list': [{'_rule': 'variable', 'type': int, 'names': {'_list': [{'_rule': 'name', 'name': a}, {'_rule': 'name', 'name': b}]}}]}]}, 'body': {'_rule': 'function_body', '_list': [{'_rule': 'statement', 'lhs': {'_rule': 'c_type_name', 'type': r, 'names': {'_list': []}}, 'assign': =, 'rhs': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': if}, {'_rule': 'brackets', '_list': [{'_rule': 'parameter', 'value': {'_rule': 'expression', '_list': [{'_rule': 'word', '_val': a}, {'_rule': 'operator', '_val': <}, {'_rule': 'word', '_val': b}]}}]}, {'_rule': 'word', '_val': then}, {'_rule': 'word', '_val': a}, {'_rule': 'word', '_val': else}, {'_rule': 'word', '_val': b}]}}]}}""")
    test("feature_7", parse_code("feature Hello { type str | string = { char c$ }; string out$ | output$; }", "feature"), """{'_rule': 'feature', 'name': Hello, 'body': {'_list': [{'_rule': 'type', 'name': str, 'alias': string, 'properties': {'_list': [{'_rule': 'variable', 'type': char, 'names': {'_list': [{'_rule': 'name', 'name': c$}]}}]}}, {'_rule': 'variable', 'type': string, 'names': {'_list': [{'_rule': 'name', 'name': out$, 'alias': output$}]}}]}}""")

#--------------------------------------------------------------------------------------------------
# general parser helpers

def parse_code(code: str, rule_name: str = "feature") -> str:
    ls = lexer(Source(code=code))
    reader = Reader(ls=ls)
    rule = Grammar.current.rule_named[rule_name]
    ast = parse_rule(rule, reader)
    log("----------------------------------------")
    log(format(ast))
    return ast

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

# recursively check to see if an ast contains "_errors"
def has_errors(ast: Dict|List|Lex) -> bool:
    if isinstance(ast, Lex): return False
    elif isinstance(ast, List):
        for item in ast:
            if has_errors(item): return True
    elif isinstance(ast, Dict):
        if "_errors" in ast: return True
        for key, value in ast.items():
            if has_errors(value): return True
    return False

# get a list of all errors in an ast
def get_ast_errors(ast: Dict|List|Lex) -> List[Dict]:
    if isinstance(ast, Lex): return []
    elif isinstance(ast, List):
        errors = []
        for item in ast:
            errors += get_ast_errors(item)
        return errors
    elif isinstance(ast, Dict):
        if "_errors" in ast: return ast["_errors"]
        errors = []
        for key, value in ast.items():
            errors += get_ast_errors(value)
        return errors
    return []

# merge a child ast into a parent ast
def merge_ast(term: Term, ast: Dict, child_ast: Dict|Lex|List) -> Dict:
    if term.var:
        ast[term.var] = child_ast
        return ast
    if term.is_type(): ast["_val"] = child_ast
    if isinstance(child_ast, list):
        if term.dec == "?":
            if len(child_ast) == 0: return ast
            child_ast = child_ast[0]
        else:
            ast["_list"] = child_ast
    if isinstance(child_ast, dict):
        if "_rule" in child_ast and "_rule" in ast:
            parent_rule = Grammar.current.rule_named[ast["_rule"]]
            if parent_rule.is_abstract(): # do a type replacement
                ast["_rule"] = child_ast["_rule"]
        for key, val in child_ast.items():
            if key != "_rule": ast[key] = val
    return ast

# remove all errors from an ast
def remove_errors(ast: Dict):
    if isinstance(ast, dict) and "_errors" in ast:
        del ast["_errors"]
    elif isinstance(ast, list):
        for item in ast:
            remove_errors(item)
#--------------------------------------------------------------------------------------------------
# Reader: reads source lexemes one at a time, with truncation

class LsReader:
    def __init__(self, ls: List[Lex]):
        self.ls = ls
        self.pos = 0

class Reader:
    def __init__(self, ls: List[Lex] = None, lsReader: LsReader = None, end: int = None, nested_sep: str = None):
        self.lsReader = lsReader or LsReader(ls)
        self.end = end or len(self.lsReader.ls)
        self.nested_sep = nested_sep
    def __str__(self):
        return '"' + " ".join([str(lex) for lex in self.lsReader.ls[self.pos():self.end]]) + '"' if self.pos() < len(self.lsReader.ls) else "<eof>"
    def __repr__(self): return self.__str__()
    def pos(self): return self.lsReader.pos
    def restore(self, pos): self.lsReader.pos = pos
    def peek(self): return self.lsReader.ls[self.lsReader.pos] if self.lsReader.pos < min(self.end, len(self.lsReader.ls)) else None
    def peek_unrestricted(self): return self.lsReader.ls[self.lsReader.pos] if self.lsReader.pos < len(self.lsReader.ls) else None
    def next(self): result = self.peek(); self.lsReader.pos += 1; return result
    def eof(self): return self.lsReader.pos >= self.end
    def matches(self, term: Term): return lex_matches(self.peek(), term.vals)
    def scan(self, terminals: List[str]) -> 'Reader':
        if len(terminals)==0: return Reader(lsReader=self.lsReader, end=self.end, nested_sep=self.nested_sep)
        i_lex = self.lsReader.pos
        while i_lex < self.end:
            lex = self.lsReader.ls[i_lex]
            if lex_matches(lex, terminals): break
            i_lex += self.lsReader.ls[i_lex].jump + 1
        return Reader(lsReader=self.lsReader, end=i_lex, nested_sep=self.nested_sep)
    def location(self) -> str:
        return self.lsReader.ls[self.lsReader.pos].location() if self.pos() < len(self.lsReader.ls) else "<eof>"
    def set_nested_separator(self, nested_sep: str): self.nested_sep = nested_sep

#--------------------------------------------------------------------------------------------------
# Parser

# parse a rule and return an ast
#@log_indent
def parse_rule(rule: Rule, reader: Reader) -> Dict:
    children = parse_terms(rule, reader)
    handle_optional_errors(rule, children)
    ast = merge_children(rule, children)
    check_premature_end(rule, ast, children, reader)
    return ast

# dispatch term parser to the correct case (take term.dec into account)
def parse_term(term: Term, reader: Reader) -> Dict:
    if term.is_singular(): return parse_singular_term(term, reader)
    elif term.is_optional(): return parse_optional(term, reader.scan(term.followers))
    else: return parse_list_term(term, reader.scan(term.followers))

# dispatch singular term parser to the correct case (ignore term.dec)
def parse_singular_term(term: Term, reader: Reader) -> Dict:
    if term.is_terminal(): return parse_terminal(term, reader)
    elif term.is_rule(): return parse_rule_term(term, reader.scan(term.followers))
    else: raise Exception(f"unknown term type: {term.vals}")

#@log_indent
def parse_list_term(term: Term, reader: Reader) -> Dict:
    items = []
    pos = reader.pos()
    before_sep_pos = pos
    while not reader.eof():
        term_reader = scan_separator(reader, term)
        child = parse_singular_term(term, term_reader)
        items.append(child)
        if has_errors(child):
            log(log_red("last item has errors!"))
            items = truncate_list(term, reader, items, pos)
            log(f"reader after restore: {reader}")
            break
        if reader.eof(): break
        pos = reader.pos()
        items, cont = parse_separator(term, reader, items, before_sep_pos)
        if not cont: break
        before_sep_pos = pos
    ast = { "_list" : items }
    return ast

#@log_indent
def parse_optional(term: Term, reader: Reader) -> Dict:
    if reader.eof(): return {}
    pos = reader.pos()
    child = parse_singular_term(term, reader)
    if has_errors(child): reader.restore(pos)
    return child

#@log_indent
def parse_rule_term(term: Term, reader: Reader) -> Dict:
    if reader.eof(): return parse_error("premature end", term, reader)
    rules = reduce_rules(term, reader.peek())
    log("rules:", rules)
    if len(rules) == 0: return parse_error("no matched rules", term, reader)
    ast = {}
    for rule in rules:
        pos = reader.pos()
        ast = parse_rule(rule, reader)
        if not has_errors(ast): return ast
        reader.restore(pos)
    return ast

# parse a terminal singular term
#@log_indent
def parse_terminal(term: Term, reader: Reader) -> Dict:
    return reader.next() if reader.matches(term) else parse_error("mismatch",term, reader)

#--------------------------------------------------------------------------------------------------
# these are parts of the main functions

# parse each term in turn, return array of children
def parse_terms(rule: Rule, reader: Reader) -> List[Dict]:
    children = []
    for term in rule.terms:
        child = parse_term(term, reader)
        children.append(child)
        if has_errors(child) and not term.is_optional(): break
    return children

# if an optional term has errors but is followed by a successfully parsed term, remove the errors
def handle_optional_errors(rule: Rule, children: List[Dict]):
    for i_term, term in enumerate(rule.terms):
        if i_term >= len(children): break
        if not term.is_optional(): continue
        child = children[i_term]
        if not has_errors(child): continue
        if (i_term+1) < len(children):
            if not has_errors(children[i_term+1]):
                log(log_red("dropping errors from optional term"))
                children[i_term] = {}
        else:
            log(log_red("optional term at end of rule has errors!"))
            log(child)
            if "_errors" in child:
                count = sum(1 for key in child if key not in ["_rule", "_errors"])
                log(log_red("dropping errors from optional term"))
                if count == 0: children[i_term] = {}
                
# merge children into a single ast
def merge_children(rule: Rule, children: List[Dict]) -> Dict:
    # merge children into ast
    ast = { "_rule" : rule.name }
    for i_term, term in enumerate(rule.terms):
        if i_term < len(children): 
            ast = merge_ast(term, ast, children[i_term])
    return ast

# if we didn't match all terms, add error
def check_premature_end(rule: Rule, ast: Dict, children: List[Dict], reader: Reader):
    if len(children) < len(rule.terms):
        log(log_red("premature end!"))
        if not has_errors(ast):
            err = parse_error("premature end", rule.terms[len(children)-1], reader)
            ast["_errors"] = [err]

def parse_error(message: str, term: Term, reader: Reader) -> Dict:
    err = { "_err" : message, "_expected" : str(term), "_got" : reader.peek_unrestricted(), "_at" : reader.location()}
    log(log_red(f"parse error: {err}"))
    return { "_errors" : [err]}

def scan_separator(reader: Reader, term: Term) -> Reader:
    term_reader = reader.scan([f'"{term.sep}"']) if (term.sep and term.contains_nested_sep == False) else reader
    if term.contains_nested_sep: term_reader.set_nested_separator(term.sep)
    return term_reader

#@log_indent
def truncate_list(term: Term, reader: Reader, items: List[Dict], pos: int) -> List[Dict]:
    if reader.nested_sep == None: return items
    if term.dec == "+" and len(items) == 1: return items
    log(log_red("truncating list!"))
    items = items[:-1]
    reader.restore(pos)
    log(f"reader after restore: {reader}")
    return items

#@log_indent
def parse_separator(term: Term, reader: Reader, items: List[Dict], restore_pos: int) -> Tuple[List[Dict], bool]:
    if term.sep == "": return items, True
    if lex_matches(reader.peek(), [f'"{term.sep}"']):
        reader.next()
        return items, True
    if term.sep == ";": return items, True
    log(log_red(f"separator error: expected {term.sep} but got {reader.peek()}"))
    items = truncate_list(term, reader, items, restore_pos)
    return items,False
