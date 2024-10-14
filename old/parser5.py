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
    def is_terminal(self): return self.is_keyword() or self.is_type()
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
    check_grammar()
    return grammar

# computes the initiators and followers for each rule and term

def compute_meta_stuff():
    done = False
    while not done: done = not (compute_initials())
    done = False
    while not done: done = not (compute_followers())
    done = False
    while not done: done = not (compute_leaves())

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
    for sub_rule in term.rules():
        vb = (sub_rule.name == s_trace)
        if vb: log(caller())
        if vb: log("trace:", sub_rule.name)
        if vb: log("before:", sub_rule.followers)
        changed = merge_arrays(sub_rule.followers, followers) or changed
        if vb: log("after:", sub_rule.followers)
    return changed

def compute_followers():
    changed = False
    # set term and rule followers using initials of the next term
    for rule in s_grammar.rules:
        for i_term, term in enumerate(rule.terms):
            if term.is_terminal(): continue
            #if term.dec and term.dec in '*+' and term.is_rule():
            #    sub_followers = []
            #    if term.sep: 
            #        merge_arrays(sub_followers, [f'"{term.sep}"'])
            #    else:
            #        merge_arrays(sub_followers, term.initials)
            #    changed = push_followers_downwards(term, sub_followers, changed)
            if (i_term + 1) < len(rule.terms):
                next_term = rule.terms[i_term+1]
                next_initials = rule.terms[i_term+1].initials
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
        for term in rule.terms:
            if term.is_terminal():
                for val in term.vals:
                    changed = merge_dicts(rule.leaves, { val : [rule] }) or changed
            if not term.dec or term.dec == '+':
                break
    # now transfer those to terms
    for rule in s_grammar.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    for val in sub_rule.leaves.keys():
                        changed = merge_dicts(term.leaves, { val : [sub_rule] }) or changed
    # and then back to the rules
    for rule in s_grammar.rules:
        for term in rule.terms:
            if term.is_rule():
                changed = merge_dicts(rule.leaves, term.leaves) or changed
            if not term.dec or term.dec == '+':
                break
    return changed

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

# checks the grammar to see if there's any assumptions we made in the parser that aren't actually true of the grammar
def check_grammar():
    bad = False
    for rule in s_grammar.rules:
        last_i_term = 0
        for i_term, term in enumerate(rule.terms):
            if term.is_terminal():
                #log("i_term:", i_term, term)
                n_terms_since_last = i_term - last_i_term
                last_i_term = i_term
                if n_terms_since_last > 2:
                    log("warning", f"rule {rule.name} has >2 nonterminals in a row at {i_term}")
                    log("  rule:", rule.dbg())
                    bad = True
    if bad:
        log("grammar check failed: exiting.")
        log_exit()

def build_rule(rule_name: str, term_strs: List[str], after_rule_name: str=None) -> Rule:
    new_rule = add_rule(rule_name, after_rule_name)
    for i_term, term_str in enumerate(term_strs):
        term = build_term(term_str, i_term, rule_name)
        new_rule.terms.append(term)
    return new_rule

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

def build_term(term_str: str, i_term: int, rule_name: str) -> Term:
    parts = re.match(r"(\w+):(.+)", term_str)
    var = parts.group(1) if parts else None
    term_str = parts.group(2) if parts else term_str
    parts = re.match(r"(.+)([*+?])([,|]?)", term_str)
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


#--------------------------------------------------------------------------------------------------
# Zero grammar spec

s_zero_grammar_spec : str = """
    feature_decl := "feature" name:<identifier> ("extends" parent:<identifier>)? ":indent" body:component_decl* ":undent"
    component_decl := (test_decl | type_decl | variable_decl | function_decl)

    test_decl := ">" lhs:expression ("=>" rhs:expression)?

    type_decl := "type" name_decl (struct_decl | type_relation_decl | enum_decl)
    name_decl := name:<identifier> ("|" alias:<identifier>)?
    struct_decl := "=" ":indent" properties:variable_decl* ":undent"
    type_relation_decl := (child_type_decl | parent_type_decl)
    child_type_decl := "<" parent:<identifier>
    parent_type_decl := ">" children:<indentifier>*|
    enum_decl := "=" values:<identifier>*|

    variable_decl := name_type_decl ("=" default:expression)?
    name_type_decl := (c_name_type_decl | ts_name_type_decl)
    c_name_type_decl := type:<identifier> names:name_decl+,
    ts_name_type_decl := names:name_decl+, ":" type:<identifier>

    function_decl := modifier:("on" | "replace" | "after" | "before") result:result_type_decl assign:("=" | "<<") signature:function_signature_decl ":indent" function_body ":undent"
    result_type_decl := "(" name_type_decl*, ")"
    function_signature_decl := (word | operator | parameter_decl)+
    word := <identifier>
    operator := <operator>
    parameter_decl := "(" variable_decl*, ")"
    function_body := statement*
    statement := lhs:statement_dest assign:("=" | "<<") rhs:expression
    statement_dest := (name_type_decl | variable_ref)
    variable_ref := <identifier>

    expression := (constant | operator | word | parameter_group)*
    constant := (<number> | <string>)
    parameter_group := "(" parameter*, ")"
    parameter := (names:<identifier>+, "=")? value:expression

    """

#--------------------------------------------------------------------------------------------------
@this_is_a_test
def test_grammar():
    log("test_grammar")
    grammar = build_grammar(s_zero_grammar_spec)
    test("test_grammar", grammar.dbg(), """
feature_decl := "feature" name:<identifier> feature_decl_2? ":indent" body:component_decl* ":undent"
feature_decl_2 := "extends" parent:<identifier>
component_decl := (test_decl | type_decl | variable_decl | function_decl)
test_decl := ">" lhs:expression test_decl_2?
test_decl_2 := "=>" rhs:expression
type_decl := "type" name_decl (struct_decl | type_relation_decl | enum_decl)
name_decl := name:<identifier> name_decl_1?
name_decl_1 := "|" alias:<identifier>
struct_decl := "=" ":indent" properties:variable_decl* ":undent"
type_relation_decl := (child_type_decl | parent_type_decl)
child_type_decl := "<" parent:<identifier>
parent_type_decl := ">" children:<indentifier>*|
enum_decl := "=" values:<identifier>*|
variable_decl := name_type_decl variable_decl_1?
variable_decl_1 := "=" default:expression
name_type_decl := (c_name_type_decl | ts_name_type_decl)
c_name_type_decl := type:<identifier> names:name_decl+,
ts_name_type_decl := names:name_decl+, ":" type:<identifier>
function_decl := modifier:("on" | "replace" | "after" | "before") result:result_type_decl assign:("=" | "<<") signature:function_signature_decl ":indent" function_body ":undent"
result_type_decl := "(" name_type_decl*, ")"
function_signature_decl := (word | operator | parameter_decl)+
word := <identifier>
operator := <operator>
parameter_decl := "(" variable_decl*, ")"
function_body := statement*
statement := lhs:statement_dest assign:("=" | "<<") rhs:expression
statement_dest := (name_type_decl | variable_ref)
variable_ref := <identifier>
expression := (constant | operator | word | parameter_group)*
constant := (<number> | <string>)
parameter_group := "(" parameter*, ")"
parameter := parameter_0? value:expression
parameter_0 := names:<identifier>+, "="
         """)
    test("grammar_errors", get_errors(grammar), """no errors""")

#--------------------------------------------------------------------------------------------------
# Parser itself

@this_is_the_test
def test_parser():
    log("test parser")
    grammar = build_grammar(s_zero_grammar_spec)
    test("parse_feature_decl", parse("feature MyFeature extends AnotherFeature { }", "feature_decl"), """
         {'_feature_decl': {'name': MyFeature, 'parent': AnotherFeature, 'body': []}}""")
    test("parse_expression", parse("a + b * c", "expression"), """
         {'_expression': [{'_word': a}, {'_operator': +}, {'_word': b}, {'_operator': *}, {'_word': c}]}""")
    test("parse_test_decl", parse("> a => b", "test_decl"), """
         {'_test_decl': {'lhs': {'_expression': {'_word': a}}, 'rhs': {'_expression': {'_word': b}}}}""")
    test("parse_variable_decl", parse("int r | red, g | green, b | blue =0", "variable_decl"),"""
         {'_variable_decl': {'type': int, 'names': [{'_name_decl': {'name': r, 'alias': red}}, {'_name_decl': {'name': g, 'alias': green}}, {'_name_decl': {'name': b, 'alias': blue}}], 'default': {'_expression': {'_constant': 0}}}}""")
    test("parse_type_decl", parse("type Col | Colour = { int r, g, b =0 }", "type_decl"))

@log_indent
def parse(code: str, rule_name: str) -> Dict:
    source = Source(code = code)
    ls = lexer(source)
    ast = parse_rule(ls, s_grammar.rule_named[rule_name])
    return clean_ast(ast)

# remove "len" members from all levels of the Dict
def clean_ast(ast: Dict) -> Dict:
    if isinstance(ast, list):
        return [clean_ast(a) for a in ast]
    elif isinstance(ast, dict):
        out = {}
        for k, v in ast.items():
            if k == "len": pass
            else:
                out[k] = clean_ast(v)
        return out
    else:
        return ast


@log_indent
def parse_rule(ls: List[Lex], rule: Rule) -> Dict:
    ast = {}
    i_lex = 0
    for term in rule.terms:
        log("i_lex:", i_lex)
        log("term:", term)
        if term.is_keyword():
            if i_lex >= len(ls):
                log("hit the end unexpectedly")
                log("ls:", ls)
                log("rule:", rule.dbg())
            if lex_matches(ls[i_lex], term.vals):
                i_lex += 1
            else: 
                return { "error": f"#1: expected one of {cleanup(term.vals)} at {ls[i_lex].location()}", "ast" : ast }
        elif term.is_type():
            if lex_matches(ls[i_lex], term.vals):
                if term.var: ast[term.var] = ls[i_lex]
                elif len(rule.terms)==1: ast = ls[i_lex]
                else: raise Exception("unbound terminal")
                i_lex += 1
            else:
                return { "error": f"#2: expected one of {cleanup(term.vals)} at {ls[i_lex].location()}", "ast" : ast }
        else:
            #log("term followers:", term.followers)
            i_lex_end = scan_forward(ls, i_lex, term.followers)
            if not i_lex_end:
                i_lex_end = len(ls)
                #return { "error": f"#3: expected one of {cleanup(term.followers)} at {ls[i_lex].location()}", "ast" : ast }
            sub_ls = ls[i_lex:i_lex_end]
            sub_ast = parse_term(sub_ls, term)
            #log("this one:", sub_ast)
            if err(sub_ast): return sub_ast
            if term.var: ast.update({ term.var : sub_ast })
            else: 
                if isinstance(sub_ast, list):
                    if len(sub_ast) > 1:
                        if len(rule.terms) > 1:
                            raise Exception("warning: throwing away multi-item list")
                        else: 
                            ast = sub_ast
                    elif len(sub_ast) == 1:
                        sub_ast = sub_ast[0]
                    elif len(sub_ast) == 0:
                        sub_ast = {}
                if isinstance(sub_ast, dict):
                    keys = list(sub_ast.keys())
                    if len(keys) > 0:
                        first_key = keys[0]
                        first_item = sub_ast[first_key]
                        if isinstance(first_item, dict):
                            ast.update(first_item)
                        else:
                            ast.update(sub_ast)
            i_lex = i_lex_end
    return { f"_{rule.name}": ast, "len" : len(ls[0:i_lex]) }

# top-level parser for a complex term (one or more rules, possible decorator)
@log_indent
def parse_term(ls: List[Lex], term: Term) -> Dict:
    result = None
    if not term.dec:
        result= parse_singular_term(ls, term)
    else:
        result = []
        min = 1 if term.dec == '+' else 0
        max = 1 if term.dec == '?' else None
        i_lex = 0
        #log("min:", min, "max:", max)
        #log("term.sep:", term.sep)
        term_followers = [f'"{term.sep}"'] if term.sep else (term.initials if max==None else [])
        #log("term_followers:", term_followers)
        while i_lex < len(ls) and (max == None or len(result) < max):
            i_lex_end = scan_forward(ls, i_lex, term_followers)
            if i_lex_end == None: i_lex_end = len(ls)
            sub_ls = ls[i_lex:i_lex_end]
            #log("sub_ls:", sub_ls)
            sub_ast = parse_singular_term(sub_ls, term)
            if err(sub_ast): # ok sort of weird thing here:
                # don't know how to distinguish between maybe an error and maybe not
                # I think we need to add the error to the end of the list
                #log("error:", sub_ast)
                #log("result is:", result)
                return result
            result.append(sub_ast)
            #log("adding", sub_ast)
            i_lex += sub_ast["len"]
            if (term.sep and i_lex < len(ls) and ls[i_lex].val == term.sep):
                i_lex += 1
        if len(result) < min:
            return { "error": f"expected at least {min} of {term} at {ls[0].location()}", "ast" : result }
    return result

    log_exit()

# parses a complex term (one or more rules), ignoring decorator
@log_indent
def parse_singular_term(ls: List[Lex], term: Term) -> Dict:
    leaves = term.leaves
    for val, rules in leaves.items():
        if lex_matches(ls[0], [val]):
            #log("found rules:", rules)
            for rule in rules:
                sub_ast = parse_rule(ls, rule)
                if not err(sub_ast): return sub_ast
    return { "error": f"expected one of {cleanup(term.vals)} at {ls[0].location()}", "ast" : {} }

def scan_forward(ls: List[Lex], i_lex: int, terminals: List[str]) -> int:
    while i_lex < len(ls):
        lex = ls[i_lex]
        if lex_matches(lex, terminals): return i_lex
        i_lex += ls[i_lex].jump
    return None

def lex_matches(lex: Lex, matches: List[str]) -> bool:
    if len(matches)==0: return False
    if matches[0][0]=='"': return f'"{lex.val}"' in matches
    elif matches[0][0]=='<': return f'<{lex.type}>' in matches
    else: return False

def err(ast: Dict) -> bool:
    return "error" in ast

def cleanup(obj) -> str:
    s = str(obj)
    return s.replace("\"", "").replace("'", "").replace("[","").replace("]","")
    