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
    def __str__(self):
        out = " | ".join(self.vals)
        if len(self.vals) > 1: out = f"({out})"
        if self.dec: out += self.dec
        if self.sep: out += self.sep
        if self.var: out = f"{self.var}:{out}"
        return out
    def __repr__(self): return self.__str__()

#--------------------------------------------------------------------------------------------------
# Rule: a name and a list of terms

class Rule:
    def __init__(self, name: str):
        self.name = name
        self.terms = []
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
    log(grammar.dbg())
    return grammar
    
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

# a rule is abstract if it has one term that points to more than one rule
@memoise
def abstract_children(rule: Rule) -> List[Rule]:
    if len(rule.terms) != 1: return []
    term = rule.terms[0]
    if term.dec: return []
    return get_rules(term)

# if a term is one or more keywords, return the list, otherwise none
@memoise
def get_keywords(term: Term) -> List[str]:
    return term.vals if term.vals and term.vals[0][0] == '"' else []

# if a term is one or more lex-types, return the list, otherwise none
@memoise
def get_types(term: Term) -> List[str]:
    return term.vals if term.vals and term.vals[0][0] == '<' else []

# returns keywords and types in a single list (all will be one or another)
@memoise
def get_terminals(term: Term) -> List[str]:
    return get_keywords(term) + get_types(term)

# if a term is a list of rules, return the list of rules, otherwise none
@memoise
def get_rules(term: Term) -> List[str]:
    if not term.vals or term.vals[0][0] in '<"': return []
    return [s_grammar.rule_named[val] for val in term.vals]

# check that each rule referred to in each term actually exists
@memoise
def get_errors(grammar: Grammar) -> str:
    out = ""
    for rule in grammar.rules:
        for i_term, term in enumerate(rule.terms):
            for val in term.vals:
                if val[0] and not (val[0] in '"<'):
                    if not (val in s_grammar.rule_named):
                        out += f"term {i_term} of {rule.name}: can't find referred-to rule '{val}'\n"
    if out == "":
        out = "no errors"
    return out

# merge two dicts of lists
def merge_dicts(d1: dict, d2: dict) -> dict:
    for key, vals in d2.items():
        if key in d1: 
            for val in vals:
                if not val in d1[key]: d1[key].append(val)
        else: d1[key] = vals
    return d1

# "initials" is a dict mapping a terminal-string to a list of rules it initiates
@memoise
def get_initials_for_rule(rule: Rule) -> dict:
    results = {}        # terminal_str -> [rule]
    for term in rule.terms:
        terminals = get_terminals(term)
        if terminals:
            for t in terminals:
                results = merge_dicts(results, {t: [rule]})
        else:
            for sub_rule in get_rules(term):
                add = get_initials_for_rule(sub_rule)
                results = merge_dicts(results, add)
        if not term.dec or not (term.dec in '?*'):
            return results
    return results

# show all initials
@memoise
def show_initials(grammar: Grammar) -> str:
    out = ""
    for rule in grammar.rules:
        out += rule.name + " : " + str(get_initials_for_rule(rule)) + "\n"
    return out

# get initials for a term: all the terminals that could possible start it
@memoise
def get_initials_for_term(term: Term) -> List[str]:
    terminals = get_terminals(term)
    if terminals:
        result = {}
        for t in terminals:
            result[t] = []
        return result
    rules = get_rules(term)
    result = {}
    for rule in rules:
        result = merge_dicts(result, get_initials_for_rule(rule))
    return result

# terminators are all terminals that could follow a given rule
# we compute them in a single pass because it's better that way
def compute_terminators() -> dict:
    terminators = {}
    # first create an empty dict for each rule
    for rule in s_grammar.rules:
        terminators[rule.name] = {}
    # now run through and look at all terms
    for rule in s_grammar.rules:
        for i_term, term in enumerate(rule.terms):
            sub_rules = get_rules(term)
            if sub_rules: # now look at initials for the next term
                if i_term+1 < len(rule.terms):
                    initials = get_initials_for_term(rule.terms[i_term+1])
                if term.sep:
                    initials = merge_dicts(initials, { f'"{term.sep}"' : [] })
                for sub_rule in sub_rules:
                    terminators[sub_rule.name] = merge_dicts(terminators[sub_rule.name], initials)
    # all last-terms should add the terminators of the rule they're in
    for rule in s_grammar.rules:
        last_term = rule.terms[-1]
        sub_rules = get_rules(last_term)
        if sub_rules:
            for sub_rule in sub_rules:
                terminators[sub_rule.name] = merge_dicts(terminators[sub_rule.name], terminators[rule.name])
    # each abstract rule should push its terminators down to its children
    for rule in s_grammar.rules:
        children = abstract_children(rule)
        for child in children:
            terminators[child.name] = merge_dicts(terminators[child.name], terminators[rule.name])
    return terminators



#--------------------------------------------------------------------------------------------------
# Zero grammar spec

s_zero_grammar_spec : str = """
    feature_decl := "feature" name:<identifier> ("extends" parent:<identifier>)? <indent> component_decl* <undent>
    component_decl := (test_decl | type_decl | variable_decl | function_decl)

    test_decl := ">" lhs:expression ("=>" rhs:expression)?

    type_decl := "type" name_decl (struct_decl | type_relation_decl | enum_decl)
    name_decl := name:<identifier> ("|" alias:<identifier>)?
    struct_decl := "=" <indent> properties:variable_decl* <undent>
    type_relation_decl := (child_type_decl | parent_type_decl)
    child_type_decl := "<" parent:<identifier>
    parent_type_decl := ">" children:<indentifier>*|
    enum_decl := "=" values:<identifier>*|

    variable_decl := name_type_decl ("=" default:expression)?
    name_type_decl := (c_name_type_decl | ts_name_type_decl)
    c_name_type_decl := type:<identifier> names:name_decl+,
    ts_name_type_decl := names:name_decl+, ":" type:<identifier>

    function_decl := modifier:modifier result:result_type_decl assign:assign_op signature:function_signature_decl <indent> function_body <undent>
    modifier := ("on" | "replace" | "after" | "before")
    result_type_decl := "(" name_type_decl*, ")"
    assign_op := ("=" | "<<")
    function_signature_decl := (word | operator | parameter_decl)+
    word := <identifier>
    operator := <operator>
    parameter_decl := "(" variable_decl*, ")"
    function_body := statement*
    statement := lhs:statement_dest assign:assign_op rhs:expression
    statement_dest := (name_type_decl | variable_ref)
    variable_ref := <identifier>

    expression := (constant | operator | word | parameter_group)*
    constant := (<number> | <string>)
    parameter_group := "(" parameter*, ")"
    parameter := (names:<identifier>+, "=")? value:expression

    """

#--------------------------------------------------------------------------------------------------
@this_is_the_test
def test_grammar():
    log("test_grammar")
    grammar = build_grammar(s_zero_grammar_spec)
    test("test_grammar", grammar.dbg(), """
feature_decl := "feature" name:<identifier> feature_decl_2? <indent> component_decl* <undent>
feature_decl_2 := "extends" parent:<identifier>
component_decl := (test_decl | type_decl | variable_decl | function_decl)
test_decl := ">" lhs:expression test_decl_2?
test_decl_2 := "=>" rhs:expression
type_decl := "type" name_decl (struct_decl | type_relation_decl | enum_decl)
name_decl := name:<identifier> name_decl_1?
name_decl_1 := "|" alias:<identifier>
struct_decl := "=" <indent> properties:variable_decl* <undent>
type_relation_decl := (child_type_decl | parent_type_decl)
child_type_decl := "<" parent:<identifier>
parent_type_decl := ">" children:<indentifier>*|
enum_decl := "=" values:<identifier>*|
variable_decl := name_type_decl variable_decl_1?
variable_decl_1 := "=" default:expression
name_type_decl := (c_name_type_decl | ts_name_type_decl)
c_name_type_decl := type:<identifier> names:name_decl+,
ts_name_type_decl := names:name_decl+, ":" type:<identifier>
function_decl := modifier:modifier result:result_type_decl assign:assign_op signature:function_signature_decl <indent> function_body <undent>
modifier := ("on" | "replace" | "after" | "before")
result_type_decl := "(" name_type_decl*, ")"
assign_op := ("=" | "<<")
function_signature_decl := (word | operator | parameter_decl)+
word := <identifier>
operator := <operator>
parameter_decl := "(" variable_decl*, ")"
function_body := statement*
statement := lhs:statement_dest assign:assign_op rhs:expression
statement_dest := (name_type_decl | variable_ref)
variable_ref := <identifier>
expression := (constant | operator | word | parameter_group)*
constant := (<number> | <string>)
parameter_group := "(" parameter*, ")"
parameter := parameter_0? value:expression
parameter_0 := names:<identifier>+, "="
         """)
    test("grammar_errors", get_errors(grammar), """no errors""")
    terminators = compute_terminators()
    for rule in grammar.rules:
        log(rule.name + f": {str(list(terminators[rule.name].keys())).replace("'", "")}")