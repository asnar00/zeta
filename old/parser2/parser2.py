# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from util import *
from typing import List, Dict, Tuple, Union

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


# lexeme: stores value and type, and also position within the source
class Lex:
    def __init__(self, source: Source, pos: int, val: str, type: str, index: int):
        self.source = source; self.pos = pos; self.val = val; self.type = type; self.index = index
    def __str__(self):
        return self.val
    def __repr__(self):
        val = str(self)
        if self.type == "newline" : return log_grey(val)
        else: return val
    def location(self):
        return self.source.location(self.pos)
    
# lexer: reads source and produces lexemes
def lexer(source: Source) -> List[Lex]:
    # naive lexer just does a straight lex
    def naive_lexer(source: Source) -> List[Lex]:
        ls = []
        specs = [ ('number', r'\d+(\.\d*)?'),                           # integer or decimal number
                    ('identifier', r'[A-Za-z_][A-Za-z0-9_$\[\]]*'),     # identifiers
                    ('string', r'"(?:\\.|[^"\\])*"'),                   # string literals with support for escaped quotes
                    ('operator', r'[-+=%^<>*/?!|&#\\]{1,2}'),           # operators, and double-operators
                    ('punctuation', r'[(){}\[\],.;:]'),                 # punctuation
                    ('newline', r'(^[ ]+)|(\n[ ]*)'),                   # line-start plus 0 or more spaces
                    ('whitespace', r'[ ]+')]                            # spaces
        patterns = '|'.join('(?P<%s>%s)' % pair for pair in specs)
        regex = re.compile(patterns)
        pos = 0
        index = 0
        while pos < len(source.code):
            m = regex.match(source.code, pos)
            if not m: raise Exception(f'unexpected character "{source.code[pos]}" at {log_short(source.code[pos:])}')
            if len(m.group()) == 0: raise Exception(f'empty match at {log_short(source.code[pos:])}')
            type = m.lastgroup
            val = m.group()
            if (type != 'whitespace'):
                if type == 'newline':
                    ls.append(Lex(source, pos, val.replace("\n", "↩︎\n").replace(" ", "_"), type, index))
                else:
                    ls.append(Lex(source, pos, val, type, index))
            pos += len(val)
            index += 1
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
                        ols.append(Lex(lex.source, lex.pos, "ws-indent", "newline"))
                elif indent < last_indent:
                    for i in range(last_indent - indent):
                        ols.append(Lex(lex.source, lex.pos, "ws-undent", "newline"))
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
                ols.append(Lex(lex.source, lex.pos, ":indent", "newline"))
                i += 1
                while i < len(ls) and ls[i].val.startswith("ws-") : i += 1
                i -= 1
            elif lex.val == "}":    # add an undent, remove all previous ws-undents
                while len(ols) > 0 and ols[-1].val.startswith("ws-"): ols.pop()
                ols.append(Lex(lex.source, lex.pos, ":undent", "newline"))
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
    
    ls = naive_lexer(source)
    ls = insert_ws_indents(ls)
    ls = handle_braces(ls)
    ls = finalise_indents(ls)
    ls = filter_newlines(ls)
    for i, lex in enumerate(ls): lex.index = i
    return ls
#--------------------------------------------------------------------------------------------------
# Grammar: a collection of Rules, each made of cross-referencing Terms

test_grammar_spec = """
    expression = (constant | variable | brackets | operation | function)
    constant = (<number> | <string>)
    variable = <identifier>
    brackets = "(" expr:expression ")"
    operation = (prefix | infix | postfix)
    prefix = operator:<operator> expr:expression
    infix = left:expression operator:<operator> right:expression
    postfix = expr:expression operator:<operator>
    function = name:<identifier> "(" arguments:(argument ,)* ")"
    argument = name:(arg_name)? value:expression
    arg_name = <identifier> "="
    """

@this_is_a_test
def test_grammar_setup():
    log("test_grammar_setup")
    test("grammar_setup", Grammar(test_grammar_spec), """
expression = OneOf(Ref("constant"), Ref("variable"), Ref("brackets"), Ref("operation"), Ref("function"))
constant = OneOf(Type("number"), Type("string"))
variable = Type("identifier")
brackets = Keyword("("), expr:Ref("expression"), Keyword(")")
operation = OneOf(Ref("prefix"), Ref("infix"), Ref("postfix"))
prefix = operator:Type("operator"), expr:Ref("expression")
infix = left:Ref("expression"), operator:Type("operator"), right:Ref("expression")
postfix = expr:Ref("expression"), operator:Type("operator")
function = name:Type("identifier"), Keyword("("), arguments:ZeroOrMore(Ref("argument"), ","), Keyword(")")
argument = name:Optional(Ref("arg_name")), value:Ref("expression")
arg_name = Type("identifier"), Keyword("=")
         """)

#--------------------------------------------------------------------------------------------------
# Terms

class Term:
    def __init__(self):
        self.variable = None                # sets variable in the AST if present
    def __repr__(self): return str(self)
    def var(self): return f"{self.variable}:" if self.variable else ""
    def is_optional(self): return isinstance(self, Optional) or isinstance(self, ZeroOrMore)
    
class Terminal(Term): pass

class Type(Terminal):
    def __init__(self, type: str): super().__init__(); self.type = type
    def __str__(self): return f"{self.var()}Type(\"{self.type}\")"

class Keyword(Terminal):
    def __init__(self, word: str): super().__init__(); self.word = word
    def __str__(self): return f"{self.var()}Keyword(\"{self.word}\")"

class OneOf(Term):
    def __init__(self, terms: List[Term]): super().__init__(); self.terms = terms
    def __str__(self): return f"{self.var()}OneOf(" + ", ".join([str(term) for term in self.terms]) + ")"

class Optional(Term):
    def __init__(self, term: Term): super().__init__(); self.term = term
    def __str__(self): return f"{self.var()}Optional({self.term})"

class ZeroOrMore(Term):
    def __init__(self, term: Term, sep: str=""): 
        super().__init__()
        self.term = term; self.sep = Keyword(sep) if sep else None
    def __str__(self): 
        sep = f", \"{self.sep.word}\"" if self.sep else ""
        return f"{self.var()}ZeroOrMore({self.term}{sep})"

class Ref(Term):
    def __init__(self, rule_name): super().__init__(); self.rule_name = rule_name
    def __str__(self): return f"{self.var()}Ref(\"{self.rule_name}\")"

#--------------------------------------------------------------------------------------------------
# Grammar

class Rule:
    def __init__(self, name = "", terms : List[Term] = []):
        self.name = name
        self.terms = terms
        self.i_singular = self.is_singular()
    def __str__(self):
        return self.name + " = " + ", ".join([str(term) for term in self.terms])
    def __repr__(self): return str(self)
    def is_singular(self): # returns i_term if the rule has only one unnamed non-keyword term
        n_unnamed = 0
        i_term = None
        for i, term in enumerate(self.terms):
            if (not term.variable) and (not isinstance(term, Keyword)): 
                i_term = i
                n_unnamed += 1
        return i_term if n_unnamed == 1 else None
    def is_nodal(self) -> bool: # true if the rule is a single OneOf of multiple Refs
        if len(self.terms) != 1: return False
        if not isinstance(self.terms[0], OneOf): return False
        for term in self.terms[0].terms:
            if not isinstance(term, Ref): return False
        return True


class Grammar:
    def __init__(self, grammar_spec: str):
        self.rules = {}         # name => Rule
        self.keyword_map = {}   # word => List[List[Rule]]
        self.type_map = {}      # type => List[List[Rule]]
        self.rule_map = {}      # rule_name => List[List[Rule]]
        self.setup(grammar_spec)

    def __str__(self):
        out = ""
        for rule in self.rules.values():
            out += f"{rule}\n"
        return out
    
    def __repr__(self): return str(self)

    # parse a grammar spec (text) 
    def setup(self, gs: str):
        lines = gs.split("\n")
        lines = [line.strip() for line in lines]
        lines = [line for line in lines if line]
        for line in lines:
            rule = self.parse_rule(line)
            self.rules[rule.name] = rule

    def parse_rule(self, line: str):
        name, rhs = line.split(" = ")
        term_strs = self.split_terms(rhs.strip())
        terms = [self.parse_term(term_str) for term_str in term_strs]
        return Rule(name, terms)

    def split_terms(self, rhs: str) -> List[str]:
        bracket_level = 0
        term = ""
        terms: List[str] = []
        for i, c in enumerate(rhs):
            cp = rhs[i-1] if i > 0 else ""
            cn = rhs[i+1] if i < len(rhs) - 1 else ""
            quotes = cp == '"' and cn == '"'
            if c == "(" and not quotes: bracket_level += 1
            elif c == ")" and not quotes: bracket_level -= 1
            if bracket_level == 0 and c == " ":
                terms.append(term)
                term = ""
            else:
                term += c
        if term != "": terms.append(term)
        terms = [term for term in terms if term.strip()]
        return terms

    @log_disable
    def parse_term(self, term_str: str) -> Term:
        result = self.try_parse_set(term_str) or \
                self.try_parse_type(term_str) or \
                self.try_parse_keyword(term_str) or \
                self.try_parse_optional(term_str) or \
                self.try_parse_zero_or_more(term_str) or \
                self.try_parse_any(term_str) or \
                self.try_parse_rule_name(term_str)
        return result

    # match var:anything
    def try_parse_set(self, term_str: str) -> Term:
        # does it match variable_name:anything?
        m = re.match(r"(\w+):(.+)", term_str)
        if m:
            variable_name = m.group(1)
            rhs = m.group(2).strip()
            log("variable_name", variable_name)
            term = self.parse_term(rhs)
            term.variable = variable_name
            return term

    # match <type_name>
    def try_parse_type(self, term_str: str) -> Term:
        # does it match <something> ?
        m = re.match(r"<(\w+)>", term_str)
        if m:
            type_name = m.group(1)
            log("type_name", type_name)
            return Type(type_name)

    # match "word"
    def try_parse_keyword(self, term_str: str) -> Term:
        m = re.match(r'\"(.+)\"', term_str)
        if m:
            keyword = m.group(1)
            log("keyword", keyword)
            return Keyword(keyword)
    
    # match (t0 | t1 | t2 | ...)
    def try_parse_any(self, term_str: str) -> Term:
        # use a regexp to extract t0 .. etc
        m = re.match(r"\((.+)\)", term_str)
        if m:
            options = m.group(1).split(" | ")
            log("options:", options)
            return OneOf([self.parse_term(option) for option in options])
    
    # match (t)?
    def try_parse_optional(self, term_str: str) -> Term:
        # use a regexp to extract t
        m = re.match(r"\((.+)\)\?", term_str)
        if m:
            option = m.group(1)
            log("optional", option)
            return Optional(self.parse_term(option))
    
    # match (..)*
    def try_parse_zero_or_more(self, term_str: str) -> Term:
        # use a regexp to extract t
        m = re.match(r"\((.+)\)\*", term_str)
        if m:
            option = m.group(1)
            log("zero_or_more", option)
            parts = option.split(" ")
            name = parts[0]
            sep = parts[1] if len(parts) > 1 else ""
            return ZeroOrMore(self.parse_term(name), sep)
    
    # match rule name
    def try_parse_rule_name(self, term_str: str) -> Term:
        # regexp match alpha name
        m = re.match(r"(\w+)", term_str)
        if m:
            rule_name = m.group(1)
            log("rule_name", rule_name)
            return Ref(rule_name)

#--------------------------------------------------------------------------------------------------

 # Term In Rule : term (i_term) of (rule)
class TIR:
    def __init__(self, rule: Rule, i_term: int): self.rule = rule; self.i_term = i_term
    def __str__(self): return f"{self.rule.name}:{self.i_term}"
    def __repr__(self): return str(self)
    def __eq__(self, other): return self.rule == other.rule and self.i_term == other.i_term
    def term(self): return self.rule.terms[self.i_term]
    def end(self): return self.i_term >= len(self.rule.terms)
    def next(self): return TIR(self.rule, self.i_term + 1)
    def prev(self): return TIR(self.rule, self.i_term - 1)

# maps any lex-val ("x"), type (<x>) or rule-name (X) to a list of TIRs
class RuleMap:
    s_rule_map = None
    def __init__(self, grammar: Grammar):
        RuleMap.s_rule_map = self
        self.grammar = grammar
        self.map = {}
        for rule in grammar.rules.values():
            for i, term in enumerate(rule.terms):
                self.add_term(TIR(rule, i), term)
        self.add_transitives()
    
    def add_term(self, tir: TIR, term: Term):
        if isinstance(term, Type): self.add(f'<{term.type}>', tir)
        elif isinstance(term, Keyword): self.add(f'"{term.word}"', tir)
        elif isinstance(term, Ref): self.add(term.rule_name, tir)
        elif isinstance(term, OneOf):
            for subterm in term.terms: self.add_term(tir, subterm)
        elif isinstance(term, Optional): self.add_term(tir, term.term)
        elif isinstance(term, ZeroOrMore):
            self.add_term(tir, term.term)
            if term.sep: self.add_term(tir, term.sep)
    
    def add(self, key, val):
        if not key in self.map: self.map[key] = [val]
        else: self.map[key].append(val)

    def get(self, item: Lex|Rule) -> List[TIR]:
        if isinstance(item, Lex):
            return self.map.get(f'"{item.val}"', []) + \
                   self.map.get(f'<{item.type}>', [])
        elif isinstance(item, Rule):
            return self.map.get(item.name, [])
        
    def add_transitives(self):
        for rule in self.grammar.rules.values():
            tirs = self.get(rule)
            if len(tirs) == 1:
                parent_rule = tirs[0].rule
                if parent_rule.is_nodal():
                    self.map[rule.name] = []
                    parent_tirs = self.get(parent_rule)
                    for parent_tir in parent_tirs:
                        self.add(rule.name, parent_tir)


#--------------------------------------------------------------------------------------------------
# Parser helpers : these should gradually stabilise, even if the parser gets rewritten a bunch

    
# grammar-dependent information, abstracted out of the parser
# lex => bitmask_list (one bit per term per rule)

s_mg = None

class MetaGrammer:
    def __init__(self, grammar_spec: str):
        global s_mg
        s_mg = self
        self.grammar = Grammar(grammar_spec)
        self.rule_map = RuleMap(self.grammar)
        self.rules = list(self.grammar.rules.values())
        self.rule_name_to_int = {rule.name: i for i, rule in enumerate(self.rules)}
        self.rule_names = [rule.name for rule in self.rules]
        self.bitmask_lists = {}
        for key in self.rule_map.map.keys():
            self.bitmask_lists[key] = self.compute_bitmask_list(key)
        self.check_overshadows()
    
    def compute_bitmask_list(self, key:str) -> List[int]:
        n_rules = len(self.grammar.rules)
        tirs = self.rule_map.map[key]
        singular_rule_names = [tir.rule.name for tir in tirs if len(tir.rule.terms) == 1]
        singular_tirs = []
        for rule_name in singular_rule_names:
            singular_tirs += self.rule_map.get(self.grammar.rules[rule_name])
        tirs += singular_tirs
        bitmask_list = [0 for i in range(n_rules)]
        for tir in tirs:
            i_rule = self.rule_name_to_int[tir.rule.name]
            bitmask_list[i_rule] |= (1 << tir.i_term)
        for tir in singular_tirs:
            i_rule = self.rule_name_to_int[tir.rule.name]
            bitmask_list[i_rule] |= (1 << 9)                # set a special bit to indicate this was a singular rule-match
        return bitmask_list
    
    # given a match, return the bitmask list
    def get_bitmask_list(self, match: 'Match') -> List[int]:
        if match.rule:
            if match.rule.name in self.bitmask_lists: return self.bitmask_lists[match.rule.name]   
        else:
            item = match.ls[0]
            if f'"{item.val}"' in self.bitmask_lists: return self.bitmask_lists[f'"{item.val}"']
            if f'<{item.type}>' in self.bitmask_lists: return self.bitmask_lists[f'<{item.type}>']
        raise("unknown lex/rule")
    
    # given a bitmask list, see if any rule is completely matched
    def find_matched_rule(self, bitmask_list: List[int]) -> Rule:
        for i_rule, bitmask in enumerate(bitmask_list):
            rule_name = self.rule_names[i_rule]
            rule = self.grammar.rules[rule_name]
            n_terms = len(rule.terms)
            if bitmask == (1 << n_terms) - 1:
                return rule
            
    # checks to see which rules overshadow other rules
    def check_overshadows(self):
        self.is_overshadowed_by = [-1 for i in range(len(self.rules))]
        rule_strings = [self.rhs_without_vars(rule) for rule in self.rules]
        for i, rule_a in enumerate(rule_strings):
            for j, rule_b in enumerate(rule_strings):
                if j == i: continue
                if len(self.rules[i].terms) == 1 or len(self.rules[j].terms) == 1: continue
                if rule_strings[j] in rule_strings[i] and len(rule_strings[j]) < len(rule_strings[i]):
                    #log(f"{self.rules[j].name} is overshadowed by {self.rules[i].name}")
                    self.is_overshadowed_by[j] = i

    def rhs_without_vars(self, rule: Rule) -> str:
        rule_string = str(rule)
        rule_string = re.sub(r"\w+:", "", rule_string)      # remove variable names
        rule_string = re.sub(r"\w+ = ", "", rule_string)    # remove lhs
        rule_string = re.sub(r"\s+", "", rule_string)       # remove whitespace
        return rule_string
    

# Run is a partially-matched rule, with start and end ms
class Run:
    def __init__(self, rule: Rule, start: int, end: int):
        self.rule = rule; self.start = start; self.end = end; self.i_term = 0
    def __str__(self):
        return f"{self.rule.name}({self.start}-{self.end})"
    def __repr__(self): return str(self)
    def i_rule(self): return s_mg.rule_name_to_int[self.rule.name]
    def is_complete(self): return self.i_term == len(self.rule.terms)
    def length(self): return self.end - self.start + 1

# Match is a rule, a list of lexemes, and a list of sub-matches
class Match:
    def __init__(self, ls: List[Lex] =[], rule: Rule = None, matches: List['Match'] = []):
        self.ls = ls
        self.rule = rule
        self.matches = matches
        self.bitmask_list = []
    def __str__(self): return " ".join([str(lex) for lex in self.ls])
    def __repr__(self): return str(self)
    def pos(self): return self.ls[0].index if len(self.ls) > 0 else 0
    def length(self): return len(self.ls)
    def n_layers(self) -> int:
        n = 0
        for match in self.matches:
            n = max(n, match.n_layers())
        return n + 1
    def get_layer(self, i_layer: int, i_this_layer: int=0) -> List['Match']:
        if i_layer == i_this_layer: return [self]
        if i_layer > i_this_layer:
            out = []
            for m in self.matches:
                out += m.get_layer(i_layer, i_this_layer + 1)
            return out

class MatchDisplay:
    def __init__(self, grammar: Grammar):
        self.rule_names = [rule.name for rule in grammar.rules.values()]

    def width(self): return 12

    def pad(self, s, max_len, fn = None):
        padding = max_len - len(s)
        str = (" "*(padding//2)) + s + (" "*(padding-(padding//2)))
        return str if not fn else fn(str)

    def show_matches_single_layer(self, ms: List[Match]):
        out = ""
        pos = 0
        for i_match, match in enumerate(ms):
            if match.rule:
                n_spaces = match.pos() - pos
                pos = match.pos() + match.length()
                out += " "*((self.width()+1)*n_spaces)
                bg_fn = log_green_background
                out += self.pad(match.rule.name, self.width()*match.length()+(match.length()-1), bg_fn) + " "
        return out
    
    def show_matches(self, ms: List[Match]):
        # compute max number of layers
        n_layers = 0
        for match in ms:
            n_layers = max(n_layers, match.n_layers())
        out = ""
        for i_layer in range(n_layers):
            layer_ms = []
            for match in ms: layer_ms += match.get_layer(i_layer)
            out += self.show_matches_single_layer(layer_ms) + "\n"
        return out
            

    def show(self, ms: List[Match], runs: List[Run]):
        ls = []
        for m in ms: ls += m.ls
        out = ""
        pos = 0
        for i, m in enumerate(ms):
            if m.pos() > pos:
                n_spaces = m.pos() - pos
                out += " "*((self.width()+1)*n_spaces)
            
            pos = m.pos() + m.length() + 1
            bg_fn = log_grey_background 
            stir = str(i) + ": \"" + " ".join([str(lex) for lex in m.ls]) + "\""
            out += self.pad(stir, self.width()*m.length()+(m.length()-1), bg_fn) + " "
        log(out)

        log(self.show_matches(ms))
        log(self.show_rules(ms, runs))
        return
    
    def show_runs(self, runs: List[Run]):
        out = ""
        for run in runs:
            out += str(run) + " "
        log(out)

    def show_rules(self, ms: List[Match], runs: List[Run]) -> str:
        # get grammar rule names in an array
        out = ""
        for i_rule in range(len(self.rule_names)):
            pos = 0
            for i_match, match in enumerate(ms):
                n_spaces = match.pos() - pos
                pos = match.pos() + match.length() + 1
                out += " "*((self.width()+1)*n_spaces)
                bitmask = match.bitmask_list[i_rule]
                bg_fn = self.is_in_run(i_rule, i_match, runs)
                stir = self.str_bitmask(bitmask, self.rule_names[i_rule])
                out += self.pad(stir, self.width()*match.length()+(match.length()-1), bg_fn) + " "
            out += "\n"
        return out
    
    def is_in_run(self, i_rule: int, i_match: int, runs: List[Run]):
        found_runs = [run for run in runs if run and run.i_rule() == i_rule and (run.start <= i_match <= run.end)]
        if len(found_runs) == 0: return None
        return log_green_background if found_runs[0].is_complete() else log_grey_background

    def str_bitmask(self, bitmask: int, rule_name: str) -> str:
        if bitmask==0: return "•"
        out = rule_name + ":"
        for i in range(0, 8):
            out += (str(i)+",") if (bitmask & (1 << i)) > 0 else ""
        if out.endswith(","): out = out[:-1]
        if bitmask & (1 << 9): out += "~"
        return out
            
    def in_sequence(self, bitmask_a: int, bitmask_b: int) -> bool:
        return (bitmask_a & (bitmask_b >> 1)) > 0

#--------------------------------------------------------------------------------------------------
# Parser itself


collapse_list = [
    (6, 8, "brackets"),
    (2, 3, "arg_name"),
    (3, 5, "infix"),
    (2, 3, "argument"),
    (4, 4, "argument"),
    (2, 4, "argument"),
    (0, 3, "function")
]

class Parser:
    def __init__(self, grammar_spec: str = test_grammar_spec):
        MetaGrammer(grammar_spec)
        self.md = MatchDisplay(s_mg.grammar)
        
    def parse(self, code: str):
        ls = lexer(Source(code = code))
        ms = [self.match_from_lex(lex) for lex in ls]
       
        n_steps = 1
        for i in range(n_steps):
            start, end, rule_name = collapse_list[i]
            ms = self.collapse(ms, start, end, rule_name)
        self.show(ms, [])
    
    def bitmask_contains(self, bitmask: int, i_term: int) -> bool:
        return (bitmask & (1 << i_term)) > 0
    def lowest_bit_index(self, bitmask: int) -> int:
        for i in range(0, 8):
            if (bitmask & (1 << i)) > 0: return i
        return -1
            

   
    def collapse(self, matches: List[Match], i_from: int, i_to: int, new_rule_name: str) -> Match:
        new_rule = s_mg.grammar.rules[new_rule_name]
        sub_matches = matches[i_from:i_to+1]
        new_match = self.match_from_rule(new_rule, sub_matches)
        matches = matches[:i_from] + [new_match] + matches[i_to+1:]
        return matches
    
    def match_from_lex(self, lex: Lex): 
        match= Match([lex])
        match.bitmask_list = s_mg.get_bitmask_list(match)
        return match

    def match_from_rule(self, rule: Rule, matches: List['Match']):
        ls = []
        for match in matches:
            ls += match.ls
        # if we're matching a rule that was promoted from a singular, set the singular
        for match in matches:
            if match.rule == None:
                i_rule = s_mg.rule_name_to_int[rule.name]
                special = (match.bitmask_list[i_rule] & (1 << 9)) > 0
                if special: # set the rule to the one that promoted us
                    match.rule = s_mg.find_matched_rule(match.bitmask_list)
        match = Match(ls, rule, matches)
        match.bitmask_list = s_mg.get_bitmask_list(match)
        return match
    
    def show(self, matches: List[Match], runs: List[Run]):
        self.md.show(matches, runs)
        return matches


        
            

#--------------------------------------------------------------------------------------------------

@this_is_the_test
def test_parser():
    log("test_parser")
    p = Parser()
    p.parse("f(n=x+(6), y)")