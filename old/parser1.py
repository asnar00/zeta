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
variable = name:Type("identifier")
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
        if isinstance(term, Type):
            self.add(f'<{term.type}>', tir)
        elif isinstance(term, Keyword):
            self.add(f'"{term.word}"', tir)
        elif isinstance(term, Ref):
            self.add(term.rule_name, tir)
        elif isinstance(term, OneOf):
            for subterm in term.terms:
                self.add_term(tir, subterm)
        elif isinstance(term, Optional):
            self.add_term(tir, term.term)
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

    
        
    # given an item, look at the map and return all possible tirs
    @staticmethod
    def find_tirs(item: Union[Lex, 'Partial']) -> List[TIR]:
        if isinstance(item, Lex): return RuleMap.s_rule_map.get(item)
        elif isinstance(item, Partial): return RuleMap.s_rule_map.get(item.tir.rule)
        else: raise Exception("invalid item in find_tirs")

    @staticmethod
    def find_start_tirs(item: Union[Lex, 'Partial']) -> List[TIR]:
        tirs = RuleMap.find_tirs(item)
        start_tirs = [tir for tir in tirs if tir.i_term == 0]
        # tolerate one start-item that's Optional/ZeroOrMore (todo: tolerate more)
        first_tirs = [tir for tir in tirs if tir.i_term == 1]
        for tir in first_tirs:
            term = tir.prev().term()
            if isinstance(term, Optional) or isinstance(term, ZeroOrMore):
                start_tirs.append(tir)
        return start_tirs

    def show(self):
        for key, tirs in self.map.items():
            log(key, "=>", tirs)


# Partial Match
class Partial:
    def __init__(self, tir: TIR):
        self.tir = tir                  # "open" term we're expecting
        self.from_lex_index = None      # index of the lexeme that started this partial
        self.to_lex_index = None        # index of the last lexeme in this partial
        self.items = []                 # list of items in this partial 
        for term in self.tir.rule.terms:
            if isinstance(term, ZeroOrMore): self.items.append([])
            else: self.items.append(None)
    def __repr__(self): return str(self)
    def __str__(self):
        rule_name = self.tir.rule.name
        matched = self.is_matched()
        if matched: rule_name = log_green(rule_name)
        items = [str(item) for item in self.items]
        range = f"[{self.from_lex_index}-{self.to_lex_index}]" if self.from_lex_index != None else ""
        items = ""
        for i, item in enumerate(self.items):
            # if item is a List: iterate through it
            if isinstance(item, list):
                if len(item) == 0: items += ". "
                else: items += f"[{', '.join([str(i) for i in item])}] "
            else:
                if item == None: items += ". "
                else: items += str(item) + " "
        return f"({rule_name}{log_grey(range)} {log_grey(items)})"
    
    # check to see if all terms that need to be, have been matched
    def is_matched(self):
        for i_term, term in enumerate(self.tir.rule.terms):
            if not(isinstance(term, Optional) or isinstance(term, ZeroOrMore)) \
                and self.items[i_term] == None:
                return False
        return True
    
    # return the AST for this partial
    def get_ast(self):
        if len(self.items) == 1:
            return { self.tir.rule.name: self.items[0] }
        ast = {}
        rule = self.tir.rule
        for i in range(len(rule.terms)):
            term = rule.terms[i]
            item = self.items[i]
            if isinstance(item, Partial): item = item.get_ast()
            if not isinstance(term, Keyword): ast.update({ term.variable: item })
        return { self.tir.rule.name: ast }
    
    # number of lexemes in the partial match
    def length(self): 
        if self.from_lex_index == None: return 0
        return (self.to_lex_index - self.from_lex_index) + 1
    
    # check if the lexeme fits within the partial's lex range
    def check_lex_range(self, item) -> Tuple[int, int]:
        from_index, to_index = Partial.get_lex_range(item)
        if self.to_lex_index == None: return from_index, to_index
        if self.to_lex_index + 1 == from_index:
            return self.from_lex_index, to_index
        return None, None
    
    # get the lex range for a lexeme or partial
    @staticmethod
    def get_lex_range(item) -> Tuple[int, int]:
        if isinstance(item, Lex): return item.index, item.index
        return item.from_lex_index, item.to_lex_index
    
    # create new partials for any rules that start with this item
    @staticmethod
    def new_partials(item:Union[Lex, 'Partial']) -> List['Partial']:
        tirs = RuleMap.find_start_tirs(item)
        new_partials = [Partial.new_partial(tir, item) for tir in tirs]
        return new_partials
    
    # create a new partial for an item at position (tir)
    @staticmethod
    def new_partial(tir: TIR, item: Union[Lex, 'Partial']) -> 'Partial':
        p = Partial(tir)
        p.add_item_to_partial_at(item, tir)
        return p
    
    # add a newly matched item to a partial; return None if failed
    def add_item_to_partial(self, item: Union[Lex, 'Partial']):
        matched_tir = self.can_add_item_to_partial(item)
        return self.add_item_to_partial_at(item, matched_tir) if matched_tir else None

    # can we add an item to a partial? if so, return the tir, otherwise None
    def can_add_item_to_partial(self, item: Union[Lex, 'Partial']) -> TIR|None:
        matched_tir = self.find_matched_tir_at(item, self.tir)
        if not matched_tir: return None
        from_lex_index, to_lex_index = self.check_lex_range(item)
        if to_lex_index == None: return None
        return matched_tir

    # add a new item to a partial at a known tir
    def add_item_to_partial_at(self, item: Union[Lex, 'Partial'], tir: TIR):
        term = tir.term()
        if isinstance(term, ZeroOrMore):
            self.items[tir.i_term].append(item)    # add to list
        else:
            self.items[tir.i_term] = item          # set item, step
            self.tir = tir.next()                  # step forward one
        from_lex_index, to_lex_index = self.check_lex_range(item)
        if self.from_lex_index == None: self.from_lex_index = from_lex_index
        self.to_lex_index = to_lex_index
        return self
        
    # find the term after or at tir that matches item
    def find_matched_tir_at(self, item: Union[Lex, 'Partial'], tir: TIR) -> TIR|None:
        if tir.end(): return None
        tirs = RuleMap.find_tirs(item)
        matched_tir = tir if tir in tirs else None
        term = tir.term()
        if isinstance(term, Optional):
            if not matched_tir: 
                return self.find_matched_tir_at(item, tir.next())
        elif isinstance(term, ZeroOrMore):
            if not matched_tir: 
                return self.find_matched_tir_at(item, tir.next())
            expecting_separator = (len(self.items[tir.i_term]) % 2) == 0
            is_separator = isinstance(item, Lex) and item.val == term.sep.word
            separator_ok = (expecting_separator == is_separator)
            return matched_tir if separator_ok else None
        else:
            return matched_tir
        
     
    # the "old" debug view
    @staticmethod
    def show_old(label, partials: List['Partial']) -> str:
        out = label 
        if len(partials) > 0:
            out += "\n  " + "\n  ".join([str(p) for p in partials]) + "\n"
        else: out+= "[]"
        log(out)

    # the "new" debug view
    @staticmethod
    def show(label, partials, ls):
        max_len = 0
        for rule in RuleMap.s_rule_map.grammar.rules.values():
            max_len = max(max_len, len(rule.name) + 3)

        def pad(s, n, col_fn):
            w = ((max_len * n) - len(s))
            start = " " * (w // 2)
            end = " " * (w - (w//2))
            return col_fn(start + s + end) + " "
        
        log(label)
        out = ""
        for lex in ls:
            out += pad(lex.val, 1, log_grey_background)
        log(out)
        for p in partials:
            out = ""
            pos = p.from_lex_index * (max_len+1)
            length = p.length()
            back = log_green_background if p.is_matched() else log_grey_background
            out += (" " * pos) + pad(p.tir.rule.name, length, back) 
            log("\n" + out + (" " * max_len) + log_grey(str(p.get_ast())))


        log_flush()


        
    

#--------------------------------------------------------------------------------------------------

@this_is_the_test
def test_parser():
    log("test_parser")

    p = Parser(test_grammar_spec)
    test("constant", p.parse("1.0"), """{'constant': 1.0}""")
    test("variable", p.parse("a"), """{'variable': a}""")
    test("function_incomplete", p.parse("f("), """{'error': 'expected function.arguments'}""")
    test("postfix", p.parse("a +"), """{'postfix': {'expr': {'variable': a}, 'operator': +}}""")
    test("infix", p.parse("a + b"), """{'infix': {'left': {'variable': a}, 'operator': +, 'right': {'variable': b}}}""")

#--------------------------------------------------------------------------------------------------
# Parser does the work

class Parser:
    def __init__(self, grammar_spec):
        self.grammar = Grammar(grammar_spec)
        self.map = RuleMap(self.grammar)

    def parse(self, code: str):
        self.ls = lexer(Source(code = code))
        partials = []
        for lex in self.ls:
            log("------------------------")
            partials = self.remove_old_partials(lex, partials)
            Partial.show("partials:", partials, self.ls)
            partials = self.add_lex(lex, partials)
            partials = self.reduce(partials)

        return self.result(partials)
    
    # remove all partials whose to_index is less than the lex index
    def remove_old_partials(self, lex: Lex, partials: List[Partial]) -> List[Partial]:
        return [p for p in partials if p.to_lex_index == None or p.to_lex_index >= lex.index-1]
    
    # reduce partials to remove any that are fully matched
    def reduce(self, partials: List[Partial]) -> List[Partial]:
        matched, unmatched = self.separate_partials(partials)
        new_matched = []
        new_unmatched = []
        for m in matched:
            # see if m fits anywhere in unmatched
            if not self.try_match(m, unmatched):
                new_matched.append(m)
                new_unmatched += Partial.new_partials(m)
        out_partials = new_matched + new_unmatched + unmatched
        Partial.show("reduce:", out_partials, self.ls)
        return out_partials
    
    # try to match a partial in a set of partials
    def try_match(self, p: Partial, partials: List[Partial]) -> bool:
        result = False
        for p2 in partials:
            if p2.add_item_to_partial(p):
                result = True
        return result
    
    # add a lexeme to a set of partials
    def add_lex(self, lex: Lex, partials: List[Partial]) -> List[Partial]:
        new_partials = Partial.new_partials(lex) + self.try_match_lex(lex, partials)
        Partial.show("add_lex:", new_partials, self.ls)
        return new_partials
    
    # add lex to each partial tht can accept it
    def try_match_lex(self, lex: Lex, partials: List[Partial]) -> List[Partial]:
        for p in partials:
            p.add_item_to_partial(lex)
        return partials
    
    # result returns nothing for now
    def result(self, partials: List[Partial]) -> Dict:
        Partial.show("result", partials, self.ls)
        matched, unmatched = self.separate_partials(partials)
        longest_matched = self.find_longest_partial(matched)
        longest_unmatched = self.find_longest_partial(unmatched)
        log("longest matched:", longest_matched)
        if longest_matched and (longest_unmatched == None or \
                                longest_matched.length() >= longest_unmatched.length()):
            return longest_matched.get_ast()
        return { "error" : self.error_message(longest_unmatched) }
    
    # error message given a set of unmatched partials
    def error_message(self, p: Partial) -> str:
        msg = "expected "
        term = p.tir.term()
        msg += p.tir.rule.name
        if term.variable: msg += "." + term.variable
        if msg.endswith(", "): msg = msg[:-2]
        return msg
    
    # find the longest partial
    def find_longest_partial(self, partials: List[Partial]) -> Partial:
        if len(partials) == 1: return partials[0]
        longest_length = partials[0].length()
        longest = partials[0]
        for p in partials[1:]:
            if p.length() > longest_length:
                longest_length = p.length()
                longest = p
        return longest
    
    # separate partials into matched and unmatched
    def separate_partials(self, partials: List[Partial]) -> Tuple[List[Partial], List[Partial]]:
        matched = []
        unmatched = []
        for p in partials:
            if p.is_matched(): matched.append(p)
            else: unmatched.append(p)
        return matched, unmatched
    
    



   