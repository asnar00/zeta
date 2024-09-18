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
    def __init__(self, source: Source, pos: int, val: str, type: str):
        self.source = source; self.pos = pos; self.val = val; self.type = type
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
    return ls
#--------------------------------------------------------------------------------------------------
# Grammar: a collection of Rules, each made of cross-referencing Terms

test_grammar_spec = """
    expression = (constant | variable | brackets | operation | function)
    constant = value:(<number> | <string>)
    variable = name:<identifier>
    brackets = "(" expr:expression ")"
    operation = (prefix | infix | postfix)
    prefix = operator:<operator> expr:expression
    infix = left:expression operator:<operator> right:expression
    postfix = expr:expression operator:<operator>
    function = name:<identifier> "(" args:(argument ,)* ")"
    argument = (arg_name)? value:expression
    arg_name = name:<identifier> "="
    """

@this_is_a_test
def test_grammar_setup():
    log("test_grammar_setup")

    test("grammar_setup", Grammar(test_grammar_spec), """
expression = OneOf(Ref("constant"), Ref("variable"), Ref("brackets"), Ref("operation"), Ref("function"))
constant = value:OneOf(Type("number"), Type("string"))
variable = name:Type("identifier")
brackets = Keyword("("), expr:Ref("expression"), Keyword(")")
operation = OneOf(Ref("prefix"), Ref("infix"), Ref("postfix"))
prefix = operator:Type("operator"), expr:Ref("expression")
infix = left:Ref("expression"), operator:Type("operator"), right:Ref("expression")
postfix = expr:Ref("expression"), operator:Type("operator")
function = name:Type("identifier"), Keyword("("), args:ZeroOrMore(Ref("argument"), ","), Keyword(")")
argument = Optional(Ref("arg_name")), value:Ref("expression")
arg_name = name:Type("identifier"), Keyword("=")
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
    def __init__(self, type_name): super().__init__(); self.type_name = type_name
    def __str__(self): return f"{self.var()}Type(\"{self.type_name}\")"

class Keyword(Terminal):
    def __init__(self, word): super().__init__(); self.word = word
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
    def __init__(self, name): super().__init__(); self.name = name
    def __str__(self): return f"{self.var()}Ref(\"{self.name}\")"

#--------------------------------------------------------------------------------------------------
# Grammar

class Rule:
    def __init__(self, name = "", terms : List[Term] = []):
        self.name = name
        self.terms = terms
    def __str__(self):
        return self.name + " = " + ", ".join([str(term) for term in self.terms])
    def __repr__(self): return str(self)

class Grammar:
    def __init__(self, grammar_spec: str):
        self.rules = {}         # name => Rule
        self.keyword_map = {}   # word => List[List[Rule]]
        self.type_map = {}      # type => List[List[Rule]]
        self.rule_map = {}      # rule_name => List[List[Rule]]
        self.setup(grammar_spec)
        self.setup_map()

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
            rule = parse_rule(line)
            self.rules[rule.name] = rule

    # find rule for either lex or parsed ast
    def find_rules(self, item: Union[Lex, Rule], i_term: int) -> List[Rule]:
        if isinstance(item, Lex):
            return self.find_terminal_rules(item, i_term)
        elif isinstance(item, Rule):
            return self.find_nonterminal_rules(item.name, i_term)
        return []

    # find all rules that accept (lex) at position (i_term)
    def find_terminal_rules(self, lex: Lex, i_term: int) -> List[Rule]:
        if lex.val in self.keyword_map:
            if i_term >= len(self.keyword_map[lex.val]): return []
            return (self.keyword_map[lex.val])[i_term]
        elif lex.type in self.type_map:
            if i_term >= len(self.type_map[lex.type]): return []
            return (self.type_map[lex.type])[i_term]
        return []
    
    # find rules that accept some ast(rule_name) in position i_term
    def find_nonterminal_rules(self, rule_name: str, i_term: int) -> List[Rule]:
        if rule_name in self.rule_map:
            if i_term >= len(self.rule_map[rule_name]): return []
            return (self.rule_map[rule_name])[i_term]
        return []
    
    # process the grammar to build a fast map from (type/keyword/rule) => (rule, i_term)
    def setup_map(self):
        for rule in self.rules.values():
            for i, term in enumerate(rule.terms):
                key_terms = self.find_key_terms(term)
                for key_term in key_terms:
                    if isinstance(key_term, Keyword):
                        self.update_list(self.keyword_map, key_term.word, i, rule)
                    elif isinstance(key_term, Type):
                        self.update_list(self.type_map, key_term.type_name, i, rule)
                    elif isinstance(key_term, Ref):
                        self.update_list(self.rule_map, key_term.name, i, rule)
        self.add_nodal_rules()

    # where a rule maps to a single nodal rule, transfer its maps
    def add_nodal_rules(self):
        for rule_name, rules in self.rule_map.items():
            # does this map to a nodal rule?
            if len(rules) == 1 and len(rules[0]) == 1:
                rule = rules[0][0]
                if self.is_nodal_rule(rule):
                    self.rule_map[rule_name] = self.rule_map[rule.name]

    # returns true if the rule is "nodal" - is a OneOf with only one term
    def is_nodal_rule(self, rule: Rule) -> bool:
        return len(rule.terms) == 1 and isinstance(rule.terms[0], OneOf)
    
    def update_list(self, m: Dict, key: str, index: int, rule: Rule):
        if key not in m: m[key] = []
        if len(m[key]) <= index:
            n_add = (index + 1) - len(m[key])
            m[key] += [[] for _ in range(n_add)]
        m[key][index].append(rule)

    def find_key_terms(self, term: Term) -> List[Term]:
        if isinstance(term, Type): return [term]
        elif isinstance(term, Keyword): return [term]
        elif isinstance(term, OneOf): 
            key_terms = []
            for t in term.terms:
                key_terms += self.find_key_terms(t)
            return key_terms
        elif isinstance(term, Optional): return [self.find_key_terms(term.term)]
        elif isinstance(term, ZeroOrMore): 
            key_terms = self.find_key_terms(term.term)
            if term.sep: key_terms += [term.sep]
            return key_terms
        elif isinstance(term, Ref): return [term]
        return term
    
    def show_map(self, m: Dict):
        out = ""
        for key, rules in m.items():
            out += f"{key} => "
            for i, rule_list in enumerate(rules):
                if len(rule_list) > 0:
                    for rule in rule_list:
                        full_match = "*" if len(rule.terms) == 1 else ""
                        out += f"{full_match}{rule.name}:{i} "
            out += "\n"
        log(out)

def parse_rule(line: str):
    name, rhs = line.split(" = ")
    term_strs = split_terms(rhs.strip())
    terms = [parse_term(term_str) for term_str in term_strs]
    return Rule(name, terms)

def split_terms(rhs: str) -> List[str]:
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
def parse_term(term_str: str) -> Term:
    result = try_parse_set(term_str) or \
            try_parse_type(term_str) or \
            try_parse_keyword(term_str) or \
            try_parse_optional(term_str) or \
            try_parse_zero_or_more(term_str) or \
            try_parse_any(term_str) or \
            try_parse_rule_name(term_str)
    return result

# match var:anything
def try_parse_set(term_str: str) -> Term:
    # does it match variable_name:anything?
    m = re.match(r"(\w+):(.+)", term_str)
    if m:
        variable_name = m.group(1)
        rhs = m.group(2).strip()
        log("variable_name", variable_name)
        term = parse_term(rhs)
        term.variable = variable_name
        return term

# match <type_name>
def try_parse_type(term_str: str) -> Term:
    # does it match <something> ?
    m = re.match(r"<(\w+)>", term_str)
    if m:
        type_name = m.group(1)
        log("type_name", type_name)
        return Type(type_name)

# match "word"
def try_parse_keyword(term_str: str) -> Term:
    m = re.match(r'\"(.+)\"', term_str)
    if m:
        keyword = m.group(1)
        log("keyword", keyword)
        return Keyword(keyword)
    
# match (t0 | t1 | t2 | ...)
def try_parse_any(term_str: str) -> Term:
    # use a regexp to extract t0 .. etc
    m = re.match(r"\((.+)\)", term_str)
    if m:
        options = m.group(1).split(" | ")
        log("options:", options)
        return OneOf([parse_term(option) for option in options])
    
# match (t)?
def try_parse_optional(term_str: str) -> Term:
    # use a regexp to extract t
    m = re.match(r"\((.+)\)\?", term_str)
    if m:
        option = m.group(1)
        log("optional", option)
        return Optional(parse_term(option))
    
# match (..)*
def try_parse_zero_or_more(term_str: str) -> Term:
    # use a regexp to extract t
    m = re.match(r"\((.+)\)\*", term_str)
    if m:
        option = m.group(1)
        log("zero_or_more", option)
        parts = option.split(" ")
        name = parts[0]
        sep = parts[1] if len(parts) > 1 else ""
        return ZeroOrMore(parse_term(name), sep)
    
# match rule name
def try_parse_rule_name(term_str: str) -> Term:
    # regexp match alpha name
    m = re.match(r"(\w+)", term_str)
    if m:
        rule_name = m.group(1)
        log("rule_name", rule_name)
        return Ref(rule_name)
    
#--------------------------------------------------------------------------------------------------
# Partial represents a partially matched rule
class Partial:
    def __init__(self, rule: Rule, first_item):
        self.rule = rule        # rule that we're trying to match
        self.matched = []       # list of matched terms
        self.ast = {}           # the ast we're building
        for term in rule.terms:
            if isinstance(term, ZeroOrMore): self.matched.append([])
            else: self.matched.append(None)
        self.i_term = 0
        self.set_item_at(0, first_item)
    def __str__(self):
        ast = str(self.get_ast()).replace("'", "")
        ms = f"{self.i_term}/{len(self.rule.terms)} {ast}"
        if self.is_matched(): ms = log_green(ms)
        return ms
    def __repr__(self): return str(self)
    def get_ast(self): return { self.rule.name: self.ast }
    def set_item_at(self, i_term: int, item: Union[Lex, 'Partial']):
        term = self.rule.terms[i_term]
        if isinstance(item, Partial): item = item.get_ast()
        if isinstance(term, ZeroOrMore):
            self.matched[i_term].append(item)
        else:
            self.matched[i_term] = item
            self.i_term += 1
        if term.variable: 
            self.ast[term.variable] = self.matched[i_term]
    def is_matched(self) -> bool:
        return (self.n_matched() == len(self.rule.terms))
    def n_matched(self) -> int:
        n = 0
        for i, item in enumerate(self.matched):
            term = self.rule.terms[i]
            if term.is_optional() or self.matched[i]: n += 1
            else: break
        return n

# a stack of lists of partials, most recent last
class PartialStack:
    def __init__(self):
        self.levels = []
    def push(self, pms: List[Partial]):
        self.levels.append(pms)
    def replace(self, level, pms: List[Partial]):
        self.levels[level] = pms
    def __str__(self):
        out = "stack:\n"
        for i, level in enumerate(self.levels):
            out += f"{i:3}: "
            for pm in level:
                out += f"{pm}\n     "
            out += "\n"
        return out.strip() + "\n"
    def find_oldest_matched(self) -> Partial:
        for level in self.levels:
            for pm in level:
                if pm.is_matched(): return pm
        return None

#-------------------------------------------------------------------------------------------------
# Parser does the work

class Parser:
    def __init__(self, grammar_spec):
        self.grammar = Grammar(grammar_spec)

    # parse: read lexemes, return an AST
    def parse(self, code: str) -> Dict:

        ls = lexer(Source(code = code))
        log("parsing", ls)
        stack = PartialStack()

        for i, lex in enumerate(ls):
            log(f"\n{i}: {lex.type} '{lex.val}'")
            self.promote_matched(stack, -1)
            self.try_match(stack, lex)
            new_pms = self.find_new_pms(lex)
            stack.push(new_pms)
            self.try_reduce(stack)
            log(stack)

        result = stack.find_oldest_matched()
        return result.get_ast() if result else {}
    
    # try and reduce the stack: for each match, try and match it further back
    def try_reduce(self, stack: PartialStack):
        for level in range(len(stack.levels)-1, -1, -1): # most recent first
            matched_pms = [pm for pm in stack.levels[level] if pm.is_matched()]
            for pm in matched_pms:
                self.try_reduce_level(stack, level-1, pm)

    # given a matched pm, see if it can be added to partials at older levels
    def try_reduce_level(self, stack: PartialStack, start_level: int, matched_pm: Partial):
        if start_level < 0: return
        for level in range(start_level, -1, -1):
            for pm in stack.levels[level]:
                i_term = self.can_match(matched_pm.rule, pm)
                if i_term:
                    pm.set_item_at(i_term, matched_pm.get_ast())
            
    
    # run through the stack and try and match the lexeme to each partial
    def try_match(self, stack: PartialStack, item):
        for level in range(len(stack.levels)):
            pms = stack.levels[level]
            stack.levels[level] = self.try_match_pms(pms, item)

    # try and match a lexeme to a list of partials
    def try_match_pms(self, pms: List[Partial], item) -> List[Partial]:
        ppms = []
        for pm in pms:
            i_term = self.can_match(item, pm)
            if i_term:
                pm.set_item_at(i_term, item)
            ppms.append(pm)
        return ppms

    # promote any matched partials to a list of partials
    def promote_matched(self, stack: PartialStack, level: int):
        if level < 0: level += len(stack.levels)
        if level < 0 or level >= len(stack.levels): return
        ppms = []
        for pm in stack.levels[level]:
            if not pm.is_matched(): ppms.append(pm)
            else:
                new_pms = self.find_new_pms(pm)
                if len(new_pms) > 0:
                    ppms += new_pms
                else: ppms.append(pm)
        stack.replace(level, ppms)

    # find new partials for a lexeme or partial
    def find_new_pms(self, lex: Lex|Partial) -> List[Partial]:
        pms = []
        to_find = lex if isinstance(lex, Lex) else lex.rule
        rules = self.grammar.find_rules(to_find, 0)
        for rule in rules: pms.append(Partial(rule, lex))
        rules_1 = self.grammar.find_rules(to_find, 1)
        rules_1 = [rule for rule in rules_1 if rule.terms[0].is_optional()]
        for rule in rules_1: pms.append(Partial(rule, lex))
        return pms
    
    # show partials
    def show_pms(self, msg: str, pms: List[Partial]):
        log(msg)
        for pm in pms:
            log("   ", pm)

    def can_match(self, item: Union[Lex|Rule], pm) -> int: # if can match, return term index
        if pm.i_term >= len(pm.rule.terms): return None
        if self.can_match_at(pm.i_term, item, pm): return pm.i_term
        term = pm.rule.terms[pm.i_term]
        if pm.i_term < (len(pm.rule.terms) - 1) and \
            term.is_optional():
            if self.can_match_at(self.i_term + 1, item, pm):
                return pm.i_term + 1
        return None
    
    def can_match_at(self, i_term: int, item, pm) -> bool:
        rules = self.grammar.find_rules(item, i_term)
        return pm.rule in rules


#--------------------------------------------------------------------------------------------------

@this_is_the_test
def test_parser():
    log("test_parser")
    p = Parser(test_grammar_spec)
    log("--------------------------------------------------------------------------")
    test("parse_variable", p.parse("a"), """{'variable': {'name': a}}""")
    log("--------------------------------------------------------------------------")
    test("parse_postfix", p.parse("a!"), """{'postfix': {'expr': {'variable': {'name': a}}, 'operator': !}}""")
    log("--------------------------------------------------------------------------")
    test("parse_infix", p.parse("a + b"), """{'infix': {'left': {'variable': {'name': a}}, 'operator': +, 'right': {'variable': {'name': b}}}}""")
    log("--------------------------------------------------------------------------")
    test("parse_argument", p.parse("a=2"), """{'argument': {'value': {'constant': {'value': 2}}}}""")