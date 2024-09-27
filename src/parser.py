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
    def __init__(self, source: Source, pos: int, val: str, type: str, index: int=0):
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
    expression := ...
    constant < expression := ...
    number < constant := <number>
    string < constant := <string>
    variable < expression := <identifier>
    brackets < expression := "(" expr:expression ")"
    operation < expression := ...
    prefix < operation := operator:<operator> expr:expression
    infix < operation := left:expression operator:<operator> right:expression
    postfix < operation := expr:expression operator:<operator>
    function < expression := name:<identifier> "(" parameters:expression*, ")"
    """

# Term represents a rule term
class Term:
    def __init__(self, val: str, var: str="", dec: str="", sep: str=""):
        self.val = val          # either <type>, "keyword", or rule
        self.var = var          # variable name, or "" if none
        self.dec = dec          # decorator: "opt" or "list"
        self.sep = sep          # separator if list
    def __str__(self):
        out = self.val
        if self.dec == "list": out += f"*{self.sep}"
        elif self.dec == "opt": out += "?"
        if self.var: out = f"{self.var}:{out}"
        return out
    def __repr__(self): return str(self)
    def keyword(self) -> str|None: return self.val if self.val.startswith('"') else None
    def type(self) -> str|None: return self.val if self.val.startswith("<") else None
    def rule(self) -> str|None: return self.val if (not self.keyword() and not self.type()) else None
    def is_optional(self) -> bool: return self.dec == "opt"
    def is_list(self) -> bool: return self.dec == "list"
    def separator(self) -> str: return f'"{self.sep}"' if self.sep else None
    def is_fixed_point(self) -> bool: return self.keyword() or self.type()

# Rule is a grammar rule
class Rule:
    def __init__(self, name: str, parent: 'Rule', terms: List[Term]):
        self.name = name
        self.parent = parent
        self.children = []
        if self.parent: self.parent.children.append(self)
        self.terms = terms
        self.initials = []          # all terminals (keyword/type) that might start this
        self.terminators = []       # all terminals (keyword/type) that might follow this
        self.leaves = []            # children that don't have children (recursed down)
    def __str__(self):
        out = self.name
        if self.parent: out += f" < {self.parent.name}"
        out += " := "
        out += " ".join([str(t) for t in self.terms]) if len(self.terms)>0 else "..."
        return out
    def __repr__(self): return str(self)
    def is_abstract(self): return len(self.terms) == 0
    def add_initial(self, initial): 
        self.initials.append(initial)
        if self.parent: self.parent.add_initial(initial)
    def add_terminators(self, terminators):
        for t in terminators:
            if not t in self.terminators: self.terminators.append(t)
        for c in self.children:
            c.add_terminators(terminators)
    def compute_leaves(self) -> List['Rule']:
        if len(self.children) == 0: return [self]
        leaves = []
        for c in self.children: leaves += c.compute_leaves()
        return leaves
    def get_initials(self):
        out = []
        for initial in self.initials:
            for key in initial.keys():
                out.append(key)
        return out
    def get_terminators(self):
        out = []
        for terminator in self.terminators:
            for key in terminator.keys():
                out.append(key)
        return out
    

# singleton grammar just because it makes things easier (see "context problem")
s_grammar = None 

# Grammar is a list of rules
class Grammar:
    def __init__(self, rules: List[Rule]):
        self.rules = rules
        # map rule name to rule
        self.rule_dict = {rule.name: rule for rule in self.rules}
        self.compute_meta_stuff()
        global s_grammar
        s_grammar = self
    def __str__(self):
        return "\n".join([str(r) for r in self.rules])
    def __repr__(self): return str(self)
    @log_disable
    def compute_meta_stuff(self):
        self.compute_initials()
        self.compute_terminators()
        self.compute_leaves()
    def compute_initials(self):
        log("compute_initials()")
        for rule in self.rules:
            if len(rule.terms) == 0: continue
            term0 = rule.terms[0]
            initial = term0.keyword() or term0.type()
            if initial: rule.add_initial({ initial: (rule, 0) })
        for rule in self.rules:
            log(f"  {rule.name}: {rule.initials}")
    def compute_terminators(self):
        log("compute_terminators")
        for rule in self.rules:
            for i_term, term in enumerate(rule.terms):
                term_rule = term.rule()
                if term_rule == None: continue
                terminators = []        # array of { match: (rule, i_term) }
                if term.is_list(): terminators.append({ term.separator() : (rule, i_term) })
                if (i_term+1) < len(rule.terms):
                    next_term = rule.terms[i_term+1]
                    next_terminal = next_term.keyword() or next_term.type()
                    if next_terminal: terminators.append({ next_terminal : (rule, i_term+1) })
                    next_term_rule = next_term.rule()
                    if next_term_rule: terminators += next_term_rule.initials
                    if next_term.is_list() or next_term.is_optional():
                        if (i_term+2) < len(rule.terms):
                            next_next_term = rule.terms[i_term+2]
                            next_next_term_rule = next_next_term.rule()
                            if next_next_term_rule: terminators += next_next_term_rule.initials
                else:
                    terminators.append({ "<eof>" : (rule, len(rule.terms))})
                self.rule_dict[term_rule].add_terminators(terminators)
        for rule in self.rules:
            log(f"  {rule.name}: {rule.terminators}")
    def compute_leaves(self):
        log("compute_leaves()")
        for rule in self.rules:
            rule.leaves = rule.compute_leaves()
        for rule in self.rules:
            log(f"{rule.name}: {[leaf.name for leaf in rule.leaves]}")

# OK, everything is super lightweight, let's keep it that way.
def grammar_from_spec(spec: str) -> Grammar:
    rule_dict = {}
    lines = spec.strip().split("\n")
    rules = []
    for line in lines:
        parts = line.split(" := ")
        lhs = parts[0].strip()
        rhs = parts[1].strip()
        lhs_parts = lhs.split(" < ")
        rule_name = lhs_parts[0].strip()
        parent_name = lhs_parts[1].strip() if len(lhs_parts) > 1 else None
        terms = []
        if rhs != "...":
            term_strs = [t.strip() for t in rhs.split(" ")]
            for term_str in term_strs:
                term_parts = term_str.split(":")
                var = term_parts[0] if len(term_parts) == 2 else ""
                val = term_parts[-1]
                dec = ""
                sep = ""
                if '*' in val:
                    ic = val.find('*')
                    sep = val[ic+1:].replace("*", "")
                    val = val[:ic]
                    dec = "list"
                elif val.endswith("?"):
                    val = val[:-1]
                    dec = "opt"
                term = Term(val, var, dec, sep)
                terms.append(term)
        parent = rule_dict[parent_name] if parent_name else None
        rule = Rule(rule_name, parent, terms)
        rules.append(rule)
        rule_dict[rule_name] = rule
    grammar = Grammar(rules)
    return grammar

@this_is_a_test
def test_parser():
    grammar = grammar_from_spec(test_grammar_spec)
    test("grammar", grammar, """
expression := ...
constant < expression := ...
number < constant := <number>
string < constant := <string>
variable < expression := <identifier>
brackets < expression := "(" expr:expression ")"
operation < expression := ...
prefix < operation := operator:<operator> expr:expression
infix < operation := left:expression operator:<operator> right:expression
postfix < operation := expr:expression operator:<operator>
function < expression := name:<identifier> "(" parameters:expression*, ")"
         """)
    
#--------------------------------------------------------------------------------------------------
# Parser baby

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
    def pos(self) -> int: return self.ls[0].index if len(self.ls) > 0 else None
    def count_layers(self, i_layer:int = 1) -> int:
        if len(self.children) == 0: return i_layer
        return max([c.count_layers(i_layer+1) for c in self.children])
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

@log_disable
# returns True if the lex matches any of the match-strings (<type>, "keyword", rule)
def match_lex(lex: Lex, match_strs: List[str]) -> bool:
    log("match_lex", lex, lex.type, match_strs)
    for match_str in match_strs:
        if match_str.startswith("<") and lex.type == match_str[1:-1]: return True
        elif match_str.startswith('"') and lex.val == match_str[1:-1]: return True
        elif match_str == lex.val : return True
    return False

# scans forward from position (i_lex) in ls to find the next fixed-point; returns index or None
@log_disable
def next_fixed_point(ls: List[Lex], i_lex: int, match_strs: List[str]) -> int:
    log("next_fixed_point", i_lex, ls[i_lex:], match_strs)
    bracket_level = 0
    open = "([{:indent"
    close = ")]}:undent"
    while i_lex < len(ls):
        lex = ls[i_lex]
        if bracket_level == 0:
            log("trying to match", lex, "against", match_strs)
            if match_lex(lex, match_strs): 
                log("  succeeded")
                return i_lex
            else: log("  failed!")
        if lex.val in open: 
            bracket_level += 1
            log( " open bracket")
        elif lex.val in close: 
            bracket_level -= 1
            log(" close bracket")
        i_lex += 1
    if "<eof>" in match_strs: 
        log("matched <eof>")
        return i_lex
    return None

# tries to parse a term (possibly list or opt) in a lex-string, returns it as a single node
@log_indent
def parse_term(term: Term, ls: List[Lex]) -> Node:
    log("parse_term:", term, "<=", ls)
    result = None
    if term.is_list(): result= parse_list_term(term, ls)
    elif term.is_optional(): result= parse_term_opt(term, ls)
    else: result= parse_rule(s_grammar.rule_dict[term.rule()], ls)
    log(" returning:", result)
    return result

# tries to parse a list term (a single term that matches zero or more times)
# returns a single node with each matched one in it (possibly zero)
@log_indent
def parse_list_term(term: Term, ls: List[Lex]) -> Node:
    log("parse_list_term:", term, "<=", ls)
    log("separator:", term.separator())
    nodes = []
    i_lex = 0
    terminators = [term.separator()]
    while i_lex < len(ls):
        i_lex_next = next_fixed_point(ls, i_lex, terminators)
        if i_lex_next == None:
            i_lex_next = len(ls)
        node = parse_rule(s_grammar.rule_dict[term.rule()], ls[i_lex:i_lex_next])
        nodes.append(node)
        if i_lex_next < len(ls) and match_lex(ls[i_lex_next], [term.separator()]):
            nodes.append(Node(ls[i_lex_next:i_lex_next+1], None))
        i_lex = i_lex_next + 1
    result= Node(ls[:i_lex], None, nodes, "list")
    log(" returning:", result)
    return result

# tries to parse a list of terms in a lex-string, one by one
@log_disable
def parse_terms(terms: List[Term], ls: List[Lex]) -> List[Node]:
    log("parse_terms", terms, "<=", ls)
    i_lex = 0
    nodes = []
    for term in terms:
        node = parse_term(term, ls[i_lex:])
        if node == None: raise Exception("parse_term returned None at {ls[i_lex].location()}")
        i_lex += node.length()
        nodes.append(node)
        if i_lex >= len(ls): break # we're done :-)
    log(" returning:", nodes)
    return nodes


# returns list of lexeme-indices if we can match all fixed-points and terminator of a rule in a lex-string
@log_disable
def can_match_rule(rule: Rule, ls: List[Lex]) -> List[int]:
    i_lex = 0
    fixed_lexes = []
    for term in rule.terms:
        if term.is_fixed_point():
            i_lex_next = next_fixed_point(ls, i_lex, [term.val])
            if i_lex_next == None: 
                log(" returning None: failed to find fixed-point")
                return None
            fixed_lexes.append(i_lex_next)
            i_lex = i_lex_next + 1
    terminators = rule.get_terminators()
    i_lex_next = next_fixed_point(ls, i_lex, terminators)
    if i_lex_next == None: return None
    if rule.terms[-1].is_fixed_point() and i_lex_next > i_lex+1: 
        log(" returning None: extra lexes found after end!")
        return None
    fixed_lexes.append(i_lex_next)
    return fixed_lexes

# tries to parse a concrete rule by identifying fixed-points (terminal lexes) in terms
# optionally, pass in an already-matched first-node
@log_indent
def parse_concrete(rule: Rule, ls: List[Lex], first_node:Node = None) -> Node:
    log("parse_concrete:", rule.name, "<=", ls)

    i_fixed_lexes = can_match_rule(rule, ls)
    if i_fixed_lexes == None: 
        log(" returning None: can't match fixed points")
        return None
    
    log("matched fixed points for rule:", rule.name, i_fixed_lexes)
    children = []
    i_lex = 0
    for i_term, term in enumerate(rule.terms):
        if term.is_fixed_point():
            lex_node = Node(ls[i_fixed_lexes[0]:i_fixed_lexes[0]+1], None) # pure lex node
            #log("adding lex_node:", lex_node)
            children.append(lex_node)
            i_lex = i_fixed_lexes[0] + 1
            i_fixed_lexes = i_fixed_lexes[1:]       # step to the next lex
        else:
            lex_range = ls[i_lex:i_fixed_lexes[0]]
            i_lex = i_fixed_lexes[0]
            log("try and match term", term, "in", lex_range)
            if len(lex_range) == 0 and first_node:
                node = first_node
            else:
                node = parse_term(term, lex_range)
            if node:
                #log("adding parsed node:", node)
                children.append(node)
            log("matched term:", node)

    result= Node(ls[0:i_lex], rule, children)
    if first_node: result.ls = first_node.ls + result.ls
    log(" returning:", result)
    return result

# tries to parse an abstract rule (eg. expression: one that has only children, and no terms)
@log_indent
def parse_abstract(rule: Rule, ls: List[Lex]) -> Node:
    log("parse_abstract:", rule.name, "<=", ls)
    leaves = [leaf for leaf in rule.leaves if match_lex(ls[0], leaf.get_initials())]
    log("  matching leaves:", [leaf.name for leaf in leaves])
    node = None
    for leaf in leaves:
        node = parse_rule(leaf, ls)
        if node: break
    if node == None:
        log(" returning None: couldn't find a leaf that matched")
        return None
    if node.length() == len(ls):
        log(" returning:", node)
        return node
    
    return parse_remaining(rule, ls, node.length(), node)

@log_disable
def parse_remaining(rule: Rule, ls: List[Lex], i_lex_start: int, first_node: Node) -> Node:
    log(f"parse_remaining: {rule.name}: ({first_node}) <= {ls[i_lex_start:]}")

    # find all rule/terms that might follow the one we just matched
    terminators = [t for t in rule.terminators if match_lex(ls[i_lex_start], list(t.keys()))]
    for t in terminators:                           # this is ugly, make it nicer
        key = list(t.keys())[0]                     # eg. <operator>
        val = list(t.values())[0]                   # eg. (infix, 1)
        rule = val[0]                               # eg. infix
        i_term = val[1]                             # eg. 1
        log(" ", key, f"{rule.name}:{i_term}")
        higher_node = parse_concrete(rule, ls[i_lex_start:], first_node)    # pass in the extra thingy
        if higher_node != None:
            if higher_node.length() < len(ls):
                return parse_remaining(rule, ls, higher_node.length(), higher_node)
            log(" returning:", higher_node)
            return higher_node
    log(" returning None")
    return None

# tries to parse any rule in a lex-string
@log_indent
def parse_rule(rule: Rule, ls: List[Lex]) -> Node:
    log("parse_rule:", rule.name, "<=", ls)
    result = parse_abstract(rule, ls) if rule.is_abstract() else parse_concrete(rule, ls)
    log(" returning:", result)
    return result

@log_indent
def parse(code: str) -> dict:
    ls = lexer(Source(code = code))
    node = parse_rule(s_grammar.rule_dict["expression"], ls)
    return node

@this_is_the_test
def test_parser():
    log("test_parser")
    grammar = grammar_from_spec(test_grammar_spec)
    result = parse("f(a, c = a + 1)")
    log()
    result.display()

