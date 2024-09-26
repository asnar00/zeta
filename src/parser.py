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
        self.val = val      # either <type>, "keyword", or rule
        self.var = var      # variable name, or "" if none
        self.dec = dec      # decorator: "opt" or "list"
        self.sep = sep      # separator if list
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
    def separator(self) -> str: return f'"{self.sep}"'

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
        self.i_fixed_points = []    # i_term for each term that's a fixed-point
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
        self.compute_fixed_points()
    @log_disable
    def compute_initials(self):
        log("compute_initials()")
        for rule in self.rules:
            if len(rule.terms) == 0: continue
            term0 = rule.terms[0]
            initial = term0.keyword() or term0.type()
            if initial: rule.add_initial(initial)
        for rule in self.rules:
            log(f"  {rule.name}: {rule.initials}")
    def compute_terminators(self):
        log("compute_terminators")
        for rule in self.rules:
            for i_term, term in enumerate(rule.terms):
                term_rule = term.rule()
                if term_rule == None: continue
                next_initials = []
                if term.is_list(): next_initials.append(term.separator())
                if (i_term+1) < len(rule.terms):
                    next_term = rule.terms[i_term+1]
                    next_terminal = next_term.keyword() or next_term.type()
                    if next_terminal: next_initials.append(next_terminal)
                    next_term_rule = next_term.rule()
                    if next_term_rule: next_initials += next_term_rule.initials
                    if next_term.is_list() or next_term.is_optional():
                        if (i_term+2) < len(rule.terms):
                            next_next_term = rule.terms[i_term+2]
                            next_next_term_rule = next_next_term.rule()
                            if next_next_term_rule: next_initials += next_next_term_rule.initials
                else:
                    next_initials.append("<eof>")
                self.rule_dict[term_rule].add_terminators(next_initials)
        for rule in self.rules:
            log(f"  {rule.name}: {rule.terminators}")
    def compute_leaves(self):
        log("compute_leaves()")
        for rule in self.rules:
            rule.leaves = rule.compute_leaves()
        for rule in self.rules:
            log(f"{rule.name}: {[leaf.name for leaf in rule.leaves]}")
    def compute_fixed_points(self):
        log("compute_fixed_points")
        for rule in self.rules:
            rule.i_fixed_points = []
            for i_term, term in enumerate(rule.terms):
                if term.keyword() or term.type():
                    rule.i_fixed_points.append(i_term)
        for rule in self.rules:
            log(f"{rule.name}: {rule.i_fixed_points}")


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
    def __init__(self, ls: List[Lex], rule: Rule=None, children: List['Node'] = []):
        self.rule = rule
        self.ls = ls
        self.children = children
    def __str__(self): return f"{self.rule_name()+":" if self.rule else ""}{self.ls_str()}"
    def __repr__(self): return str(self)
    def ls_str(self): return '"' + " ".join(str(lex) for lex in self.ls) + '"'
    def rule_name(self): return self.rule.name if self.rule else ""
    def length(self) -> int: return len(self.ls)
    def pos(self) -> int: return self.ls[0].index
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
            nodes = self.get_layer(i_layer)
            pos = 0
            for node in nodes:
                while pos < node.pos(): out += self.pad("", 1); pos += 1
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
        if match_str.startswith("<"): return lex.type == match_str[1:-1]
        elif match_str.startswith('"'): return lex.val == match_str[1:-1]
        elif match_str == lex.val: return lex.val == match_str
    return False

# scans forward from position (i_lex) in ls to find the next fixed-point; returns index or -1
@log_disable
def next_fixed_point(ls: List[Lex], i_lex: int, match_strs: List[str]) -> int:
    log("next_fixed_point", i_lex, ls[i_lex:], match_strs)
    bracket_level = 0
    open = "([{:indent"
    close = ")]}:undent"
    while i_lex < len(ls):
        lex = ls[i_lex]
        if bracket_level == 0:
            if match_lex(lex, match_strs): return i_lex
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
    return -1

# tries to parse a list of terms in a lex-string, one by one
@log_indent
def try_parse_term_list(terms: List[Term], ls: List[Lex]) -> Node:
    log("try_parse_term_list", terms, ls)
    return None

# tries to parse a concrete rule by identifying fixed-points (terminal lexes) in terms
@log_indent
def try_parse_concrete(rule, ls):
    log("try_parse_concrete", rule.name, ls)
    i_lex = 0
    i_lex_fixed = []
    # scan forward in the lex string to find all fixed points, store their indices
    for i_fixed in range(0, len(rule.i_fixed_points)):
        i_term_fixed = rule.i_fixed_points[i_fixed]
        term_fixed = rule.terms[i_term_fixed]
        match_strs = [term_fixed.keyword() or term_fixed.type()]
        log("i_term", i_term_fixed, "=>", match_strs)
        i_lex_next = next_fixed_point(ls, i_lex, match_strs)
        log("i_lex_next", i_lex_next)
        if i_lex_next == -1: return None
        i_lex_fixed.append(i_lex_next)
        i_lex = i_lex_next + 1
    # now check that the following lex is one of the terminators for this rule
    log("terminators:", rule.terminators)
    i_lex_next = next_fixed_point(ls, i_lex, rule.terminators)
    log("i_lex_next", i_lex_next)
    if i_lex_next == -1: return None
    # we have to check whether there's any terms afer the last fixed-term
    if rule.i_fixed_points[-1] == len(rule.terms) - 1:
        if (i_lex_next - i_lex_fixed[-1]) > 1:
            log("extra terms after last fixed-point! returning None")
            return None
    i_lex_fixed.append(i_lex_next)
    # at this point, we know we're looking at this rule! any errors are further down
    node = Node(ls[:i_lex_next], rule)
    log("i_lex_fixed", i_lex_fixed, [ls[i] for i in i_lex_fixed[:-1]])
    log("i_fixed_points", rule.i_fixed_points)
    i_lex = 0
    i_term = 0
    for i in range(0, len(rule.i_fixed_points)):
        prev_ls = ls[i_lex:i_lex_fixed[i]]
        prev_terms = rule.terms[i_term:rule.i_fixed_points[i]]

        if len(prev_ls) > 0:
            log("  prev_ls", prev_ls)
            log("  prev_terms", prev_terms)
            node.children.append(try_parse_term_list(prev_terms, prev_ls))

        fixed_ls = ls[i_lex_fixed[i]:i_lex_fixed[i]+1]
        fixed_term = rule.terms[rule.i_fixed_points[i]]
        log("  fixed_ls", [str(lex) for lex in fixed_ls])
        log("  fixed_term", fixed_term)

        node.children.append(Node(fixed_ls))

        i_term = rule.i_fixed_points[i] + 1
        i_lex = i_lex_fixed[i] + 1

    end_ls = ls[i_lex_fixed[-1]:i_lex_next]
    end_terms = rule.terms[rule.i_fixed_points[-1]+1:]
    if len(end_ls) > 0:
        log("  end_ls", end_ls)
        log("  end_terms", end_terms)
        node.children.append(try_parse_term_list(end_terms, end_ls))

    log("node:", node)
    log("children:", node.children)
    return node

# tries to parse an abstract rule (eg. expression: one that has only children, and no terms)
@log_indent
def try_parse_abstract(rule: Rule, ls: List[Lex]) -> Node:
    log("try_parse_abstract", rule.name, ls)
    leaves = [leaf for leaf in rule.leaves if match_lex(ls[0], leaf.initials)]
    log("  leaves:", [leaf.name for leaf in leaves])
    nodes = [try_parse(leaf, ls) for leaf in leaves]
    nodes = [node for node in nodes if node]
    log("nodes:", nodes)
    if len(nodes) != 1: return None
    return nodes[0]

# tries to parse any rule in a lex-string
def try_parse(rule: Rule, ls: List[Lex]) -> Node:
    log("try_parse", rule.name, ls)
    if rule.is_abstract():      # eg. expression or operation
        return try_parse_abstract(rule, ls)
    else:
        return try_parse_concrete(rule, ls)
    
def parse(code: str) -> dict:
    ls = lexer(Source(code = code))
    node = try_parse(s_grammar.rule_dict["expression"], ls)
    return node

@this_is_the_test
def test_parser():
    log("test_parser")
    grammar = grammar_from_spec(test_grammar_spec)
    log(parse("f(a = x+(6-y))"))


"""
ok so. We want to think about this new approach we're talking about:
let's call it the "fixed-point" approach.

The idea is that within every rule term-sequence, there are "fixed points" that can be definitely identified.
You do an "next-outer" scan forward in the lex stream until you find one of those fixed points;
then try to match the interior.

You also, for each rule, compute a "termination-set".
eg. for variable: you see where it occurs in each rule, look at what comes after.
rule.terminators = []

if you see "expression" followed by ")", then ")" gets added to the terminators of all children of expression.
So the first thing is to go figure out all the children.

So for example: you would try "brackets" as follows:

    we find a "(" -> great, it's a bracket.
    scan forward to the next fixed-point (")") generating a lex-range.
    hand that to the parser for "expression". 

similarly "variable":

    you look for an identifier -> great, that's good.
    the terminator set would be anything that follows "expression" in the rules.

So: let's consider what the algorithm is:

    "try_parse(rule) at lex (i_lex)"

and we start with "expression" at lex 0

    expression has no terms, but has children; so let's get all children-that-don't-have-children.

    that's one thing: get_leaf_children. let's pre-compute that.

    a + (b + c)

    identifier followed by an operator 
"""