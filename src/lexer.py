# ᕦ(ツ)ᕤ
# lexer.py
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
    test("source_markdown", source.code, "a b c\nd e f")

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
    test("source_loc", source.location(0), "src/test/Hello.zero.md:22:1")

class SourceLocation:
    def __init__(self, file: str, i_line: int, i_col: int):
        self.file = file
        self.i_line = i_line
        self.i_col = i_col
    def __str__(self):
        if self.i_line==0: return ""
        return f"{self.file}:{self.i_line}:{self.i_col}"
    def __repr__(self):
        return self.__str__()
    def __lt__(self, other):
        return self.i_line < other.i_line or (self.i_line == other.i_line and self.i_col < other.i_col)

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
        return SourceLocation(path, original_line+1, column+1)

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
    test("lexer_number", lexer(Source(code="1 0x1a2b")))

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
        if self.source == None: return SourceLocation(None, None, None)
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
    specs = [ ('number', r'(?:0x[0-9a-fA-F]+|\d+(?:\.\d*)?)'),                           # integer or decimal number
                ('identifier', r'[A-Za-z_][A-Za-z0-9_$]*'),         # identifiers
                ('string', r'"(?:\\.|[^"\\])*"'),                   # string literals with support for escaped quotes
                ('operator', r'[-+=%^<>*/?!|&#\\]{1,2}'),           # operators, and double-operators
                ('punctuation', r'[(){}\[\].,;:]'),                 # punctuation
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
    def matches(self, vals: List[str]): return lex_matches(self.peek(), vals)
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

# returns true if a lex matches a set of terminals
def lex_matches(lex: Lex, matches: List[str]) -> bool:
    if lex==None or len(matches)==0: return False
    if matches[0][0]=='"': return f'"{lex.val}"' in matches
    elif matches[0][0]=='<': return f'<{lex.type}>' in matches
    else: return False