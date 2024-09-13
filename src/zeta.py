# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo
# zero to anything

from util import *
from typing import List, Dict
from collections import namedtuple
import re

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
        if self.type == "line" : return log_grey(val)
        else: return val
    def location(self):
        return self.source.location(self.pos)
    
# lexer: reads source and produces lexemes
def lexer(source: Source) -> List[Lex]:
    ls = naive_lexer(source)
    ls = insert_ws_indents(ls)
    ls = handle_braces(ls)
    ls = finalise_indents(ls)
    ls = filter_newlines(ls)
    return ls

# naive lexer just does a straight lex
def naive_lexer(source: Source) -> List[Lex]:
    ls = []
    specs = [ ('num', r'\d+(\.\d*)?'),                  # integer or decimal number
                ('id', r'[A-Za-z_][A-Za-z0-9_$]*'),     # identifiers
                ('str', r'"(?:\\.|[^"\\])*"'),          # string literals with support for escaped quotes
                ('op', r'[-+=%^<>/?|&]{1,2}'),          # operators, and double-operators
                ('punc', r'[(){}\[\],.;:]'),            # punctuation
                ('line', r'(^[ ]+)|(\n[ ]*)'),          # line-start plus 0 or more spaces
                ('skip', r'[ ]+')]                      # spaces
    patterns = '|'.join('(?P<%s>%s)' % pair for pair in specs)
    regex = re.compile(patterns)
    pos = 0
    while pos < len(source.code):
        m = regex.match(source.code, pos)
        if not m: raise Exception(f'unexpected character "{source.code[pos]}" at {log_short(source.code[pos:])}')
        if len(m.group()) == 0: raise Exception(f'empty match at {log_short(source.code[pos:])}')
        type = m.lastgroup
        val = m.group()
        if (type != 'skip'):
            if type == 'line':
                ls.append(Lex(source, pos, val.replace("\n", "↩︎\n").replace(" ", "_"), type))
            else:
                ls.append(Lex(source, pos, val, type))
        pos += len(val)
    return ls

# replaces all 'line' lexemes with the appropriate ws-indent/undent/newline lexemes
def insert_ws_indents(ls: List[Lex]) -> List[Lex]:
    ols = []
    last_indent = 0
    for lex in ls:
        if lex.type == "line":
            indent = len(lex.val) // 4
            if indent > last_indent:
                if len(ols) > 0 and ols[-1].val == ":": ols.pop()
                for i in range(indent - last_indent):
                    ols.append(Lex(lex.source, lex.pos, "ws-indent", "line"))
            elif indent < last_indent:
                for i in range(last_indent - indent):
                    ols.append(Lex(lex.source, lex.pos, "ws-undent", "line"))
            else:
                ols.append(Lex(lex.source, lex.pos, "ws-newline", "line"))
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
            ols.append(Lex(lex.source, lex.pos, ":indent", "line"))
            i += 1
            while i < len(ls) and ls[i].val.startswith("ws-") : i += 1
            i -= 1
        elif lex.val == "}":    # add an undent, remove all previous ws-undents
            while len(ols) > 0 and ols[-1].val.startswith("ws-"): ols.pop()
            ols.append(Lex(lex.source, lex.pos, ":undent", "line"))
        elif lex.val == ";":
            ols.append(Lex(lex.source, lex.pos, ":newline", "line"))
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
    
#--------------------------------------------------------------------------------------------------
# Grammar

# grammar atoms are the building blocks of a grammar
# each is implemented as a class, so they can be chained as in the example below
# grammar atoms are designed to support both parsing and printing (i.e. they're bidirectional)
# they're also designed to help with parser debugging, by reporting error locations in source and in the grammar

@this_is_a_test
def test_grammar_atoms():
    feature_decl = label("feature", sequence(
        keyword("feature"), set("name", identifier()),
        optional(sequence(keyword("extends"), set("parent", identifier())))))
    
    ast = parse(feature_decl, Source(code = "feature Hello extends Main"))
    test("feature_decl", ast, "{'_type': 'feature', 'name': Hello, 'parent': Main}")

    ast = parse(list(enum("a", "b")), Source(code = "a b c"))
    test("bad_list", ast, "{'_list': [a, b], '_error': expected one of ('a', 'b') at :1:5}")

    ast = parse(sequence(list(enum("a", "b")), keyword("end")), Source(code = "a b end"))
    test("good_list", ast, "{'_list': [a, b]}")

    function_decl = keyword("function")
    struct_decl = keyword("struct")
    variable_decl = keyword("variable")
    test_decl = keyword("test")
    component_decl = any(function_decl, struct_decl, variable_decl, test_decl)
    body_decl = sequence(keyword(":indent"), list(component_decl), keyword(":undent"))
    source = Source(code="""
{
    function
    struct
    variable
    test
}    
    """)
    ast = parse(body_decl, source)
    test("body_decl", ast, "{'_list': [{}, {}, {}, {}]}")

def parse(atom: 'Atom', source: Source) -> 'AST':
    ls = lexer(source)
    reader = Reader(ls)
    ast = atom.parse(reader)
    return ast

# Reader reads forward in the list of lexemes
class Reader:
    def __init__(self, ls: List[Lex]): self.ls = ls; self.pos = 0
    def peek(self): 
        self.skip()
        return self.ls[self.pos] if not self.eof() else None
    def advance(self): self.pos += 1
    def eof(self): return self.pos >= len(self.ls)
    def skip(self):
        while not self.eof() and self.ls[self.pos].val == ':newline': self.advance()
    def match(self, fn):
        if self.eof(): return None
        lex = self.peek()
        if fn(lex):
            self.advance()
            return lex
        return None
    def location(self):
        return self.peek().location() if not self.eof() else "eof"
    
# Error holds a message and a point in the source file
class Error:
    def __init__(self, expected, caller, reader):
        self.expected = expected; self.caller = caller; self.reader = reader; self.pos = reader.pos
    def __str__(self):
        return f"expected {self.expected} at {self.reader.location()}"
    def __repr__(self): return str(self)
    def is_later_than(self, other):
        return self.pos > other.pos

# returns true if error, false if not
def err(ast) -> bool:
    return isinstance(ast, Error)

# combines multiple errors into a single error
def combine_errors(errors: List[Error]) -> Error:
        # first find the latest one
        latest = errors[0]
        for error in errors:
            if error.is_later_than(latest):
                latest = error
        # now find all errors at the same point as latest
        same_point = [latest]
        for error in errors:
            if error.pos == latest.pos and error != latest:
                same_point.append(error)
        # now combine all the 'expecteds' into an "or" string
        expecteds = [error.expected for error in same_point]
        expected = " or ".join(expecteds)
        # now return a new error with the combined expecteds
        latest.expected = expected
        return latest

class Writer: pass

class AST(Dict):
    pass

# note: these are all lower-case class names, so you can use them in rule expressions
# but this is not a recommended practice or convention for general code
# ideally we should find a better way of doing this, but it's the most concise way for now
    
class Atom:
    def parse(self, reader: Reader): raise Exception(f"{self.__class__.__name__}.parse() not implemented")
    def print(self, writer: Writer, ast: AST): raise Exception(f"{self.__class__.__name__}.print() not implemented")

# adds "_type" : (name) to the start of the AST, labeling it as a node of a specific type
class label(Atom):
    def __init__(self, name, atom) : self.caller = caller(); self.name = name; self.atom = atom
    def parse(self, reader: Reader):
        ast = { "_type" : self.name }
        ast.update(self.atom.parse(reader))
        return ast

# matches a keyword, returns an empty AST
class keyword(Atom):
    def __init__(self, val): self.caller = caller(); self.val = val
    def parse(self, reader: Reader) -> AST:
        if reader.match(lambda lex: lex.val == self.val): return {}
        return Error(self.val, self.caller, reader)

# matches an identifier, returns the lexeme
class identifier(Atom):
    def __init__(self): self.caller = caller()
    def parse(self, reader: Reader):
        lex = reader.match(lambda lex: lex.type == "id")
        return lex if lex else Error("identifier", self.caller, reader)

# optionally matches something, returns an error if it happened after the current read position
class optional(Atom):
    def __init__(self, atom): self.caller = caller(); self.atom = atom
    def parse(self, reader: Reader) -> AST:
        reader.skip()
        pos = reader.pos
        ast = self.atom.parse(reader)
        return AST({}) if (err(ast) and ast.pos == pos) else ast
    
# sets a property of the AST to the result of parsing (atom)
class set(Atom):
    def __init__(self, name, atom): self.caller = caller(); self.name = name; self.atom = atom
    def parse(self, reader: Reader) -> AST:
        return AST({ self.name : self.atom.parse(reader) })

# matches a sequence of atoms, combines all results into a single AST node
class sequence(Atom):
    def __init__(self, *atoms): self.caller = caller(); self.atoms = atoms
    def parse(self, reader: Reader) -> AST:
        ast = AST({})
        for atom in self.atoms:
            sub_ast = atom.parse(reader)
            if err(sub_ast):
                if ('_error' in ast): return ast['_error']
                return sub_ast
            else:
                if '_error' in ast: del ast['_error']
                ast.update(sub_ast)
        return ast

# matches any of one or more atom types, returns the first one that succeeds
class any(Atom):
    def __init__(self, *atoms): self.caller = caller(); self.atoms = atoms
    def parse(self, reader: Reader) -> AST:
        errors = []
        for atom in self.atoms:
            pos = reader.pos
            ast = atom.parse(reader)
            if not err(ast): return ast
            errors.append(ast)
            reader.pos = pos
        return combine_errors(errors)

# matches a list of zero or more of the same atom, optionally separated by a string
class list(Atom):
    def __init__(self, atom, sep: str=None): self.caller = caller(); self.atom = atom; self.sep = sep
    def parse(self, reader: Reader) -> AST:
        items = []
        while not reader.eof():
            sub_ast = self.atom.parse(reader)
            if err(sub_ast):
                return { '_list': items, "_error": sub_ast }
            items.append(sub_ast)
            if self.sep and self.sep != ":newline":
                if not reader.match(lambda lex: lex.val == self.sep):
                    return { '_list': items, '_error': Error(f"'{self.sep}'", self.caller, reader)}
        return { '_list': items }

# matches one of a list of words, returns the lexeme
class enum(Atom):
    def __init__(self, *words): self.caller = caller(); self.words = words
    def parse(self, reader: Reader) -> AST:
        lex = reader.match(lambda lex: lex.val in self.words)
        return lex if lex else Error(f"one of {self.words}", self.caller, reader)

# matches forward until it finds one of (words), but outside braces/brackets
class upto(Atom):
    def __init__(self, *words): self.caller = caller(); self.words = words
    def parse(self, reader: Reader) -> AST:
        depth = 0
        out = []
        while True:
            if reader.eof(): return out
            lex = reader.peek()
            if depth == 0 and str(lex) in self.words: return out
            out.append(lex)
            if str(lex) in ["(", "[", ":indent"]: depth += 1
            elif str(lex) in [")", "]", ":undent"]: depth -= 1
            reader.advance()

def block(atom: Atom) -> Atom:
    return sequence(keyword(":indent"), atom, keyword(":undent"))

def brackets(atom: Atom) -> Atom:
    return sequence(keyword("("), atom, keyword(")"))

def indent() -> Atom:
    return keyword(":indent")

def undent() -> Atom:
    return keyword(":undent")

#--------------------------------------------------------------------------------------------------
# Zero grammar





#--------------------------------------------------------------------------------------------------
# main

def main():
    print("----------------------------------------------------------------")
    print("ᕦ(ツ)ᕤ zeta.py")
    test_run_all()

if __name__ == "__main__":
    main()
    print("done.")