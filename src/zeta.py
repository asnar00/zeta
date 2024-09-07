# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo

from typing import List, Tuple, Union
import inspect
import os
import re

#------------------------------------------------------------------------------
# kenny loggings

# set if logging is enabled
s_log: bool = False

# log: prints stuff if logging is enabled
def log(*args):
    if s_log:
        print(*args)

# log_enable is a decorator that turns on logging within a function
def log_enable(fn):
    def wrapper(*args, **kwargs):
        global s_log
        old_log = s_log
        s_log = True
        result = fn(*args, **kwargs)
        s_log = old_log
        return result
    return wrapper

# log_disable is a decorator that turns off logging within a function
def log_disable(fn):
    def wrapper(*args, **kwargs):
        global s_log
        old_log = s_log
        s_log = False
        result = fn(*args, **kwargs)
        s_log = old_log
        return result
    return wrapper

# clear the log- handy because we're constantly re-running
def log_clear():
    os.system('clear')  # For Linux/macOS

# returns a grey-coloured foreground, black background
def log_grey(str) -> str:
    return f'\033[30;1m{str}\033[0m'

# returns a grey-coloured background, white foreground
def log_grey_background(str) -> str:
    return f'\033[47;30m{str}\033[0m'

# strips all colour-related codes out of a string
def log_strip(str) -> str:
    str = re.sub(r'\033\[[0-9;]*m', '', str)
    # also strip out :number:, replace with :...:
    str = re.sub(r':\d+:', ':..:', str)
    return str

# log_disclose shows CRs and tabs and spaces within a string
def log_disclose(str) -> str:
    return str.replace('\n', '↩︎\n').replace('\t', '▶︎').replace(' ', '_')

# log_assert checks if two strings are equal, and prints a message if they're not
def log_assert(name, a, b: str):
    sa = log_strip(str(a)).rstrip()
    sb = log_strip(b).rstrip()
    if sb.startswith("\n"): sb = sb[1:]
    ctx = caller()
    ctx_str = f"{ctx[0]}:{ctx[1]}"
    if sa == sb:
        print(f"{log_grey(ctx_str)} {name}: passed")
    else:
        print(f"{log_grey(ctx_str)} {name}: failed")
        print("expected:")
        print(sb)
        print("got:")
        print(sa)

#------------------------------------------------------------------------------
# phil call-ins

s_cwd = os.getcwd() + "/src/"

# returns the (file, line) of the function that called our caller
def caller():
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to caller(), so...
    file = frame.filename.replace(s_cwd, '')    # dunno if we totally need this but 
    return (file, frame.lineno)                 # easier to process this way

# same, but returns a string
def caller_string() -> str:
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to caller(), so...
    file = frame.filename.replace(s_cwd, '')
    return f"{file}:{frame.lineno}"
#------------------------------------------------------------------------------
# file system of a down

# read a file
def readFile(path: str) -> str:
    with open(path, "r") as file:
        return file.read()

#------------------------------------------------------------------------------
# return to the source

# Source is a file containing source code
class Source:
    def __init__(self, path: str):
        self.path = path
        self.text = readFile(path)

    # convert a character index to a file/line/column
    def loc(self, pos: int) -> 'SourceLoc':
        lineno = 1
        startLine = 0
        for i in range(0, pos):
            if self.text[i] == '\n':
                lineno += 1
                startLine = pos
        column = (pos - startLine) + 1
        return SourceLoc(self, lineno, column)

# SourceLoc is a location in a source file (file/line/column), all 1-based
class SourceLoc:
    def __init__(self, source: Source, lineno: int, column: int=0):
        self.source = source
        self.lineno = lineno
        self.column = column

    # we need this so we can just print them
    def __str__(self):
        return f'{self.source.path}:{self.lineno}:{self.column}'
    
    # and this
    def __repr__(self):
        return self.__str__()

#------------------------------------------------------------------------------
# lexy's midnight runners

# Lex is a lexeme: points at Source, has a start/end index, stores the value
# this lets (value) be different to the source if necessary, eg for smart indents and so on
class Lex:
    def __init__(self, source: Source, i: int, j: int, val: str=None):
        self.source = source
        self.i = i
        self.j = j
        self.val = val if val else source.text[i:j]

    def set_end(self, j:int):
        self.j = j
        self.val = self.source.text[self.i:self.j]

    def slice(self, i: int, j: int, val: str=None):
        result= Lex(self.source, i + self.i, j + self.i, val)
        return result

    def __str__(self):
        return self.val
    
    def __repr__(self):
        return self.__str__()
    
    def location(self):
        line = 1
        iCr = 0
        for i in range(0, self.i):
            if self.source.text[i] == '\n': 
                iCr = i
                line += 1
        return SourceLoc(self.source, line, self.i - iCr)
    
# LexStr is a string of lexemes
class LexStr:
    def __init__(self, lexemes: List[Lex] = []):
        self.lexemes = lexemes
    def push(self, lex: Lex):
        self.lexemes.append(lex)
    def __str__(self):
        out = ""
        indent = 0
        for lex in self.lexemes:
            out += str(lex) + " "
            if lex.val == "{indent}": indent += 1
            if lex.val == "{undent}": indent -= 1
            if lex.val in ["{newline}", "{indent}", "{undent}"]:
                out += "\n" + "  " * indent
        return out
    def __repr__(self):
        return self.__str__()
    def __len__(self):
        return len(self.lexemes)
    def __getitem__(self, index):
        return self.lexemes[index]
    def __setitem__(self, index, value):
        self.lexemes[index] = value
    def __iter__(self):
        return iter(self.lexemes)
    def __iadd__(self, other: Union['Lex', 'LexStr']):
        if isinstance(other, Lex):
            # Append a single Lex object
            self.lexemes.append(other)
        elif isinstance(other, LexStr):
            # Extend with another LexStr's lexemes
            self.lexemes.extend(other.lexemes)
        else:
            raise TypeError(f"Unsupported operand type for +=: {type(other)}")
        return self  # Return self to support chaining of operations
    
# pulls out all code snippets from a source file; each lex is a snippet
def extract_code(source: Source) -> LexStr:
    out = LexStr()
    lines = source.text.split('\n')
    if len(lines) > 0 and lines[-1]=='' : lines = lines[:-1]
    iChar = 0
    for line in lines:
        jChar = iChar + len(line)
        if line.startswith("    "):
            if len(out) > 0:
                if out[-1].j == (iChar-1):
                    out[-1].set_end(jChar)
                else:
                    out.push(Lex(source, iChar, iChar + len(line)))
            else:
                out.push(Lex(source, iChar, iChar + len(line)))
        iChar += len(line) + 1
    return out

# is_alphanum returns true for letters and "_"
def is_alphanum(c: str) -> bool:
    return c.isalpha() or c == "_"

# is_alphanum_or_digit returns true for letters, digits, and "_"
def is_alphanum_or_digit(c: str) -> bool:
    return c.isalnum() or c in "_$"

# returns true if (str) is an identifier
def is_id(val: str):
    return val[0].isalpha() or val[0] == '_'

# lexer: takes a source and returns a list of lexemes
class Lexer:
    def __init__(self, source: Source):
        self.source = source

    def lex(self):
        code = extract_code(self.source)
        out = LexStr([])
        self.last_indent = None
        for i in range(0, len(code)):
            out += self.lex_substring(code[i])
        return out
    
    @log_disable
    def lex_substring(self, string: Lex) -> LexStr:
        log(string)
        self.out = LexStr([])
        self.string = string
        self.ls = str(string)
        self.i = 0
        self.ops = "!@#$%^&*-+=.<>?/~|"
        punct = "()[],;:"
        while self.i < len(self.ls):
            self.line_start()
            self.find_eol()
            # process actual characters up until end of line
            while self.i < self.iEnd:
                c = self.ls[self.i]
                cn = self.ls[self.i+1] if self.i+1 < self.iEnd else ''
                # note: each case is responsible for advancing "self.i" correctly!!
                if c == ' ': self.whitespace()
                elif c in self.ops: self.operator(cn)
                elif is_alphanum(c): self.alphanum()
                elif c.isdigit(): self.number()
                elif c in punct: self.punctuation()
                elif c == '"': self.string_literal()
                elif c in "{}": self.braces(c)
                else:
                    log(f"lexer error: unhandled character '{c}'")
                    exit(0)
            self.i = self.iEnd + 1
        # now clean up all the indents and undents
        for lex in self.out:
            if lex.val.startswith("{indent-"):
                lex.val = "{indent}"
            elif lex.val.startswith("{undent-"):
                lex.val = "{undent}"
        return self.out
    
    # count and skip whitespace, handle line ending
    def line_start(self): 
        j = self.i
        while j < len(self.ls) and self.ls[j] == ' ': j += 1
        indent = j - self.i
        log("    start:", indent, "spaces")
        if self.last_indent != None:
            if indent > self.last_indent:
                # sig-whitespace indent: skip if last was indent-brace
                if len(self.out) ==0 or self.out[-1].val != "{indent-brace}":
                    # if last was ":", remove it
                    if len(self.out) > 0 and self.out[-1].val == ":":
                        self.out.lexemes = self.out.lexemes[:-1]
                        log("--- removing previous ':'")
                    self.out += self.string.slice(self.i-1, j, '{indent-sig}')
                    log("{indent-sigwhite}")
            elif indent < self.last_indent:
                # sig-whitespace undent
                log("{undent-sig}")
                self.out += self.string.slice(self.i-1, j, '{undent-sig}')
            elif indent == self.last_indent:
                # sig-whitespace: no change, give us a newline
                log("{newline}")
                self.out += self.string.slice(self.i-1, j, '{newline}')
        self.last_indent = indent
        self.i = j # now positioned on first non-whitespace

    def find_eol(self):
        self.iEnd = self.ls.find('\n', self.i)
        if self.iEnd == -1: self.iEnd = len(self.ls)

    def match(self, fn, n_max=None):
        j_max = (n_max + self.i) if n_max else self.iEnd
        j = self.i+1
        while j < j_max and fn(self.ls[j], self.ls[j-1] if j>0 else ''): j += 1
        return j
    
    def push(self, j, tag, val=None):
        self.out += self.string.slice(self.i, j, val)
        self.i = j
        if tag: log("{punctuation}", str(self.out[-1]))

    def whitespace(self):
        log("{whitespace}")
        self.i += 1

    def operator(self, cn):
        j = self.i + 1
        if cn != '\n' and cn in self.ops: j = self.i + 2
        self.push(j, "{op}")

    def alphanum(self):
        j = self.match(lambda c, cp: is_alphanum_or_digit(c))
        self.push(j, "{alphanum}")

    def number(self):
        j = self.match(lambda c, cp : c.isdigit())
        self.push(j, "{number}")

    def punctuation(self):
        self.push(self.i + 1, "{punctuation}")

    def string_literal(self):
        j = self.match(lambda c, cp: c != '"' or cp == "\\")
        self.push(j+1, "{string}")

    def braces(self, c):
        if c == "{":
            self.push(self.i+1, "{indent-brace}", "{indent-brace}")
        elif c == "}":
            if len(self.out) > 0 and self.out[-1].val != "{undent-sig}":
                self.out += self.string.slice(self.i, self.i+1, "{undent-brace}")
            else:
                self.out[-1].val = "{undent-brace}"
                log(" --replaced last undent with undent-brace")
            self.i = self.i+1

def test_lexer():
    source = Source("src/test/Hello.zero.md")
    lexer = Lexer(source)
    ls = lexer.lex()
    log_assert("lexer", ls, """
feature Hello extends Main {indent} 
  > hello ( ) {newline} 
  => "hello world" {newline} 
  > hello ( ) {newline} 
  on ( out$ : string ) << hello ( ) {indent} 
    out$ << "hello world" {undent} 
  on ( string out$ ) << hello ( ) {indent} 
    out$ << "hello world" ; {undent} 
  {newline} 
  on ( out$ : string ) << hello ( ) {indent} 
    out$ << "hello world" ; {undent} 
  {newline} 
  on ( out$ : string ) << hello ( ) {indent} 
    out$ << "hello world" {undent} 
  on ( string out$ ) << hello ( ) {indent} 
    out$ << "hello world"
""")

#------------------------------------------------------------------------------
# parser atoms

# matches a specific keyword
def keyword(word: str)-> dict:
    return {'fn': 'keyword', 'value': word, 'caller': caller()}

# matches an indent
def indent()-> dict:
    return {'fn': 'indent', 'caller': caller()}

# matches an undent
def undent()-> dict:
    return {'fn': 'undent', 'caller': caller()}

# matches a newline (no indent change)
def newline()-> dict:
    return {'fn': 'newline', 'caller': caller()}

# matches an alpha-numeric identifier
def identifier()-> dict:
    return {'fn': 'identifier', 'caller': caller()}

# labels the result of a rule as being of a certain type (eg "feature", "function")
def label(s: str, rule: dict)-> dict:
    return {'fn': 'label', 'label': s, 'rule': rule, 'caller': caller()}

# sets a property in the output AST to the result of the rule
def set(name: str, rule: dict)-> dict:
    return {'fn': 'set', 'name': name, 'rule': rule, 'caller': caller()}

# runs each rule in sequence, collects all results into a single AST node
def sequence(*rules: List[dict])-> dict:
    return {'fn': 'sequence', 'rules': rules, 'caller': caller()}

# optionally runs the rule, but succeeds even if it doesn't match
def optional(rule: dict)-> dict:
    return {'fn': 'optional', 'rule': rule, 'caller': caller()}

# matches one of the list of words (like keyword)
def enum(*words: List[str])-> dict:
    return {'fn': 'enum', 'words': words, 'caller': caller()}

# matches any of the sub-rules, or returns the last error
def any(*rules: List[dict])-> dict:
    return {'fn': 'any', 'rules': rules, 'caller': caller()}

# matches a list of the same rule, separated and terminated
def list(rule: dict, sep: str=None, term: str=None)-> dict:
    return {'fn': 'list', 'rule': rule, 'sep': sep, 'term': term, 'caller': caller()}

# matches an indented block of stuff
def block(rule: dict)-> dict:
    return {'fn': 'block', 'rule': rule, 'caller': caller()}

# grabs all lexemes up to one of the terminator strings, skipping brackets and quotes
def upto(*chars: List[str])-> dict:
    return {'fn': 'upto', 'chars': chars, 'caller': caller()}

# matches either a thing, or the thing in brackets
def maybe_bracketed(rule: dict)-> dict:
    return {'fn': 'maybe_bracketed', 'rule': rule, 'caller': caller()}

#------------------------------------------------------------------------------
# print a parser rule in a nice way, with links back to source code
# for debugging parser rules in a sensible, streamlined way

# print a parser rule as a nicely formatted, readable string matching the source code
def rule_as_string(rule):
    out = rule_as_string_rec(rule)
    file = rule['caller'][0]
    padding = len(file) + 6
    lines = out.split('**')
    last_lineno = 0
    result = " "*padding
    for i, line in enumerate(lines):
        ic = line.find(":")
        lineno = 0
        indent = 0
        if ic >= 0:
            start = line[:ic]
            parts = start.split("/")
            lineno = int(parts[0])
            indent = int(parts[1])
            line = line[ic+1:]
        if lineno > last_lineno:
            file_line = log_grey(f"{file}:{lineno:3}: ")
            result += f"\n{file_line}" + "    " * indent
        last_lineno = lineno
        result += line
    return result

# recursive routine that prints a rule as a string, with source line numbers embedded in it
def rule_as_string_rec(rule, indent: int=0, line: int=0) -> str:
    if isinstance(rule, str): return rule
    if 'fn' in rule and rule['fn'] == 'label':
        return f'{rule["label"]} = {rule_as_string_rec(rule["rule"], indent, line)}'
    out = ''
    rule_line = rule['caller'][1] if 'caller' in rule else line
    out += f'**{rule_line}/{indent}:'
    for key, val in rule.items():
        if key == 'fn':
            out += f'{val}('
        elif key != 'caller': 
            if isinstance(val, str): 
                out += f"'{val}', "
            elif isinstance(val, Tuple):
                for v in val: 
                    sub = maybe_rule_as_string(v, indent+1, line)
                    out += f'{sub}, '
            elif isinstance(val, dict): 
                sub = maybe_rule_as_string(val, indent+1, line)
                out += f'{sub}, '
    if out.endswith(", ") : out = out[:-2]
    out += ')'
    return out

# if the rule is labeled, just return the name, otherwise dive into it
def maybe_rule_as_string(rule, indent: int=0, line: int=0) -> str:
    if isinstance(rule, dict) and rule['fn'] == 'label':
        return rule['label'] + "()"
    return rule_as_string_rec(rule, indent)

def print_newline(indent: int) -> str:
    return '\n' + '  ' * indent

#------------------------------------------------------------------------------
# parse rules for zero

class Language: pass

class Zero(Language):
    # feature (name) [extends (parent)] { list(function | struct | variable) }
    def feature(self) -> dict:
        return label('feature', sequence(
                    keyword('feature'), set('name', identifier()),
                    optional(sequence(keyword('extends'), set('parent', identifier()))),
                    block(list(any(self.function(), self.struct(), self.variable())))))

    # (on/replaceafter/before) (result) (name) (parameters) { function_body }
    def function(self) -> dict:
        return label('function', sequence(
            set('modifier', enum('on', 'replace', 'after', 'before')),
            set('result', maybe_bracketed(self.name_type())),
            set('assign_op', enum("=", "<<")), 
            set('name', identifier()),
            keyword('('), set('parameters', list(self.variable(), sep=',', term=')')), keyword(')'),
            keyword('{'), set('body', self.function_body()), keyword('}')))

    # stand-in for now
    def function_body(self) -> dict:
        return {'fn': 'function_body'}
        
    # struct (name) { list(variable) }
    def struct(self) -> dict:
        return label('struct', sequence(
                    keyword('struct'), set('name', identifier()),
                    block(list(self.variable))))

    # ((name type) | (type:name)) [= value]
    def variable(self) -> dict:
        return label('variable', sequence(
                    self.name_type(),
                    optional(sequence(keyword('='), set('value', self.constant())))))

    def name_type(self) -> dict:
        return any(
                sequence(set('name', identifier()), keyword(':'), set('type', identifier())),
                sequence(set('type', identifier()), set('name', identifier())))
        
    def constant(self) -> dict:
        return { "fn": "constant" }

def test_parse_rules():
    log_assert("rule_as_string",
        rule_as_string(Zero().feature()), """
             feature = 
zeta.py:198: sequence(
zeta.py:199:     keyword('feature'), set('name', identifier()), 
zeta.py:200:     optional(sequence(keyword('extends'), set('parent', identifier()))), 
zeta.py:201:     block(list(any(function(), struct(), variable()))))
               """)

#------------------------------------------------------------------------------
# parsing helpers

# Reader is a lex-string and an index
class Reader:
    def __init__(self, ls: LexStr):
        self.ls = ls
        self.i = 0

    def peek(self) -> Lex:
        return self.ls[self.i] if self.i < len(self.ls) else None
    
    def advance(self):
        self.i += 1

    def eof(self) -> bool:
        return self.i >= len(self.ls)
    
    def match(self, fn) -> Lex:
        lex = self.peek()
        if lex and fn(str(lex)):
            self.advance()
            return lex
        return None

    def location(self, iLex: int=None) -> SourceLoc:
        if iLex is None: iLex = self.i
        if iLex >= len(self.ls): return "EOF"
        lex = self.ls[iLex]
        return lex.location()
    
# Error just holds a message and a point in the source file
class Error:
    def __init__(self, expected: str, reader: Reader, caller):
        self.caller = " " + log_grey(caller[0] + ":" + str(caller[1]))
        self.expected = expected
        self.reader = reader
        self.iLex = reader.i
        self.location = reader.location(self.iLex)

    def __str__(self):
        val = str(self.reader.ls[self.iLex]) if self.iLex < len(self.reader.ls) else "eof"
        for iLex in range(self.iLex+1, min(self.iLex+4, len(self.reader.ls))):
            val = val + " " + str(self.reader.ls[iLex])
        out= f"Expected {self.expected} at {self.location}:{self.caller}"
        return out
    
    def show_source(self) -> str:
        lex = self.reader.ls[self.iLex]
        lenLex = len(str(lex))
        text = lex.source.text
        lines = text.split('\n')[:-1]
        out = ""
        for i in range(max(0,self.location.line -3), min(self.location.line + 2, len(lines))):
            line = lines[i]
            if i == self.location.line-1:
                before = line[:self.location.column-1]
                mid = line[self.location.column-1:self.location.column-1+lenLex]
                after = line[self.location.column-1+lenLex:]
                out += f"{i+1:3} {before}{log_grey_background(mid)}{after}" + "\n"
            else:
                out += f"{i+1:3} {line}" + "\n"
        return out
    
    def is_later_than(self, other):
        return self.iLex > other.iLex
    
# return true if (obj) is an error
def err(obj) -> bool:
    return isinstance(obj, Error)

# combine multiple errors
def combine_errors(errors: List[Error]) -> Error:
    log("combine_errors:")
    for error in errors:
        log("  ", error)
    # first find the latest one
    latest = errors[0]
    for error in errors:
        if error.is_later_than(latest):
            latest = error
    # now find all errors at the same point as latest
    same_point = [latest]
    for error in errors:
        if error.iLex == latest.iLex and error != latest:
            same_point.append(error)
    # now combine all the 'expecteds' into an "or" string
    expecteds = [error.expected for error in same_point]
    expected = " or ".join(expecteds)
    # now return a new error with the combined expecteds
    latest.expected = expected
    return latest

#------------------------------------------------------------------------------
# Parser contains a method for each parser atom

class Parser:
    def __init__(self):
        pass

    def keyword(self, caller, reader, word: str)-> dict:
        if reader.eof() and word in ['{newline}', '{undent}']: return {} # special case for premature eof
        return {} if reader.match(lambda s: s == word) else Error(f"'{word}'", reader, caller)

    def indent(self, caller, reader)-> dict:
        return {} if reader.match(lambda s: s == "{indent}") else Error("{indent}", reader, caller)

    def undent(self, caller, reader )-> dict:
        return {} if reader.match(lambda s: s == "{undent}") else Error("{undent}", reader, caller)
    
    def newline(self, caller, reader)-> dict:
        return {} if reader.match(lambda s: s == "{newline}") else Error("{newline}", reader, caller)
    
    def identifier(self, caller, reader )-> dict:
        lex = reader.match(lambda s: is_id(s))
        return [lex] if lex else Error("identifier", reader, caller)

    def label(self, caller, reader, s: str, parser_fn)-> dict:
        ast = { '_type' : type }
        sub_ast = parser_fn(reader)
        if err(sub_ast): return sub_ast
        log("label(", type, "): ", sub_ast)
        ast.update(sub_ast)
        return ast

    def set(self, caller, reader, name: str, parser_fn)-> dict:
        ast = parser_fn(reader)
        if err(ast): return ast
        log("set(", name, "):", ast)
        return { name : ast }

    @log_disable
    def sequence(self, caller, reader, *parser_fns)-> dict:
        log("sequence")
        ast = {}
        log("  parser_fns", parser_fns)
        for parser_fn in parser_fns:
            result = parser_fn(reader)
            log("  result", result)
            if err(result): return result
            ast.update(result)
        return ast
    
    def optional(self, caller, reader, parser_fn)-> dict:
        iLex = reader.i
        ast = parser_fn(reader)
        if err(ast):
            if ast.iLex == iLex: return {}
            return ast
        return ast

    def enum(self, caller, reader, *words: List[str])-> dict:
        lex = reader.peek()
        if lex and str(lex) in words:
            reader.advance()
            return [lex]
        return Error(f"{words}", reader, caller)

    def any(self, caller, reader, *parser_fns)-> dict:
        errors = []
        for parse_fn in parser_fns:
            iLex = reader.i
            ast = parse_fn(reader)
            if not err(ast): return ast
            errors.append(ast)
            reader.i = iLex
        error = combine_errors(errors)
        return error

    def list(self, caller, reader, parser_fn, sep: str, term: str)-> dict:
        ast = []
        while True:
            if reader.match(lambda s: s == term):
                break
            sub_ast = parser_fn(reader)
            if err(sub_ast): return sub_ast
            ast.append(sub_ast)
            if reader.match(lambda s: s == term):
                break
            elif sep != None and reader.match(lambda s: s == sep):
                continue
        return ast

    def block(self, caller, reader, parser_fn)-> dict:
        if reader.match(lambda s: s == "{indent}"):
            ast = parser_fn(reader)
            if err(ast): return ast
            if reader.match(lambda s: s == "{undent}"):
                return ast
            return Error("{undent}", reader, caller)
        return Error("{indent}", reader, caller)
    
    def upto(self, caller, reader, *words: List[str])-> dict:
        depth = 0
        out = []
        while True:
            if reader.eof(): return out
            lex = reader.match(lambda s: s in words)
            if depth == 0 and lex: return out
            out.append(lex)
            if str(lex) in ["(", "[", "{indent}"]: depth += 1
            elif str(lex) in [")", "]", "{undent}"]: depth -= 1
            reader.advance()

    def maybe_bracketed(self, caller, reader, parser_fn)-> dict:
        open_bracket = reader.match(lambda s: s == "(")
        ast = parser_fn(reader)
        close_bracket = reader.match(lambda s: s == ")")
        if (open_bracket and close_bracket) or ((not open_bracket) and (not close_bracket)):
            return ast
        return Error("( ... )", reader, caller)

#------------------------------------------------------------------------------
# despatch takes a rule tree and a reader, and returns a parser function

def make_processor(imp, rule: dict):
    caller = rule['caller']
    if 'fn' not in rule: raise Exception(f"no 'fn' in rule {rule}")
    fn = rule['fn']
    method = getattr(imp, fn, None)
    if not method: raise Exception(f"no method {fn} in parser")
    args = []
    for key, val in rule.items():
        if not (key in ['caller', 'fn']):
            if isinstance(val, str) or isinstance(val, LexStr): args.append(val)
            elif isinstance(val, dict): args.append(make_processor(imp, val))
            elif isinstance(val, List) or isinstance(val, Tuple): args.extend([make_processor(imp, v) for v in val])
    return lambda reader: method(caller, reader, *args)

@log_enable
def test_parser():
    source = Source("src/test/Hello.zero.md")
    ls = Lexer(source).lex()
    fn = sequence(keyword("feature"), set("name", identifier()), optional(sequence(keyword("extends"), set("parent", identifier()))))
    parser = make_processor(Parser(), fn)
    reader = Reader(ls)
    ast = parser(reader)
    log(ast)


#------------------------------------------------------------------------------
# main, test, etc

def test():
    print("test ------------------------------------")
    test_parse_rules()
    test_lexer()
    test_parser()

def main():
    log_clear()
    print('ᕦ(ツ)ᕤ zeta.py')
    test()
          
if __name__ == '__main__':
    main()
    print("done.")





