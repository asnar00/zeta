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
    ctx = context()
    ctx_str = f"{ctx[0]}:{ctx[1]}"
    if sa == sb:
        print(f"{log_grey(ctx_str)} {name}: passed")
    else:
        print(f"{log_grey(ctx_str)} {name}: failed")
        print("expected:")
        print(log_disclose(sb))
        print("got:")
        print(log_disclose(sa))

#------------------------------------------------------------------------------
# context: figure out which file/line called us

s_cwd = os.getcwd() + "/src/"

# returns the (file, line) of the function that called our caller
def context():
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to context(), so...
    file = frame.filename.replace(s_cwd, '')    # dunno if we totally need this but 
    return (file, frame.lineno)                 # easier to process this way

# same, but returns a string
def context_string() -> str:
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to context(), so...
    file = frame.filename.replace(s_cwd, '')
    return f"{file}:{frame.lineno}"
#------------------------------------------------------------------------------
# file system 

# read a file
def readFile(path: str) -> str:
    with open(path, "r") as file:
        return file.read()

#------------------------------------------------------------------------------
# structure-based parsing and printing!
# these are the "atoms" out of which parsers are built
# we store them as dictionaries, so we can use them to parse, print, or do other things
# but we construct them using functions, so the parser rules are easier to read

# matches a specific keyword
def kw(kw: str)-> dict:
    return {'fn': 'kw', 'value': kw, 'ctx': context()}

# matches an indent
def indent()-> dict:
    return {'fn': 'indent', 'ctx': context()}

# matches an undent
def undent()-> dict:
    return {'fn': 'undent', 'ctx': context()}

# matches an alpha-numeric identifier
def id()-> dict:
    return {'fn': 'id'}

# labels the result of a rule as being of a certain type (eg "feature", "function")
def label(s: str, rule: dict)-> dict:
    return {'fn': 'label', 'label': s, 'rule': rule, 'ctx': context()}

# sets a property in the output AST to the result of the rule
def set(name: str, rule: dict)-> dict:
    return {'fn': 'set', 'name': name, 'rule': rule, 'ctx': context()}

# runs each rule in sequence, collects all results into a single AST node
def seq(*rules: List[dict])-> dict:
    return {'fn': 'seq', 'rules': rules, 'ctx': context()}

# optionally runs the rule, but succeeds even if it doesn't match
def opt(rule: dict)-> dict:
    return {'fn': 'opt', 'rule': rule, 'ctx': context()}

# matches one of the list of words (like keyword)
def enum(*words: List[str])-> dict:
    return {'fn': 'enum', 'words': words, 'ctx': context()}

# matches any of the sub-rules, or returns the last error
def any(*rules: List[dict])-> dict:
    return {'fn': 'any', 'rules': rules, 'ctx': context()}

# matches a list of the same rule, separated and terminated
def list(rule: dict, sep: str=None, term: str=None)-> dict:
    return {'fn': 'list', 'rule': rule, 'sep': sep, 'term': term, 'ctx': context()}

# matches an indented block of stuff
def block(rule: dict)-> dict:
    return {'fn': 'block', 'rule': rule, 'ctx': context()}

# grabs all lexemes up to one of the terminator strings, skipping brackets and quotes
def upto(*chars: List[str])-> dict:
    return {'fn': 'upto', 'chars': chars, 'ctx': context()}

# matches either a thing, or the thing in brackets
def maybe_bracketed(rule: dict)-> dict:
    return {'fn': 'maybe_bracketed', 'rule': rule, 'ctx': context()}

#------------------------------------------------------------------------------
# print a parser rule in a nice way, with links back to source code
# for debugging parser rules in a sensible, streamlined way

# print a parser rule as a nicely formatted, readable string matching the source code
def rule_as_string(rule):
    out = rule_as_string_rec(rule)
    file = rule['ctx'][0]
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
    rule_line = rule['ctx'][1] if 'ctx' in rule else line
    out += f'**{rule_line}/{indent}:'
    for key, val in rule.items():
        if key == 'fn':
            out += f'{val}('
        elif key != 'ctx': 
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

# feature (name) [extends (parent)] { list(function | struct | variable) }
def feature() -> dict:
    return label('feature', seq(
                kw('feature'), set('name', id()),
                opt(seq(kw('extends'), set('parent', id()))),
                block(list(any(function(), struct(), variable())))))

# (on/replaceafter/before) (result) (name) (parameters) { function_body }
def function() -> dict:
    return label('function', seq(
        set('modifier', enum('on', 'replace', 'after', 'before')),
        set('result', maybe_bracketed(name_type())),
        set('assign_op', enum("=", "<<")), 
        set('name', id()),
        kw('('), set('parameters', list(variable(), sep=',', term=')')), kw(')'),
        kw('{'), set('body', function_body()), kw('}')))

# stand-in for now
def function_body() -> dict:
    return {'fn': 'function_body'}
    
# struct (name) { list(variable) }
def struct() -> dict:
    return label('struct', seq(
                kw('struct'), set('name', id()),
                block(list(variable))))

# ((name type) | (type:name)) [= value]
def variable() -> dict:
    return label('variable', seq(
                name_type(),
                opt(seq(kw('='), set('value', constant())))))

def name_type() -> dict:
    return any(
            seq(set('name', id()), kw(':'), set('type', id())),
            seq(set('type', id()), set('name', id())))
    
def constant() -> dict:
    return { "fn": "constant" }

def test_parse_rules():
    log_assert("rule_as_string",
        rule_as_string(feature()), """
             feature = 
zeta.py:198: seq(
zeta.py:199:     kw('feature'), set('name', id()), 
zeta.py:200:     opt(seq(kw('extends'), set('parent', id()))), 
zeta.py:201:     block(list(any(function(), struct(), variable()))))
               """)
#------------------------------------------------------------------------------
# Source management

# Source is a file containing source code
class Source:
    def __init__(self, filename: str):
        self.filename = filename
        self.text = readFile(filename)

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
        return f'{self.source.filename}:{self.lineno}:{self.column}'
    
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

# takes a substring (enclosed in a lex) and ... lexes it
# structure: does it line by line, passing in indent from the last run
# on each line, counts whitespace, generates whitespace indents/undents, then does the rest of the line
# braces-based indent/undent checks against the last generated indent/undent token, maintains sense
# returns a list of lexemes, and the last indent level, allowing multiple substrings to chain properly
@log_disable
def lexer_substring(string: Lex, last_indent: int=None) -> Tuple[LexStr, int]:
    log(string)
    out = LexStr([])
    ls = str(string)
    i = 0
    ops = "!@#$%^&*-+=.<>?/~|"
    punct = "()[],;:"
    safeCount = 1000
    while i < len(ls):
        safeCount -=1
        if safeCount == 0:
            log("safecount!")
            exit(0)
        # count and skip whitespace, handle line ending
        j = i
        while j < len(ls) and ls[j] == ' ':
            j += 1
        indent = j - i
        log("    start:", indent, "spaces")
        if last_indent != None:
            if indent > last_indent:
                # sig-whitespace indent: skip if last was indent-brace
                if len(out) ==0 or out[-1].val != "{indent-brace}":
                    # if last was ":", remove it
                    if len(out) > 0 and out[-1].val == ":":
                        out.lexemes = out.lexemes[:-1]
                        log("--- removing previous ':'")
                    out += string.slice(i-1, j, '{indent-sig}')
                    log("{indent-sigwhite}")
            elif indent < last_indent:
                # sig-whitespace undent
                log("{undent-sig}")
                out += string.slice(i-1, j, '{undent-sig}')
            elif indent == last_indent:
                # sig-whitespace: no change, give us a newline
                log("{newline}")
                out += string.slice(i-1, j, '{newline}')
        last_indent = indent
        i = j # now positioned on first non-whitespace
        # now find end of line
        iEnd = ls.find('\n', i)
        if iEnd == -1: iEnd = len(ls)
        # process actual characters up until end of line
        while i < iEnd:
            c = ls[i]
            cn = ls[i+1] if i+1 < iEnd else ''
            # each case is responsible for advancing "i" correctly!!
            # skip whitespace
            if c == ' ':
                log("{whitespace}")
                i += 1
                continue
            # operators: match single or double
            elif c in ops:
                j = i + 1
                if cn != '\n' and cn in ops:
                    j = i + 2
                out += string.slice(i, j)
                log("{op}", log_disclose(str(out[-1])))
                i = j
            # alpha-numeric: match until non-alphanumeric
            elif is_alphanum(c):
                j = i+1
                while j < iEnd and is_alphanum_or_digit(ls[j]):
                    j += 1
                identifier= string.slice(i, j)
                out += identifier
                i = j
                log("{alphanum}", str(out[-1]))
            # digits
            elif c.isdigit():
                j = i+1
                while j < iEnd and ls[j].isdigit():
                    j += 1
                number = string.slice(i, j)
                out += number
                i = j
                log("{number}", str(out[-1]))
            # punctuation (including brackets)
            elif c in punct:
                out += string.slice(i, i+1)
                i = i+1
                log("{punctuation}", str(out[-1]))
            # string literal
            elif c == '"':
                j = i+1
                while j < iEnd and (ls[j] != '"' or ls[j-1] == "\\"):
                    j += 1
                literal = string.slice(i, j+1)
                out += literal
                log("{string}", str(out[-1]))
                i = j+1
            # braces based indent
            elif c == "{":
                out += string.slice(i, i+1, "{indent-brace}")
                i = i+1
                log("{indent-brace}")
            # braces-based undent: skip if previous was sig-whitespace indent
            elif c == "}":
                if len(out) > 0 and out[-1].val != "{undent-sig}":
                    out += string.slice(i, i+1, "{undent-brace}")
                else:
                    out[-1].val = "{undent-brace}"
                    log(" --replaced last undent with undent-brace")
                i = i+1
            else:
                log(f"lexer error: unhandled character '{c}'")
                exit(0)
        #---- end of while loop
        i = iEnd + 1
    # now clean up all the indents and undents
    for lex in out:
        if lex.val.startswith("{indent-"):
            lex.val = "{indent}"
        elif lex.val.startswith("{undent-"):
            lex.val = "{undent}"
    return out, last_indent

@log_enable
def lexer(source: Source) -> LexStr:
    code = extract_code(source)
    out = LexStr([])
    last_indent = None
    for i in range(0, len(code)):
        ls, last_indent = lexer_substring(code[i], last_indent)
        out += ls
    return out

def test_lexer():
    source = Source("src/test/Hello.zero.md")
    ls = lexer(source)
    log_assert("lexer", ls, """
feature Hello extends Main {newline} 
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
# main, test, etc

def test():
    print("test ------------------------------------")
    test_parse_rules()
    print(" ")
    test_lexer()

def main():
    log_clear()
    print('ᕦ(ツ)ᕤ zeta.py')
    test()
          
if __name__ == '__main__':
    main()
    print("done.")





