# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo

from typing import List, Tuple, Union, Dict, Any, Callable
import inspect
import os
import re
import traceback
import sys

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

# clear the log : handy because we're constantly re-running
def log_clear():
    os.system('clear')  # For Linux/macOS

# returns a grey-coloured foreground, black background
def log_grey(str) -> str:
    return f'\033[30;1m{str}\033[0m'

# returns red-coloured foreground, black background
def log_red(str) -> str:
    return f'\033[31;1m{str}\033[0m'

# returns a green foreground, black backgroud
def log_green(str) -> str:
    return f'\033[32;1m{str}\033[0m'

# returns a grey-coloured background, white foreground
def log_grey_background(str) -> str:
    return f'\033[47;30m{str}\033[0m'

# strips all colour-related codes and dynamic things out of a string (so we can compare test results)
def log_strip(str) -> str:
    str = re.sub(r'\033\[[0-9;]*m', '', str)
    str = re.sub(r':\d+:', ':...:', str) # strip out :number:, replace with :...:
    str = re.sub(r"0x[0-9a-f]+", "...", str)  # replace any hex-addresses ("0x....") with "..."
    str = re.sub(r"\n +", "\n", str) # replace (cr followed by multiple spaces) with (cr)
    return str.strip()

# log_disclose shows CRs and tabs and spaces within a string
def log_disclose(str) -> str:
    return str.replace('\n', '↩︎\n').replace('\t', '▶︎').replace(' ', '_')

# returns a length-limited string version of anything
def log_short(obj: Any, maxLen=64) -> str:
    s = str(obj) if not isinstance(obj, str) else '"' + obj + '"'
    if s == None: return "None"
    m = re.match(r"<__main__\.(\w+)", s)    # if s matches "<__main__.ClassName", return "ClassName"
    if m: s = m.group(1)
    if len(s) <= maxLen: return s
    return s[:maxLen] + " " + log_grey("...") + " " + s[-12:]

#------------------------------------------------------------------------------
# testing testing

s_tests = []

# test_assert checks if two strings are equal, and prints a message if they're not
def test_assert(name, a, b: str):
    sa = log_strip(str(a))
    sb = log_strip(b)
    if sb.startswith("\n"): sb = sb[1:]
    ctx = caller()
    ctx_str = f"{ctx[0]}:{ctx[1]}:"
    if sa == sb:
        print(f"{log_grey(ctx_str)} {log_green("passed")} {name}")
    else:
        print(f"{log_grey(ctx_str)} {log_red("failed")} {name}")
        print("expected:")
        print(sb) # or print(log_disclose(sb)) if you want to see CRs and spaces
        print("got:")
        print(sa) # or print(log_disclose(sa)) if you want to see CRs and spaces

# decorator that wraps a function, adding it to s_tests
def this_is_a_test(fn):
    s_tests.append(fn)
    return fn

# run all tests
def test_run_all():
    print("test ------------------------------------")
    global s_tests
    for test_fn in s_tests:
        test_fn()

#------------------------------------------------------------------------------
# phil call-ins: debug helpers to find out who called us, where from, and with what

s_cwd = os.getcwd() + "/src/"

# returns the (file, line) of the function that called our caller
def caller():
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to caller(), so...
    file = frame.filename.replace(s_cwd, '')    # dunno if we totally need this but 
    return (file, frame.lineno)                 # easier to process this way

# same, but returns a string
def caller_as_string(caller) -> str:
    return log_grey(f"{caller[0]}:{caller[1]}:") if caller else "no caller"

# caller-show prints a complete call stack- all functions and parameters
def caller_show(maxLevels: int = None, verbose: bool = True) -> str:
    out = ""
    level = 1
    while maxLevels == None or level <= maxLevels:
        cs = caller_show_level(level, verbose)
        if cs == None: break
        out += cs + "\n"
        level += 1
    return out.rstrip()

def pad(str, maxlen):
    return str + " " * max(0, (maxlen - len(str)))

def caller_show_level(level: int, verbose: bool = True) -> str:
    level = level + 1
    # get stack frame at (level) levels above us
    frame = inspect.currentframe()
    outers = inspect.getouterframes(frame)
    if level >= len(outers): return None
    frame = outers[level]
    if frame == None: return "unnamed"
    # get function name
    name = frame.function
    if name == "<module>": return None
    # get file and line number
    file = frame.filename.replace(s_cwd, '')
    line = frame.lineno
    loc = pad(f"{file}:{line}: ", 13)
    out = log_grey(loc) + name
    if verbose:
        # get all the parameters
        args = inspect.getargvalues(frame.frame)
        for arg_name in args.args:
            arg_value = args.locals[arg_name]
            arg_type = type(arg_value).__name__
            out += f"\n{pad(" ", 17)}{arg_name}: {arg_type} = "
            out += log_short(arg_value, 48)
    return out.rstrip()

def caller_get_arg(level: int, arg_name: str) -> Any:
    level = level + 1
    frame = inspect.currentframe()
    outers = inspect.getouterframes(frame)
    if level >= len(outers): return None
    frame = outers[level]
    if frame == None: return None
    args = inspect.getargvalues(frame.frame)
    return args.locals[arg_name] if arg_name in args.locals else None

def caller_exception(e: Exception) -> str:
    # Extract the traceback details from the exception
    tb = traceback.extract_tb(e.__traceback__)
    
    # Get the last frame (where the exception was raised)
    last_frame = tb[-1]
    
    # Return the file and line number in the desired format
    return log_grey(f"{last_frame.filename.replace(s_cwd, '')}:{last_frame.lineno}: ") + log_red(str(e))

def my_test_wrapper_func(n: str):
    return my_test_func(n)

def my_test_func(n: str):
   return(caller_show())

@this_is_a_test
def test_caller():
    test_assert("caller", my_test_wrapper_func("yo"), """
                                                zeta.py:...: my_test_func
                                                                n: str = "yo"
                                                zeta.py:...: my_test_wrapper_func
                                                                n: str = "yo"
                                                zeta.py:...: test_caller
                                                zeta.py:...: test_run_all
                                                zeta.py:...: main
""")
    
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
    def __init__(self):
        self.path = None
        self.text = None
    # construct from a path, read text
    @staticmethod
    def from_file(path: str) -> 'Source':
        source = Source()
        source.path = path
        source.text = readFile(path)
        return source

    # construct from a string, bypassing filesystem (for testing)
    @staticmethod
    def from_text(text: str) -> 'Source':
        source = Source()
        source.text = text
        return source

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
        if self.source.path:
            return f'{self.source.path}:{self.lineno}:{self.column}'
        return f'{self.lineno}:{self.column}'
    
    # and this
    def __repr__(self):
        return self.__str__()

#------------------------------------------------------------------------------
# lexy's midnight runners

# Lex is a lexeme: points at Source, has a start/end index, stores the value
# this lets (value) be different to the source if necessary, eg for smart indents and so on
class Lex:
    def __init__(self, source: Source, i: int=0, j: int=None, val: str=None):
        self.source = source
        self.i = i
        self.j = j if j else len(source.text)
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
        return out.rstrip()
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
    out = LexStr([])
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
        self.code = extract_code(self.source) if source.path else LexStr([Lex(source)])

    def lex(self):
        out = LexStr([])
        self.last_indent = None
        for i in range(0, len(self.code)):
            out += self.lex_substring(self.code[i])
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

@this_is_a_test
def test_lexer():
    source = Source.from_file("src/test/Hello.zero.md")
    lexer = Lexer(source)
    ls = lexer.lex()
    test_assert("lexer", ls, """
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
# grammar rule atoms
# the 'grammar' is a collection of syntax structured specified using these atoms
# each 'atom' returns a small structure that defines the atom/rule
# the idea is to be able to use these rules for parsing and printing

# matches a specific keyword
def keyword(word: str)-> Dict:
    return {'fn': 'keyword', 'value': word, 'caller': caller()}

# matches an indent
def indent()-> Dict:
    return {'fn': 'indent', 'caller': caller()}

# matches an undent
def undent()-> Dict:
    return {'fn': 'undent', 'caller': caller()}

# matches a newline (no indent change)
def newline()-> Dict:
    return {'fn': 'newline', 'caller': caller()}

# matches an alpha-numeric identifier
def identifier()-> Dict:
    return {'fn': 'identifier', 'caller': caller()}

# labels the result of a rule as being of a certain type (eg "feature", "function")
def label(s: str, rule: Dict)-> Dict:
    return {'fn': 'label', 'label': s, 'rule': rule, 'caller': caller()}

# sets a property in the output AST to the result of the rule
def set(name: str, rule: Dict)-> Dict:
    return {'fn': 'set', 'name': name, 'rule': rule, 'caller': caller()}

# runs each rule in sequence, collects all results into a single AST node
def sequence(*rules: List[Dict])-> Dict:
    return {'fn': 'sequence', 'rules': rules, 'caller': caller()}

# optionally runs the rule, but succeeds even if it doesn't match
def optional(rule: Dict)-> Dict:
    return {'fn': 'optional', 'rule': rule, 'caller': caller()}

# matches one of the list of words (like keyword)
def enum(*words: List[str])-> Dict:
    return {'fn': 'enum', 'words': words, 'caller': caller()}

# matches any of the sub-rules, or returns the last error
def any(*rules: List[Dict])-> Dict:
    return {'fn': 'any', 'rules': rules, 'caller': caller()}

# matches a list of the same rule, separated and terminated
def list(rule: Dict, sep: str=None)-> Dict:
    return {'fn': 'list', 'rule': rule, 'sep': sep, 'caller': caller()}

# matches an indented block of stuff
def block(rule: Dict)-> Dict:
    return {'fn': 'block', 'rule': rule, 'caller': caller()}

# grabs all lexemes up to (but not including) one of the terminator strings, skipping brackets and quotes
def upto(*chars: List[str])-> Dict:
    return {'fn': 'upto', 'chars': chars, 'caller': caller()}

# matches a thing in brackets
def brackets(rule: Dict) -> Dict:
    return {'fn': 'brackets', 'rule': rule, 'caller': caller()}
            
# matches either a thing, or the thing in brackets
def maybe_bracketed(rule: Dict)-> Dict:
    return {'fn': 'maybe_bracketed', 'rule': rule, 'caller': caller()}

#------------------------------------------------------------------------------
# print a grammar rule in a nice way, with links back to source code
# for debugging grammar rules in a sensible, streamlined way

# print a rule as a nicely formatted, readable string matching the source code
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
    ic = result.find("\n")
    if result[:ic].strip() == "": result = result[ic+1:]
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
            elif isinstance(val, Dict): 
                sub = maybe_rule_as_string(val, indent+1, line)
                out += f'{sub}, '
    if out.endswith(", ") : out = out[:-2]
    out += ')'
    return out

# if the rule is labeled, just return the name, otherwise dive into it
def maybe_rule_as_string(rule, indent: int=0, line: int=0) -> str:
    if isinstance(rule, Dict) and rule['fn'] == 'label':
        return rule['label'] + "()"
    return rule_as_string_rec(rule, indent)

def print_newline(indent: int) -> str:
    return '\n' + '  ' * indent

@this_is_a_test
def test_print_rules():
    rule = sequence(keyword('feature'), set('name', identifier()),
                    optional(sequence(keyword('extends'), set('parent', identifier()))))
    test_assert("rule",
        rule_as_string(rule), """
zeta.py:...: sequence(keyword('feature'), set('name', identifier()), 
zeta.py:...:     optional(sequence(keyword('extends'), set('parent', identifier()))))
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
        self.caller = caller
        self.expected = expected
        self.reader = reader
        self.iLex = reader.i
        self.location = reader.location(self.iLex)

    def __str__(self):
        val = str(self.reader.ls[self.iLex]) if self.iLex < len(self.reader.ls) else "eof"
        for iLex in range(self.iLex+1, min(self.iLex+4, len(self.reader.ls))):
            val = val + " " + str(self.reader.ls[iLex])
        out= f"{caller_as_string(self.caller)} Expected {self.expected} at {self.location} "
        return out
    
    def __repr__(self):
        return "[" + self.__str__() + "]"
    
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

    def keyword(self, caller, reader, word: str)-> Dict:
        if reader.eof() and word in ['{newline}', '{undent}']: return {} # special case for premature eof
        return {} if reader.match(lambda s: s == word) else Error(f"'{word}'", reader, caller)

    def indent(self, caller, reader)-> Dict:
        return {} if reader.match(lambda s: s == "{indent}") else Error("{indent}", reader, caller)

    def undent(self, caller, reader )-> Dict:
        return {} if reader.match(lambda s: s == "{undent}") else Error("{undent}", reader, caller)
    
    def newline(self, caller, reader)-> Dict:
        return {} if reader.match(lambda s: s == "{newline}") else Error("{newline}", reader, caller)
    
    def identifier(self, caller, reader )-> Dict:
        lex = reader.match(lambda s: is_id(s))
        return [lex] if lex else Error("identifier", reader, caller)

    def label(self, caller, reader, s: str, parser_fn)-> Dict:
        ast = { '_type' : type }
        sub_ast = parser_fn(reader)
        if err(sub_ast): return sub_ast
        log("label(", type, "): ", sub_ast)
        ast.update(sub_ast)
        return ast

    def set(self, caller, reader, name: str, parser_fn)-> Dict:
        ast = parser_fn(reader)
        if err(ast): return ast
        maybe_error = ast['_error'] if '_error' in ast else None
        result= { name : ast }
        if maybe_error: result['_error'] = maybe_error
        return result
    
    def sequence(self, caller, reader, *parser_fns)-> Dict:
        ast = {}
        for parser_fn in parser_fns:
            sub_ast = parser_fn(reader)
            if err(sub_ast):
                if ('_error' in ast): return ast['_error']
                return sub_ast
            else:
                if '_error' in ast: del ast['_error']
                ast.update(sub_ast)
        return ast
    
    def optional(self, caller, reader, parser_fn)-> Dict:
        iLex = reader.i
        ast = parser_fn(reader)
        if err(ast):
            if ast.iLex == iLex: return {}
            return ast
        return ast

    def enum(self, caller, reader, *words: List[str])-> Dict:
        lex = reader.peek()
        if lex and str(lex) in words:
            reader.advance()
            return [lex]
        return Error(f"one of {words}", reader, caller)

    def any(self, caller, reader, *parser_fns)-> Dict:
        errors = []
        for parse_fn in parser_fns:
            iLex = reader.i
            ast = parse_fn(reader)
            if not err(ast): return ast
            errors.append(ast)
            reader.i = iLex
        error = combine_errors(errors)
        return error

    def list(self, caller, reader, parser_fn, sep: str)-> Dict:
        items = []
        while not (reader.eof()):
            sub_ast = parser_fn(reader)
            if err(sub_ast):
                return{ '_list': items, '_error': sub_ast }
            items.append(sub_ast)
            if sep != None:
                if not reader.match(lambda s: s == sep):
                    return { '_list': items, '_error': Error(f"'{sep}'", reader, caller)}
        return { '_list': items }

    def block(self, caller, reader, parser_fn)-> Dict:
        if reader.match(lambda s: s == "{indent}"):
            ast = parser_fn(reader)
            if err(ast): return ast
            if reader.match(lambda s: s == "{undent}"):
                return ast
            return Error("{undent}", reader, caller)
        return Error("{indent}", reader, caller)
    
    def upto(self, caller, reader, *words: List[str])-> Dict:
        depth = 0
        out = []
        while True:
            if reader.eof(): return out
            lex = reader.peek()
            if depth == 0 and str(lex) in words: return out
            out.append(lex)
            if str(lex) in ["(", "[", "{indent}"]: depth += 1
            elif str(lex) in [")", "]", "{undent}"]: depth -= 1
            reader.advance()

    def brackets(self, caller, reader, parser_fn) -> Dict:
        open_bracket = reader.match(lambda s: s == "(")
        ast = parser_fn(reader)
        close_bracket = reader.match(lambda s: s == ")")
        if open_bracket and close_bracket:
            return ast
        return Error("( ... )", reader, caller)

    def maybe_bracketed(self, caller, reader, parser_fn)-> Dict:
        open_bracket = reader.match(lambda s: s == "(")
        ast = parser_fn(reader)
        close_bracket = reader.match(lambda s: s == ")")
        if (open_bracket and close_bracket) or ((not open_bracket) and (not close_bracket)):
            return ast
        return Error("( ... )", reader, caller)

#------------------------------------------------------------------------------
# despatch takes a rule tree and a reader, and returns a parser function

def make_parser(imp, rule: Any) -> Callable:
    try:
        if rule == None: return None
        if isinstance(rule, str) or isinstance(rule, LexStr): return rule
        if isinstance(rule, List) or isinstance(rule, Tuple): 
            print("rule is list or tuple:", rule)
            exit(0)
        caller = rule['caller'] if 'caller' in rule else None
        if 'fn' not in rule: raise Exception(f"no 'fn' in rule {rule}")
        fn = rule['fn']
        method = getattr(imp, fn, None)
        if not method: raise Exception(f"no method '{fn}' in {imp.__class__.__name__}")
        args = []
        for key, val in rule.items():
            if not (key in ['caller', 'fn']):
                if isinstance(val, List) or isinstance(val, Tuple): 
                    args.extend([make_parser(imp, v) for v in val])
                else:
                    args.append(make_parser(imp, val))
        return lambda reader: method(caller, reader, *args)
    except Exception as e:
        print(caller_exception(e))
        print("problematic rule:")
        rule = caller_get_arg(1, 'rule')
        print(rule_as_string(rule))
        exit(0)

@log_enable
@this_is_a_test
def test_parser():
    source = Source.from_file('src/test/Hello.zero.md')
    ls = Lexer(source).lex()
    fn = sequence(keyword('feature'), set('name', identifier()),
                  optional(sequence(keyword('extends'), set('parent', identifier()))))
    parser = make_parser(Parser(), fn)
    reader = Reader(ls)
    ast = parser(reader)
    test_assert("parser", ast, """{'name': [Hello], 'parent': [Main]}""")

def test_parse(fn, text: str) -> LexStr:
    source = Source.from_text(text)
    ls = Lexer(source).lex()
    parser = make_parser(Parser(), fn)
    reader = Reader(ls)
    return parser(reader)

# this ensures that sequences containing lists followed by something not in the list, work correctly
@this_is_a_test
@log_enable
def test_sequence_list():
    fn = sequence(list(enum("a", "b")), keyword("{indent}"))
    # this should succeed because the list is followed by {indent}
    test_assert("good_list", test_parse(fn, "a b {"), "{'_list': [[a], [b]]}")
    # this should fail because the list contains an erroneous item ("c") not in the enum
    test_assert("bad_list", test_parse(fn, "a b c {"), "zeta.py:946: Expected one of ('a', 'b') at 1:4")

#------------------------------------------------------------------------------
# parse rules for zero

class Grammar: pass

class Zero(Grammar):
    # feature (name) [extends (parent)] { list(function | struct | variable) }
    def feature(self) -> Dict:
        return label('feature', sequence(
                    keyword('feature'), set('name', identifier()),
                    optional(sequence(keyword('extends'), set('parent', identifier()))),
                    set('components', block(list(any(self.function(), self.struct(), self.variable(), self.test()))))))

    # ">" blah [ "=>" blah]
    def test(self) -> Dict:
        return label("test", sequence(
                    keyword(">"), set("expression", upto("{newline}", "=>")),
                    optional(sequence(keyword("=>"), set("expected", upto("{newline}"))))))
    
    # (on/replaceafter/before) (result) (name) (parameters) { function_body }
    def function(self) -> Dict:
        return label('function', sequence(
            set('modifier', enum('on', 'replace', 'after', 'before')),
            set('result', maybe_bracketed(self.name_type())),
            set('assign_op', enum("=", "<<")), 
            set('signature', self.function_signature()),
            set('body', block(self.function_body()))))

    # function signature: any sequence of (word/param)
    def function_signature(self) -> Dict:
        return list(any(set('word', identifier()), 
                        set('param', brackets(list(self.variable(), ",")))))
    
    # stand-in for now
    def function_body(self) -> Dict:
        return upto("{undent}")
        
    # struct (name) { list(variable) }
    def struct(self) -> Dict:
        return label('struct', sequence(
                    keyword('struct'), set('name', identifier()),
                    block(list(self.variable()))))

    # ((name type) | (type:name)) [= value]
    def variable(self) -> Dict:
        return label('variable', sequence(
                    self.name_type(),
                    optional(sequence(keyword('='), set('value', self.constant())))))

    def name_type(self) -> Dict:
        return any(
                sequence(set('name', identifier()), keyword(':'), set('type', identifier())),
                sequence(set('type', identifier()), set('name', identifier())))
        
    def constant(self) -> Dict:
        return upto("{newline}", ",", ";")

@this_is_a_test
def test_zero_grammar():
    zero = Zero()
    
    
#------------------------------------------------------------------------------
# main, test, etc

def main():
    log_clear()
    print('ᕦ(ツ)ᕤ zeta.py')
    test_run_all()
          
if __name__ == '__main__':
    main()
    print("done.")