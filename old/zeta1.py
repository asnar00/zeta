# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo
# zero to anything

from typing import List, Tuple, Union, Dict, Any, Callable
from typing import get_type_hints
import inspect
import os
import re
import traceback
import sys
import subprocess
import requests
import shutil
from threading import Thread

#------------------------------------------------------------------------------
# logging

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

log_clear()
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
def log_short(obj: Any, maxLen=32) -> str:
    if obj == None: return "None"
    s = str(obj)
    m = re.match(r"<__main__\.(\w+)", s)    # if s matches "<__main__.ClassName", return "ClassName"
    if m: s = m.group(1)
    if len(s) <= maxLen: return s
    return s[:maxLen] + " " + log_grey("...") + " " + s[-12:]

#------------------------------------------------------------------------------
# testing

s_tests = []

# test_assert checks if two strings are equal, and prints a message if they're not
def test_assert(name, a, b: str):
    sa = log_strip(str(a))
    sb = log_strip(b)
    if sb.startswith("\n"): sb = sb[1:]
    ctx = caller()
    if sa == sb:
        print(f"{log_grey(ctx)} {log_green("passed")} {name}")
    else:
        print(f"{log_grey(ctx)} {log_red("failed")} {name}")
        print("expected:")
        print(log_grey(sb)) # or print(log_disclose(sb)) if you want to see CRs and spaces
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
# call-stack introspection

s_cwd = os.getcwd() + "/src/"

# returns 'file:line:' of the function that called our caller
def caller() -> str:
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to caller(), so...
    file = frame.filename.replace(s_cwd, '')    # dunno if we totally need this but 
    return f"{file}:{frame.lineno}:"

# extracts filename from a 'caller' string
def caller_file(caller: str) -> str:
    parts = caller.split(":")
    return parts[0]

# extracts line number from a 'caller' string
def caller_line(caller: str) -> int:
    parts = caller.split(":")
    return int(parts[1])

# get the actual line of source code
def caller_source(caller: str) -> str:
    parts = caller.split(":")
    file = parts[0]
    line = int(parts[1])
    return read_file(s_cwd + file).split('\n')[line-1]

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

# shows a single level of the callstack (0 is the caller of this function), including function-params
def caller_show_level(level: int, verbose: bool = True) -> str:
    level = level + 1
    # get stack frame at (level) levels above us
    frame = inspect.currentframe()
    outers = inspect.getouterframes(frame)
    if level >= len(outers): return None
    frame = outers[level]
    if frame == None: return None
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

# finds the argument value from a function higher up in the call-stack
def caller_get_arg(level: int, arg_name: str) -> Any:
    level = level + 1
    frame = inspect.currentframe()
    outers = inspect.getouterframes(frame)
    if level >= len(outers): return None
    frame = outers[level]
    if frame == None: return None
    args = inspect.getargvalues(frame.frame)
    return args.locals[arg_name] if arg_name in args.locals else None

#------------------------------------------------------------------------------
# exception handling

# prints a short exception message including a clickable line number
def exception_message(e: Exception) -> str:
    # Extract the traceback details from the exception
    tb = traceback.extract_tb(e.__traceback__)
    # Get the last frame (where the exception was raised)
    i_frame = len(tb) - 1
    last_frame = tb[i_frame]
    return log_grey(f"{last_frame.filename.replace(s_cwd, '')}:{last_frame.lineno}: ") + log_red("!!! " + str(e))

# Get the function signature with parameter names, types, and values.
def get_function_signature(func, frame):
    if inspect.ismethod(func):
        func = func.__func__
    sig = inspect.signature(func) # Get function's parameter specifications
    # Get type hints (if any)
    try:
        type_hints = get_type_hints(func)
    except:
        type_hints = {}
    # Get local variables from the frame
    local_vars = frame.f_locals
    # Build the signature string
    params = []
    for name, param in sig.parameters.items():
        param_type = type_hints.get(name, inspect.Parameter.empty)
        param_value = local_vars.get(name, inspect.Parameter.empty)
        param_str = name
        if param_value is not inspect.Parameter.empty:
            actual_type = type(param_value).__name__
            ps = f"{param_value!r}"
            ps = log_short(ps)
            if param_type is not inspect.Parameter.empty:
                if actual_type != param_type.__name__:
                    actual_type = log_red(actual_type)
                param_str += f": {actual_type}"
            param_str += f" = {ps}"
        params.append(param_str)  
    return ", ".join(params)

# exception handler: called when an exception is raised
def exception_handler(exc_type, exc_value, exc_traceback):
    msg = log_red(f"exception: {str(exc_value)!r}") + "\n"
    frames = []
    tb = exc_traceback
    while tb is not None:
        frame = tb.tb_frame
        filename = frame.f_code.co_filename.replace(s_cwd, '')
        function_name = frame.f_code.co_name
        lineno = tb.tb_lineno
        
        # Get the function object
        func = frame.f_globals.get(function_name)
        if func is None:
            func = frame.f_locals.get(function_name)
            if func is None and 'self' in frame.f_locals:
                # It might be a method called on self
                instance = frame.f_locals['self']
                func = getattr(instance.__class__, function_name, None)
                if func:
                    func = func.__get__(instance, instance.__class__)  # Bind the method
        
        
        if func is not None:
            # Get the function signature
            try:
                signature = get_function_signature(func, frame)
            except:
                signature = ""
        else:
            signature = ""
        
        frames.append((filename, lineno, function_name, signature))
        tb = tb.tb_next
    
    # Print frame information in reverse order
    for filename, lineno, function_name, signature in reversed(frames):
        if function_name.startswith("wrapper"): continue
        if function_name.startswith("<lambda>"): continue
        if function_name.startswith("<module>"): continue
        loc = log_grey(f"{filename}:{lineno}: ")
        msg += (f"{loc}{function_name} ({signature})") + "\n"
    print(msg)

# installs our exception handler
def exception_install_handler():
    sys.excepthook = exception_handler

# comment this out to revert to normal exception readouts
exception_install_handler()

def my_test_wrapper_func(n: str):
    return my_test_func(n)

def my_test_func(n: str):
   return(caller_show())

@this_is_a_test
def test_caller():
    test_assert("caller", my_test_wrapper_func("yo"), """
        zeta.py:...: my_test_func
        n: str = yo
        zeta.py:...: my_test_wrapper_func
        n: str = yo
        zeta.py:...: test_caller
        zeta.py:...: test_run_all
        zeta.py:...: main
    """)

def my_test_raiser(p: str):
    raise Exception("oops " + p)

def my_test_bad_func(p: str):
    my_test_raiser(p)

# exception readout can't be tested
# test exception: uncomment the following line to see a test exception trace readout
#@this_is_a_test
def test_exception():
    my_test_bad_func("hey")

#------------------------------------------------------------------------------
# file system

s_file_cache = {}  # maps filename => text

# read a file, cache it
def read_file(path: str) -> str:
    global s_file_cache
    if path in s_file_cache: 
        return s_file_cache[path]
    text = ""
    with open(path, "r") as file:
        text = file.read()
        s_file_cache[path] = text
    return text

# write a file, ensuring folders exist
def write_file(path: str, text: str):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as file:
        file.write(text)
    s_file_cache[path] = text

#------------------------------------------------------------------------------
# source code management

# Source is a file containing source code
class Source:
    def __init__(self):
        self.path = None
        self.text = ""
        self.source_map = []

    # construct from a path, read text
    @staticmethod
    def from_file(path: str) -> 'Source':
        source = Source()
        source.path = path
        source.text = read_file(path)
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
# lexer

s_lex_verbose = False

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
        global s_lex_verbose
        if s_lex_verbose:
            return f"**{self.location().lineno}>>{self.val}"
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
            out += log_disclose(str(lex)) + " "
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

# returns true if (str) is an operator
def is_operator(val: str):
    return val in "!@#$%^&*-+=.<>?/~|"

# returns true if (str) is a punctuation character
def is_punctuation(val: str):
    return val in "()[],;:"

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
        while self.i < len(self.ls):
            self.line_start()
            self.find_eol()
            # process actual characters up until end of line
            while self.i < self.iEnd:
                c = self.ls[self.i]
                cn = self.ls[self.i+1] if self.i+1 < self.iEnd else ''
                # note: each case is responsible for advancing "self.i" correctly!!
                if c == ' ':  self.i += 1
                elif is_operator(c): self.read(lambda c, cp: is_operator(c), 2)
                elif is_alphanum(c): self.read(lambda c, cp: is_alphanum_or_digit(c))
                elif c.isdigit(): self.read(lambda c, cp : c.isdigit())
                elif is_punctuation(c): self.push(self.i + 1)
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
                nUndents = (self.last_indent - indent) // 4
                for i in range(0, nUndents):
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
    
    def push(self, j, val=None):
        if j > self.i:
            self.out += self.string.slice(self.i, j, val)
            self.i = j

    def read(self, fn, n_max=None):
        j = self.match(fn, n_max)
        self.push(j)

    def string_literal(self):
        j = self.match(lambda c, cp: c != '"' or cp == "\\")
        self.push(j+1)

    def braces(self, c):
        if c == "{":
            self.push(self.i+1, "{indent-brace}")
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
        => "hello_world" {newline} 
        > hello ( ) {newline} 
        on ( out$ : string ) << hello ( ) {indent} 
        out$ << "hello_world" {undent} 
        {undent} 
        on ( string out$ ) << hello ( ) {indent} 
        out$ << "hello_world" ; {undent} 
        {newline} 
        on ( out$ : string ) << hello ( ) {indent} 
        out$ << "hello_world" ; {undent} 
        {newline} 
        on ( out$ : string ) << hello ( ) {indent} 
        out$ << "hello_world" {undent} 
        on ( string out$ ) << hello ( ) {indent} 
        out$ << "hello_world"
    """)

#------------------------------------------------------------------------------
# grammar atoms

# an Rule is an atomic component of a grammar rule
class Rule(dict):
    pass

# there follows a list of grammar atoms:

# matches a specific keyword
def keyword(word: str)-> Rule:
    return Rule({'fn': 'keyword', 'value': word, 'caller': caller()})

# matches an indent
def indent()-> Rule:
    return Rule({'fn': 'indent', 'caller': caller()})

# matches an undent
def undent()-> Rule:
    return Rule({'fn': 'undent', 'caller': caller()})

# matches a newline (no indent change)
def newline()-> Rule:
    return Rule({'fn': 'newline', 'caller': caller()})

# matches an alpha-numeric identifier
def identifier()-> Rule:
    return Rule({'fn': 'identifier', 'caller': caller()})

# labels the result of a rule as being of a certain type (eg "feature", "function")
def label(s: str, rule: Rule)-> Rule:
    return Rule({'fn': 'label', 'label': s, 'rule': rule, 'caller': caller()})

# sets a property in the output AST to the result of the rule
def set(name: str, rule: Rule)-> Rule:
    return Rule({'fn': 'set', 'name': name, 'rule': rule, 'caller': caller()})

# runs each rule in sequence, collects all results into a single AST node
def sequence(*rules: List[Rule])-> Rule:
    return Rule({'fn': 'sequence', 'rules': rules, 'caller': caller()})

# optionally runs the rule, but succeeds even if it doesn't match
def optional(rule: Rule)-> Rule:
    return Rule({'fn': 'optional', 'rule': rule, 'caller': caller()})

# matches one of the list of words (like keyword)
def enum(*words: List[str])-> Rule:
    return Rule({'fn': 'enum', 'words': words, 'caller': caller()})

# matches any of the sub-rules, or returns the last error
def any(*rules: List[Rule])-> Rule:
    return Rule({'fn': 'any', 'rules': rules, 'caller': caller()})

# matches a list of the same rule, separated and terminated
def list(rule: Rule, sep: str=None)-> Rule:
    return Rule({'fn': 'list', 'rule': rule, 'sep': sep, 'caller': caller()})

# matches an indented block of stuff
def block(rule: Rule)-> Rule:
    return Rule({'fn': 'block', 'rule': rule, 'caller': caller()})

# grabs all lexemes up to (but not including) one of the terminator strings, skipping brackets and quotes
def upto(*chars: List[str])-> Rule:
    return Rule({'fn': 'upto', 'chars': chars, 'caller': caller()})

# matches a thing in brackets
def brackets(rule: Rule) -> Rule:
    return Rule({'fn': 'brackets', 'rule': rule, 'caller': caller()})
            
# matches either a thing, or the thing in brackets
def maybe_bracketed(rule: Rule)-> Rule:
    return Rule({'fn': 'maybe_bracketed', 'rule': rule, 'caller': caller()})

# turns on debug-mode for the sub-tree inside this
def debug(rule: Rule):
    return Rule({'fn': 'debug', 'rule': rule, 'caller': caller()})

#------------------------------------------------------------------------------
# print a grammar rule in a nice way, with links back to source code
# for debugging grammar rules in a sensible, streamlined way

# print a rule as a nicely formatted, readable string matching the source code
def rule_as_string(rule):
    if rule == None: return "None"
    out = rule_as_string_rec(rule)
    file = caller_file(rule['caller'])
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
    if rule is None: return "None"
    if isinstance(rule, str): return rule
    if 'fn' in rule and rule['fn'] == 'label':
        return f'{rule["label"]} = {rule_as_string_rec(rule["rule"], indent, line)}'
    out = ''
    rule_line = caller_line(rule['caller']) if 'caller' in rule else line
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
            elif isinstance(val, Rule): 
                sub = maybe_rule_as_string(val, indent+1, line)
                out += f'{sub}, '
    if out.endswith(", ") : out = out[:-2]
    out += ')'
    return out

# if the rule is labeled, just return the name, otherwise dive into it
def maybe_rule_as_string(rule, indent: int=0, line: int=0) -> str:
    if isinstance(rule, Rule) and rule['fn'] == 'label':
        return rule['label'] + "()"
    return rule_as_string_rec(rule, indent)

@this_is_a_test
def test_print_rules():
    rule = sequence(keyword('feature'), set('name', identifier()),
                    optional(sequence(keyword('extends'), set('parent', identifier()))))
    test_assert("rule", rule_as_string(rule), """
        zeta.py:747: sequence(keyword('feature'), set('name', identifier()), 
        zeta.py:748:     optional(sequence(keyword('extends'), set('parent', identifier()))))
    """)

#------------------------------------------------------------------------------
# parsing helpers

# Reader is a lex-string and an index
class Reader:
    def __init__(self, ls: LexStr):
        self.ls = ls
        self.i = 0
        self.debug = False

    # skip newlines
    def skip_newlines(self):
        while (not self.eof()) and (str(self.ls[self.i]) == '{newline}'): self.i += 1

    # peek, skipping newlines
    def peek(self) -> Lex:
        self.skip_newlines()
        return self.ls[self.i] if self.i < len(self.ls) else None
    
    # move one lex forward 
    def advance(self):
        self.i += 1

    # true if we're at the end
    def eof(self) -> bool:
        return self.i >= len(self.ls)
    
    # match a single lexeme with any function
    def match(self, fn) -> Lex:
        lex = self.peek()

        if self.debug: # print everything we know about the current state
            print()
            loc = self.ls[self.i].location()
            print(f"{log_grey(loc)}", self.ls[self.i : self.i + 4])
            print(caller_show_level(1, False))
            i = 1
            line = ''
            while True:
                level = caller_show_level(i, False)
                if level == None or 'parse_debug' in level: break
                caller = caller_get_arg(i, 'caller')
                source = caller_source(caller).strip()
                next = f"{log_grey(caller)} {log_grey(source)}"
                if next != line: 
                    print(next)
                    line = next
                i += 2

        if lex and fn(str(lex)):
            if self.debug: print(f"MATCHED")
            self.advance()
            return lex
        if self.debug: print("no match")
        return None

    # return the source-code location of the current lexeme
    def location(self, iLex: int=None) -> SourceLoc:
        if iLex is None: iLex = self.i
        if iLex >= len(self.ls): return "EOF"
        lex = self.ls[iLex]
        return lex.location()
    
    def __str__(self):
        return str(self.ls[self.i:self.i + 3])
    
    def __repr__(self): return self.__str__()

    def state(self):
        loc = self.ls[self.i].location()
        return f"{log_grey(loc)} {self.ls[self.i : self.i + 4]}"
    
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
        out= f"{log_grey(self.caller)} Expected {self.expected} at {self.location} "
        return out
    
    def __repr__(self):
        return "[" + self.__str__() + "]"
    
    # shows the error and the surrounding source code
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
    
    # combine multiple errors
    @staticmethod
    @log_disable
    def combine(errors: List['Error']) -> 'Error':
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
        log("returning:", latest)
        return latest
    
# return true if (obj) is an error
def err(obj) -> bool:
    return isinstance(obj, Error)



#------------------------------------------------------------------------------
# Parser contains a method for each parser atom

class AST(dict):
    pass

class Parser:
    def __init__(self):
        pass

    def make(self, rule: Any) -> Callable:
        try:
            if rule == None: return None
            if isinstance(rule, str) or isinstance(rule, LexStr): return rule
            if isinstance(rule, List) or isinstance(rule, Tuple): 
                print("rule is list or tuple:", rule)
                exit(0)
            caller = rule['caller'] if 'caller' in rule else None
            if 'fn' not in rule: raise Exception(f"no 'fn' in rule {rule}")
            fn = rule['fn']
            method = getattr(self, "parse_" + fn, None)
            if not method: raise Exception(f"no method '{fn}' in {self.__class__.__name__}")
            args = []
            for key, val in rule.items():
                if not (key in ['caller', 'fn']):
                    if isinstance(val, List) or isinstance(val, Tuple): 
                        args.extend([self.make(v) for v in val])
                    else:
                        args.append(self.make(val))
            return lambda reader: method(caller, reader, *args)
        except Exception as e:
            print(exception_message(e))
            print("problematic rule:")
            rule = caller_get_arg(1, 'rule')
            print(rule_as_string(rule))
            exit(0)

    def parse_keyword(self, caller, reader, word: str)-> AST|Error:
        if reader.eof() and word in ['{newline}', '{undent}']: return {} # special case for premature eof
        return AST({}) if reader.match(lambda s: s == word) else Error(f"'{word}'", reader, caller)

    def parse_indent(self, caller, reader)-> AST|Error:
        return AST({}) if reader.match(lambda s: s == "{indent}") else Error("{indent}", reader, caller)

    def parse_undent(self, caller, reader )-> AST|Error:
        return AST({}) if reader.match(lambda s: s == "{undent}") else Error("{undent}", reader, caller)
    
    def parse_newline(self, caller, reader)-> AST|Error:
        return AST({}) if reader.match(lambda s: s == "{newline}") else Error("{newline}", reader, caller)
    
    def parse_identifier(self, caller, reader )-> LexStr|Error:
        lex = reader.match(lambda s: is_id(s))
        return LexStr([lex]) if lex else Error("identifier", reader, caller)

    def parse_label(self, caller, reader, label: str, parser_fn)-> AST|Error:
        ast = AST({ '_type' : label })
        sub_ast = parser_fn(reader)
        if err(sub_ast): return sub_ast
        ast.update(sub_ast)
        return ast

    def parse_set(self, caller, reader, name: str, parser_fn)-> AST|Error:
        ast = parser_fn(reader)
        if err(ast): return ast
        if '_list' in ast: ast = ast['_list']
        result= AST({ name : ast })
        return result
    
    def parse_sequence(self, caller, reader, *parser_fns)-> AST|Error:
        ast =AST({})
        for parser_fn in parser_fns:
            sub_ast = parser_fn(reader)
            if err(sub_ast):
                if ('_error' in ast): return ast['_error']
                return sub_ast
            else:
                if '_error' in ast: del ast['_error']
                ast.update(sub_ast)
        return ast
    
    def parse_optional(self, caller, reader, parser_fn)-> AST:
        reader.skip_newlines()
        iLex = reader.i
        ast = parser_fn(reader)
        if err(ast):
            if reader.debug: print(f"\noptional failed: {ast}")
            if ast.iLex == iLex: return AST({})
            return ast
        return ast

    def parse_enum(self, caller, reader, *words: List[str])-> LexStr|Error:
        lex = reader.peek()
        if lex and str(lex) in words:
            reader.advance()
            return LexStr([lex])
        return Error(f"one of {words}", reader, caller)

    def parse_any(self, caller, reader, *parser_fns)-> AST:
        errors = []
        for parse_fn in parser_fns:
            iLex = reader.i
            ast = parse_fn(reader)
            if not err(ast): return ast
            errors.append(ast)
            reader.i = iLex
        error = Error.combine(errors)
        return error

    def parse_list(self, caller, reader, parser_fn, sep: str)-> AST:
        items = []
        while not (reader.eof()):
            sub_ast = parser_fn(reader)
            if err(sub_ast):
                return{ '_list': items, '_error': sub_ast }
            items.append(sub_ast)
            if sep != None and sep != "{newline}":
                if not reader.match(lambda s: s == sep):
                    return { '_list': items, '_error': Error(f"'{sep}'", reader, caller)}
        return { '_list': items }

    def parse_block(self, caller, reader, parser_fn)-> AST|Error:
        if reader.match(lambda s: s == "{indent}"):
            ast = parser_fn(reader)
            if err(ast): return ast
            if reader.match(lambda s: s == "{undent}") or reader.eof(): # tolerate premature eof in this case
                return ast
            return Error("{undent}", reader, caller)
        return Error("{indent}", reader, caller)
    
    def parse_upto(self, caller, reader, *words: List[str])-> LexStr|Error:
        depth = 0
        out = LexStr([])
        while True:
            if reader.eof(): return out
            lex = reader.ls[reader.i]
            if depth == 0 and str(lex) in words: return out
            out += lex
            if str(lex) in ["(", "[", "{indent}"]: depth += 1
            elif str(lex) in [")", "]", "{undent}"]: depth -= 1
            reader.advance()

    def parse_brackets(self, caller, reader, parser_fn) -> AST|Error:
        open_bracket = reader.match(lambda s: s == "(")
        ast = parser_fn(reader)
        close_bracket = reader.match(lambda s: s == ")")
        if open_bracket and close_bracket:
            return ast
        return Error("( ... )", reader, caller)

    def parse_maybe_bracketed(self, caller, reader, parser_fn)-> AST|Error:
        open_bracket = reader.match(lambda s: s == "(")
        ast = parser_fn(reader)
        close_bracket = reader.match(lambda s: s == ")")
        if (open_bracket and close_bracket) or ((not open_bracket) and (not close_bracket)):
            return ast
        return Error("( ... )", reader, caller)
    
    def parse_debug(self, caller, reader, parser_fn) -> AST|Error:
        reader.debug = True
        result = parser_fn(reader)
        reader.debug = False
        return result

#------------------------------------------------------------------------------
# test the parser

@log_enable
@this_is_a_test
def test_parser():
    source = Source.from_file('src/test/Hello.zero.md')
    ls = Lexer(source).lex()
    fn = sequence(keyword('feature'), set('name', identifier()),
                  optional(sequence(keyword('extends'), set('parent', identifier()))))
    parser = Parser().make(fn)
    reader = Reader(ls)
    ast = parser(reader)
    test_assert("parser", ast, """{'name': Hello, 'parent': Main}""")

def test_parse(fn, text: str) -> LexStr:
    source = Source.from_text(text)
    ls = Lexer(source).lex()
    parser = Parser().make(fn)
    reader = Reader(ls)
    return parser(reader)

# this ensures that sequences containing lists followed by something not in the list, work correctly
@this_is_a_test
@log_enable
def test_sequence_list():
    fn = sequence(list(enum("a", "b")), keyword("{indent}"))
    # this should succeed because the list is followed by {indent}
    test_assert("good_list", test_parse(fn, "a b {"), "{'_list': [a, b]}")
    # this should fail because the list contains an erroneous item ("c") not in the enum
    test_assert("bad_list", test_parse(fn, "a b c {"), "zeta.py:946: Expected one of ('a', 'b') at 1:4")

#------------------------------------------------------------------------------
# pretty-print AST using clickable file/line numbers from original source code

@log_disable
def show_ast(ast, filename:str=""):
    if err(ast): return ast
    log()
    global s_lex_verbose
    s_lex_verbose = True
    ast = str(ast)
    s_lex_verbose = False
    log(ast)
    log()

    def split_ast(ast:str) -> List[Tuple[int, str]]: # split wherever you find "label:", index with line numbers
        log("split_ast")
        split_ast = re.sub(r"'(\w+)':\s*", r"\n'\1': ", ast).split("\n")
        lines = []
        for line in split_ast:
            pattern = r'\*\*(\d+)>>'
            matches = re.findall(pattern, line)
            numbers = [int(match) for match in matches]
            lowest_number = min(numbers) if numbers else 0
            cleaned = re.sub(pattern, '', line)
            log("   ", lowest_number, ":", cleaned)
            lines.append((lowest_number, cleaned))
        return lines

    def combine(lines): # given an array of lines with 0: or n:, combine into one
        linenos = [line[0] for line in lines if line[0] != 0]
        minline = min(linenos) if linenos else 0
        content = "".join([line[1] for line in lines])
        return (minline, content)
    
    def find_next_type(lines, i=0): # returns the index of the next line containing '_type'
        lineno = 0
        for i in range(i, len(lines)):
            if lines[i][0] != 0:
                lineno = lines[i][0]
                break
        for j in range(i, len(lines)):
            if "_type" in lines[j][1] or lines[j][0]>lineno:
                return j
        return len(lines)
    
    lines = split_ast(ast)
    log()
    i_type = 0
    all_combined = []
    while i_type < len(lines):
        i_next_type = find_next_type(lines, i_type+1)
        combined = combine(lines[i_type:i_next_type])
        log(combined[0], ":", combined[1])
        all_combined.append(combined)
        i_type = i_next_type

    log()
    out = ""
    for c in all_combined:
        loc = log_grey(f"{filename}:{c[0]}:")
        out += f"{loc} {c[1]}\n"
    log(out)
    return out

#------------------------------------------------------------------------------
# parse rules for zero

class Grammar: pass

class Zero(Grammar):
    # feature (name) [extends (parent)] { list(function | struct | variable) }
    def feature(self) -> Rule:
        return label('feature', sequence(
                    keyword('feature'), set('name', identifier()),
                    optional(sequence(keyword('extends'), set('parent', identifier()))),
                    set('components', 
                        block(list(self.component(), sep="{newline}")))))

    def component(self) -> Rule:
        return any(self.function(), self.struct(), self.variable(), self.test())
    
    # ">" blah [ "=>" blah]
    def test(self) -> Rule:
        return label('test', sequence(
                    keyword(">"), 
                    set("expression", upto("{newline}", "{undent}", "=>")),
                    optional(sequence(
                        keyword("=>"), 
                        set("result", upto("{newline}", "{undent}"))))))
    
    # (on/replaceafter/before) (result) (name) (parameters) { function_body }
    def function(self) -> Rule:
        return label('function', sequence(
            set('modifier', enum('on', 'replace', 'after', 'before')),
            set('result', maybe_bracketed(self.name_type())),
            set('assign_op', enum("=", "<<")), 
            set('signature', self.function_signature()),
            set('body', block(self.function_body()))))

    # function signature: any sequence of (word/param)
    def function_signature(self) -> Rule:
        return list(any(set('word', identifier()), 
                        set('params', brackets(list(self.variable(), ",")))))
    
    # stand-in for now
    def function_body(self) -> Rule:
        return upto("{undent}")
        
    # struct (name) { list(variable) }
    def struct(self) -> Rule:
        return label('struct', sequence(
                    set('modifier', enum('struct', 'extends')), set('name', identifier()),
                    block(list(self.variable()))))

    # ((name type) | (type:name)) [= value]
    def variable(self) -> Rule:
        return label('variable', sequence(
                    self.name_type(),
                    optional(sequence(keyword('='), set('value', self.constant())))))

    def name_type(self) -> Rule:
        return any(
                sequence(set('name', identifier()), keyword(':'), set('type', identifier())),
                sequence(set('type', identifier()), set('name', identifier())))
        
    def constant(self) -> Rule:
        return upto("{newline}", ",", ";")

@this_is_a_test
def test_zero_grammar():
    filename = "src/test/Hello.zero.md"
    zero = Zero()
    source = Source.from_file(filename)
    lexer = Lexer(source)
    ls = lexer.lex()
    parser = Parser().make(zero.feature())
    reader = Reader(ls)
    ast = parser(reader)
    ast = show_ast(ast, filename)
    test_assert("parse_feature", ast, """
        src/test/Hello.zero.md:...: {'_type': 'feature', 'name': Hello, 'parent': Main, 'components': [{
        src/test/Hello.zero.md:...: '_type': 'test', 'expression': hello ( ), 
        src/test/Hello.zero.md:...: 'result': "hello_world"}, {'_type': 'test', 'expression': hello ( )}, {
        src/test/Hello.zero.md:...: '_type': 'function', 'modifier': on, 'result': {'name': out$, 'type': string}, 'assign_op': <<, 'signature': [{'word': hello}, {'params': []}], 
        src/test/Hello.zero.md:...: 'body': out$ << "hello_world"}]}
    """)

    function_parser = Parser().make(zero.function())
    function_ast_cpp = show_ast(function_parser(reader), filename)
    test_assert("parse_function_cpp", function_ast_cpp, """
        src/test/Hello.zero.md:...: {'_type': 'function', 'modifier': on, 'result': {'type': string, 'name': out$}, 'assign_op': <<, 'signature': [{'word': hello}, {'params': []}], 
        src/test/Hello.zero.md:...: 'body': out$ << "hello_world" ;}
    """)
    function_ast_ts = show_ast(function_parser(reader), filename)
    test_assert("parse_function_ts", function_ast_ts, """
        src/test/Hello.zero.md:...: {'_type': 'function', 'modifier': on, 'result': {'name': out$, 'type': string}, 'assign_op': <<, 'signature': [{'word': hello}, {'params': []}], 
        src/test/Hello.zero.md:...: 'body': out$ << "hello_world" ;}
    """)
    function_ast_py = show_ast(function_parser(reader), filename)
    test_assert("parse_function_py", function_ast_py, """
        src/test/Hello.zero.md:...: {'_type': 'function', 'modifier': on, 'result': {'name': out$, 'type': string}, 'assign_op': <<, 'signature': [{'word': hello}, {'params': []}], 
        src/test/Hello.zero.md:...: 'body': out$ << "hello_world"}
    """)
    function_ast_blended = show_ast(function_parser(reader), filename)
    test_assert("parse_function_blended", function_ast_blended, """
        src/test/Hello.zero.md:...: {'_type': 'function', 'modifier': on, 'result': {'type': string, 'name': out$}, 'assign_op': <<, 'signature': [{'word': hello}, {'params': []}], 
        src/test/Hello.zero.md:...: 'body': out$ << "hello_world"}
    """)

#------------------------------------------------------------------------------
# system management utilities

class System:
    # runs a shell command, optionally processes output+errors line by line, returns collected output
    @staticmethod
    def run_process(cmd: List[str], processFn=(lambda x: x)) -> str:
        collected_output = ""
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

        # Helper function to process and log output
        def process_output(stream, append_to):
            nonlocal collected_output
            while True:
                line = stream.readline()
                if line:
                    processed_line = processFn(line).strip()
                    log(processed_line)
                    append_to.append(processed_line + '\n')
                else:
                    break

        # Using lists to collect output as strings are immutable
        stdout_output = []
        stderr_output = []

        # Start threads to handle stdout and stderr
        stdout_thread = Thread(target=process_output, args=(process.stdout, stdout_output))
        stderr_thread = Thread(target=process_output, args=(process.stderr, stderr_output))

        stdout_thread.start()
        stderr_thread.start()
        stdout_thread.join()
        stderr_thread.join()

        # Collect final outputs
        collected_output = ''.join(stdout_output) + ''.join(stderr_output)
        return collected_output

    # returns the correct shell config file path depending on the shell in use
    @staticmethod
    def get_shell_config_file():
        shell = os.environ.get('SHELL', '')
        home = os.path.expanduser('~')
        if 'zsh' in shell:
            return os.path.join(home, '.zshrc')
        elif 'bash' in shell:
            return os.path.join(home, '.bash_profile')
        else:
            return os.path.join(home, '.profile')

    # ensures that new_path is added to the PATH variable
    @staticmethod
    def update_PATH(new_path):
        shell_config = System.get_shell_config_file()
        path_entry = f'export PATH="{new_path}:$PATH"'
        
        # Check if path already exists in the file
        with open(shell_config, "r") as f:
            content = f.read()
            if new_path in content:
                log(f"PATH entry for {new_path} already exists in {shell_config}")
                return

        # If not, append it to the file
        with open(shell_config, "a") as f:
            f.write(f'\n{path_entry}\n')
        
        log(f"Updated {shell_config} with new PATH entry: {new_path}")

        # Source the shell configuration file
        source_command = f"source {shell_config}"
        subprocess.run(source_command, shell=True, executable="/bin/bash")
        
        # Update current environment
        os.environ["PATH"] = f"{new_path}:{os.environ['PATH']}"

#------------------------------------------------------------------------------
# Printer helpers

# Writer is the counterpart to Reader: it writes out to a Source file
class Writer:
    def __init__(self, source: Source, indent_char: str="{"):
        self.source = source
        self.indent_char = indent_char
        self.undent_char = "}" if indent_char == "{" else ""
        self.indent_level = 0
        self.line_start = True

    def write(self, out: str|LexStr) -> bool:
        if isinstance(out, LexStr):
            for lex in out:
                self.source.source_map.append((len(self.source.text), lex.location()))
                self.space(str(lex)[0])
                self.source.text += str(lex)
                self.line_start = False
        else:
            if out == '{newline}': return self.newline()
            self.space(str(out)[0])
            self.source.text += out
            self.line_start = False
        return True

    def indent(self):
        self.indent_level += 1
        if self.indent_char != '': 
            c = self.source.text[-1] if len(self.source.text) > 0 else ''
            if c != ' ': self.source.text += ' '
            self.write(self.indent_char)
        self.newline()
        return True

    def undent(self):
        self.indent_level -= 1
        self.newline()
        if self.undent_char != "":
            self.write(self.undent_char)
        return True
    
    def newline(self):
        self.source.text += "\n" + "    " * self.indent_level
        self.line_start = True
        return True
    
    def get_position(self):
        return len(self.source.text)
    
    def restore_position(self, pos: int):
        self.source.text = self.source.text[:pos]
        self.source.source_map = [x for x in self.source.source_map if x[0] < pos]
        return True
    
    def space(self, next: str): # print a space if required to separate next from prev
        if len(self.source.text) ==0 or self.line_start == True: return
        c = self.source.text[-1]
        space_needed = is_alphanum_or_digit(c) or is_alphanum_or_digit(next) or is_operator(next) or is_operator(c)
        if space_needed: self.source.text += " "


#------------------------------------------------------------------------------
# Printer

# Printer prints out an AST using grammar rules
class Printer:
    def __init__(self):
        pass

    def make(self, rule: Any) -> Callable:
        try:
            if rule == None: return None
            if isinstance(rule, str) or isinstance(rule, LexStr): return rule
            if isinstance(rule, List) or isinstance(rule, Tuple): 
                print("rule is list or tuple:", rule)
                exit(0)
            caller = rule['caller'] if 'caller' in rule else None
            if 'fn' not in rule: raise Exception(f"no 'fn' in rule {rule}")
            fn = rule['fn']
            method = getattr(self, "print_" + fn, None)
            if not method: raise Exception(f"no method '{fn}' in {self.__class__.__name__}")
            args = []
            for key, val in rule.items():
                if not (key in ['caller', 'fn']):
                    if isinstance(val, List) or isinstance(val, Tuple): 
                        args.extend([self.make(v) for v in val])
                    else:
                        args.append(self.make(val))
            return lambda writer, ast: method(caller, writer, ast, *args)
        except Exception as e:
            print(exception_message(e))
            print("problematic rule:")
            rule = caller_get_arg(1, 'rule')
            print(rule_as_string(rule))
            exit(0)

    def print_keyword(self, caller, writer, ast: AST, word: str) -> bool:
        return writer.write(word)

    def print_indent(self, caller, writer, ast: AST) -> bool:
        return writer.indent()

    def print_undent(self, caller, writer, ast: AST) -> bool:
        return writer.undent()

    def print_newline(self, caller, writer, ast: AST) -> bool:
        return writer.newline()

    def print_identifier(self, caller, writer, ast: AST|LexStr) -> bool:
        if not isinstance(ast, LexStr): print("oops")
        return writer.write(ast) if isinstance(ast, LexStr) else False

    def print_label(self, caller, writer, ast: AST, label: str, printer_fn: Callable) -> bool:
        if (not isinstance(ast, AST)) or (not '_type' in ast) or (ast['_type'] != label): 
            return False
        return printer_fn(writer, ast)

    def print_set(self, caller, writer, ast: AST, name: str, printer_fn: Callable) -> bool:
        if not name in ast:
            return False
        return printer_fn(writer, ast[name])

    def print_sequence(self, caller, writer, ast: AST, *printer_fns: List[Callable]):
        for printer_fn in printer_fns:
            success = printer_fn(writer, ast)
            if not success: 
                return False
        return True

    def print_optional(self, caller, writer, ast: AST, printer_fn: Callable):
        pos = writer.get_position()
        success = printer_fn(writer, ast)
        if not success:
            writer.restore_position(pos)
        return True
    
    def print_enum(self, caller, writer, ast: AST|LexStr, *words: List[str]) -> bool:
        return writer.write(ast) if isinstance(ast, LexStr) and str(ast) in words else False
    
    def print_any(self, caller, writer, ast: AST, *printer_fns: List[Callable]) -> bool:
        pos = writer.get_position()
        for printer_fn in printer_fns:
            if printer_fn(writer, ast):
                return True
            writer.restore_position(pos)
        return False

    def print_list(self, caller, writer, ast: AST, printer_fn: Callable, sep: str) -> bool:
        for i, item in enumerate(ast):
            printer_fn(writer, item)
            if sep:
                if (i < len(ast) - 1): writer.write(sep)
        return True
    
    def print_block(self, caller, writer, ast: AST, printer_fn: Callable) -> bool:
        writer.indent()
        success = printer_fn(writer, ast)
        writer.undent()
        return True
    
    def print_upto(self, caller, writer, ast: AST, *words: List[str]) -> bool:
        if isinstance(ast, LexStr):
            writer.write(ast)
            return True
        return False
    
    def print_brackets(self, caller, writer, ast: AST, printer_fn: Callable) -> bool:
        pos = writer.get_position()
        writer.write("(")
        success = printer_fn(writer, ast)
        if not success:
            writer.restore_position(pos)
            return False
        writer.write(")")
        return True
    
    def print_maybe_bracketed(self, caller, writer, ast: AST, printer_fn: Callable) -> bool:
        return self.print_brackets(caller, writer, ast, printer_fn)
    
    def print_debug(self, caller, writer, ast: AST, printer_fn: Callable) -> bool:
        return printer_fn(self, writer, ast)

@this_is_a_test
def test_printer():
    log("test_printer")
    ast = parse_zero("src/test/Hello.zero.md")
    source_out = Source()
    writer = Writer(source_out, indent_char="{") # braces for indents
    printer = Printer().make(Zero().feature())
    printer(writer, ast)
    test_assert("print_feature", source_out.text, """
        feature Hello extends Main {
            > hello () => "hello world"
            > hello ()
            on ( out$ : string ) << hello () {
                out$ << "hello world"
            }
        }
    """)
    source_out_py = Source()
    writer = Writer(source_out_py, indent_char=":") # python-style indents
    printer(writer, ast)
    test_assert("print_feature_py", source_out_py.text, """
        feature Hello extends Main :
            > hello () => "hello world"
            > hello ()
            on ( out$ : string ) << hello () :
                out$ << "hello world"
    """)
    source_out_blended = Source()
    writer = Writer(source_out_blended, indent_char="") # supercool blended indents
    printer(writer, ast)
    test_assert("print_feature_blended", source_out_blended.text, """
          feature Hello extends Main
    > hello () => "hello world"
    > hello ()
    on ( out$ : string ) << hello ()
        out$ << "hello world"
          """)

#------------------------------------------------------------------------------
# target languages

# Language represents a target programming language, eg. typescript / C++ / Python / Swift
class Language:
    def ext(self): pass
    def indent_char(self): pass

class Typescript(Language):
    def ext(self): return "ts"
    def indent_char(self): return "{"
    def function(self):
        return sequence(
            keyword('function'), set('name', identifier()),
            set('args', brackets(list(self.variable(), ","))),
            optional(sequence(keyword(':'), set('result', set('type', identifier())))),
            block(set('body', self.function_body())))
    
    def variable(self):
        return sequence(
            self.name_type(),
            optional(sequence(keyword('='), set('value', self.constant()))))
    
    def name_type(self):
        return sequence(set('name', identifier()), keyword(':'), set('type', identifier()))
    
    def constant(self):
        return upto(",", ";", ")", "{newline}")
    
    def function_body(self):
        return upto("{undent}")
    
    def component(self): pass

#------------------------------------------------------------------------------
# target backends

# a Backend represents a specific target platform or runtime, eg. Deno, Node.js, etc

class Backend:
    def install(self) -> bool: pass
    def setup(self, project_path: str): pass
    def run(self, processFn, filename: str, options: List[str]=[])->str: pass

#--------------------------------------------------------------------------------
# Deno backend

import json

class Deno(Backend):
    def install(self):
        def check_version() -> str:
            try:
                # Check both the system PATH and the user's .deno/bin directory
                deno_path = shutil.which("deno")
                if deno_path:
                    result = subprocess.run([deno_path, "--version"], check=True, capture_output=True, text=True)
                else:
                    home_dir = os.path.expanduser("~")
                    deno_bin = os.path.join(home_dir, ".deno", "bin", "deno")
                    if os.path.exists(deno_bin):
                        result = subprocess.run([deno_bin, "--version"], check=True, capture_output=True, text=True)
                    else:
                        return None
                version_output = result.stdout
                version_match = re.search(r"deno (\d+\.\d+\.\d+)", version_output)
                if version_match:
                    return version_match.group(1)
                else:
                    return None
            except (subprocess.CalledProcessError, FileNotFoundError):
                return None
            
        def get_latest_version() ->str:
            try:
                response = requests.get("https://github.com/denoland/deno/releases/latest")
                latest_version = response.url.split('/')[-1].lstrip('v')
                return latest_version
            except requests.RequestException:
                log("Failed to fetch the latest version. Please check your internet connection.")
                return None
            
        def install_latest_version():
            try:
                log("Starting Deno installation...")
                # Using curl to download and run the Deno installer
                install_command = "curl -fsSL https://deno.land/x/install/install.sh | sh"
                result = subprocess.run(install_command, shell=True, check=True, capture_output=True, text=True)
                log("Installer output:")
                log(result.stdout)
                # Update PATH
                deno_path = os.path.expanduser("~/.deno/bin")
                System.update_PATH(deno_path)
                log("Deno installation completed and PATH updated.")
                return True
            except subprocess.CalledProcessError as e:
                log(f"Failed to install Deno: {e}")
                log("Error output:")
                log(e.stderr)
                return False

        def ensure_latest_version(self) -> bool:
            current_version = self.check_version()
            latest_version = self.get_latest_version()
            if latest_version == None:
                log("failed to determine latest version of Deno")
                return False 
            if current_version == None or current_version != latest_version:
                log(f"deno: current version is {current_version}, latest version is {latest_version}")
                log(f"installing Deno version {latest_version}")
                self.install_latest_version()
                return True
            log("deno is up to date :-)")

        return ensure_latest_version()

    def setup(self):
        project_path = "build/deno"
        # Create the project directory if it doesn't exist
        os.makedirs(project_path, exist_ok=True)
        # Create deno.json configuration file
        deno_config = {
            "compilerOptions": {
                "allowJs": True,
                "lib": ["deno.window"]
            },
            "lint": {
                "files": {
                    "include": ["src/"]
                },
                "rules": {
                    "tags": ["recommended"]
                }
            },
            "fmt": {
                "files": {
                    "include": ["src/"]
                },
                "options": {
                    "useTabs": False,
                    "lineWidth": 80,
                    "indentWidth": 4,
                    "singleQuote": True,
                    "proseWrap": "always"
                }
            }
        }
        with open(f'{project_path}/deno.json', 'w') as f:
            json.dump(deno_config, f, indent=2)
        # Create import_map.json file
        import_map = {
            "imports": {}
        }
        with open(f'{project_path}/import_map.json', 'w') as f:
            json.dump(import_map, f, indent=2)
        log(f"Deno project initialized in {project_path}")

    def run(self, processFn, filename: str, options: List[str]=[])->str:
        if not os.path.exists(filename):
            return f"Error: File not found: {filename}", ""
        try:
            cmd = ['deno', 'run', '--allow-all', filename, *options]
            return System.run_process(cmd, processFn)
        except FileNotFoundError:
            raise
            return "Error: Deno is not installed or not in the system PATH.", ""
        except Exception as e:
            traceback.print_exc()
            print(f"An unexpected error occurred: {str(e)}")

#------------------------------------------------------------------------------
# parse, generate code, test code (typescript/deno for the moment)
    
# move the code all the way over to the left
def detab(text: str) -> str:
    lines = text.split("\n")
    min_indent = 10000
    for line in lines:
        if line.strip() == "": continue
        indent = len(line) - len(line.lstrip())
        min_indent = min(min_indent, indent)
    out = ""
    for line in lines:
        out += line[min_indent:] + "\n"
    return out.strip()

# parse a file, return the AST
@log_disable
def parse_zero(filename: str) -> AST:
    filename = "src/test/Hello.zero.md"
    source = Source.from_file(filename)
    lexer = Lexer(source)
    ls = lexer.lex()
    parser = Parser().make(Zero().feature())
    reader = Reader(ls)
    ast = parser(reader)
    log(show_ast(ast, filename))
    return ast

# separate components by type; so functions => ['functions'], etc.
def separate_components(feature_ast: AST):
    for component in feature_ast['components']:
        type = component['_type']
        listname = type + "s"
        if not listname in feature_ast: feature_ast[listname] = []
        feature_ast[listname].append(component)

# read function signature to generate name, arguments
def process_function(function_ast: AST):
    sig = function_ast['signature']
    name = LexStr([])
    args = []
    # I'm too tired. go to bed.
    # in the middle of: checking assign_op, if "<<" then add first param = stream<result>
    # then we can get onto stream<T>.push() and off we go.

    for i, item in enumerate(sig):
        if 'word' in item: 
            name += item['word']
        elif 'params' in item:
            for p in item['params']: 
                args.append(p)

    function_ast['name'] = name
    function_ast['args'] = args

# generate code in the target language from the AST
def generate_code(language: Language, context_name: str, ast: AST) -> str:
    source = Source()
    writer = Writer(source, indent_char=language.indent_char())
    separate_components(ast)
    for f in ast['functions']: 
        process_function(f)

        print(f)
        printer = Printer().make(language.function())
        printer(writer, f)
        print("function code:")
        print(source.text)

    code = f"""
{source.text}
function main() {{
    console.log("ᕦ(ツ)ᕤ {context_name}.ts")
}}
main()"""
    return detab(code)

# find the filename for the context in the right place for the backend
def context_path(language: Language, backend: Backend, context_name: str) -> str:
    folder_name = backend.__class__.__name__.lower()
    return f"build/{folder_name}/{context_name}.{language.ext()}"

# save code to a file in the correct place for the backend
def save_code(language: Language, backend: Backend, context_name: str, code: str):
    path = context_path(language, backend, context_name)
    write_file(path, code)

def remove_ansi_sequences(text):
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    return ansi_escape.sub('', text)

def process_log(output: str, source_map = None) -> str:
    output = remove_ansi_sequences(output)
    def map_function(file, line, char):
        if source_map == None:
            return f"{file}:{line}:{char}"
        else:
            return source_map(file, line, char)
    def replace_callback(match):
        file, line, char = match.groups()
        return map_function(file, line, char)
    # Assuming 'content' contains your file contents
    pattern = r'file://([^:]+):(\d+):(\d+)'
    new_output = re.sub(pattern, replace_callback, output)
    return new_output

# test the code using the backend
def test_code(language: Language, backend: Backend, context_name: str) -> str:
    path = context_path(language, backend, context_name)
    processFn = lambda line: process_log(line, None) # todo: add source map :-)
    output = backend.run(processFn, path, ["-test"])
    return output
    
@this_is_a_test
@log_enable
def test_den():
    print("\ntest_deno ---------------------------------")
    filename = "src/test/Hello.zero.md"
    context_name = "test"
    language = Typescript()
    backend = Deno()
    ast = parse_zero(filename)
    code = generate_code(language, context_name, ast)
    print("code:")
    print(code)
    save_code(language, backend, context_name, code)
    print("\ntest:")
    output = test_code(language, backend, context_name)
   
#------------------------------------------------------------------------------
# main, test, etc

def main():
    print('ᕦ(ツ)ᕤ zeta.py')
    test_run_all()
          
if __name__ == '__main__':
    main()
    print("done.")