# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo

from typing import List, Tuple
import inspect
import os
import re

#------------------------------------------------------------------------------
# kenny loggings

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
    sa = log_strip(str(a))
    sb = log_strip(b).rstrip()
    if sb.startswith("\n"): sb = sb[1:]
    ctx = context()
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
# context: figure out which file/line called us

s_cwd = os.getcwd() + "/src/"

# returns the (file, line) of the function that called our caller
def context():
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to context(), so...
    file = frame.filename.replace(s_cwd, '')    # dunno if we totally need this but 
    return (file, frame.lineno)                 # easier to process this way

#------------------------------------------------------------------------------
# file system 

# read a file
def readFile(path: str) -> str:
    with open(path, "r") as file:
        return file.read()
    
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
# structure-based parsing and printing!
# these are the "atoms" out of which parsers are built
# we store them as dictionaries, so we can use them to parse, print, or do other things

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

#------------------------------------------------------------------------------
# print a parser rule in a nice way

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

def feature() -> dict:
    return label('feature', seq(
                kw('feature'), set('name', id()),
                opt(seq(kw(':'), set('parent', id()))),
                block(list(any(function(), struct(), variable())))))

def function() -> dict:
    return label('function', seq(
        set('modifier', enum('on', 'replace', 'after', 'before')),
        set('result', name_type()),
        kw('='), set('name', id()),
        kw('('), set('parameters', list(variable(), sep=',', term=')')), kw(')'),
        kw('{'), set('body', function_body()), kw('}')))

def function_body() -> dict:
    return {'fn': 'function_body'}
    
def struct() -> dict:
    return label('struct', seq(
                kw('struct'), set('name', id()),
                block(list(variable))))

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

#------------------------------------------------------------------------------
# main, test, etc

def test():
    print("test ------------------------------------")
    log_assert("rule_as_string",
        rule_as_string(feature()), """
             feature = 
zeta.py:198: seq(
zeta.py:199:     kw('feature'), set('name', id()), 
zeta.py:200:     opt(seq(kw(':'), set('parent', id()))), 
zeta.py:201:     block(list(any(function(), struct(), variable()))))
               """)
def main():
    log_clear()
    print('ᕦ(ツ)ᕤ zeta.py\n')
    test()
          
if __name__ == '__main__':
    main()
    print("\ndone.")





