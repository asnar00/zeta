# ᕦ(ツ)ᕤ
# grammar.py
# author: asnaroo
# zero to anything

from util import *
from lexer import *
from typing import List, Dict
from collections import namedtuple
import re

#--------------------------------------------------------------------------------------------------
# Grammar: a collection of Rules, each made of cross-referencing Terms

@this_is_the_test
def test_grammar():
    log("test_grammar")
    grammar = """
        expression = (constant | variable | brackets | operation | function)
        constant = (<number> | <string>)
        variable = <identifier>
        brackets = "(" expr:expression ")"
        operation = (prefix | infix | postfix)
        prefix = operator:<operator> expr:expression
        infix = left:expression operator:<operator> right:expression
        postfix = expr:expression operator:<operator>
        function = name:<identifier> "(" args:(argument ,)* ")"
        argument = (argument_name)? value:expression
        argument_name = <identifier> "="
        """
    test("parse_grammar", parse_grammar(grammar))

#--------------------------------------------------------------------------------------------------
# Terms

class Term:
    def __init__(self):
        self.set_variable = None
    def var(self): return f"{self.set_variable}:" if self.set_variable else ""
    
class Terminal(Term): pass

class Type(Terminal):
    def __init__(self, type_name): super().__init__(); self.type_name = type_name
    def __str__(self): return f"{self.var()}Type(\"{self.type_name}\")"

class Keyword(Terminal):
    def __init__(self, word): super().__init__(); self.word = word
    def __str__(self): return f"{self.var()}Keyword(\"{self.word}\")"

class Any(Term):
    def __init__(self, atoms: List[Term]): super().__init__(); self.atoms = atoms
    def __str__(self):
        return f"{self.var()}Any(" + ", ".join([str(term) for term in self.atoms]) + ")"

class Optional(Term):
    def __init__(self, term: Term): super().__init__(); self.term = term
    def __str__(self): return f"{self.var()}Optional({self.term})"

class ZeroOrMore(Term):
    def __init__(self, term: Term, sep: str=""): super().__init__(); self.term = term; self.sep = sep
    def __str__(self): return f"{self.var()}ZeroOrMore({self.term}, \"{self.sep}\")"

class Ref(Term):
    def __init__(self, name): super().__init__(); self.name = name
    def __str__(self): return f"{self.var()}Ref(\"{self.name}\")"

#--------------------------------------------------------------------------------------------------
# Grammar

class Grammar:
    def __init__(self):
        self.rules = {}

    def __str__(self):
        out = ""
        for rule in self.rules.values():
            out += f"{rule}\n"
        return out
    
    def __repr__(self): return str(self)

class Rule:
    def __init__(self, name = "", terms = []):
        self.name = name
        self.terms = terms
    def __str__(self):
        return self.name + " = " + ", ".join([str(term) for term in self.terms])
    def __repr__(self): return str(self)

def parse_grammar(gs: str):
    lines = gs.split("\n")
    lines = [line.strip() for line in lines]
    lines = [line for line in lines if line]
    grammar = Grammar()
    for line in lines:
        rule = parse_rule(line)
        grammar.rules[rule.name] = rule
    return grammar

def parse_rule(line: str):
    log("parse_rule", line)
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
        term.set_variable = variable_name
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
        return Any([parse_term(option) for option in options])
    
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