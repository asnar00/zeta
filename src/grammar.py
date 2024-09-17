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

test_grammar_spec = """
    expression = (constant | variable | brackets | operation | function)
    constant = (<number> | <string>)
    variable = name:<identifier>
    brackets = "(" expr:expression ")"
    operation = (prefix | infix | postfix)
    prefix = operator:<operator> expr:expression
    infix = left:expression operator:<operator> right:expression
    postfix = expr:expression operator:<operator>
    function = name:<identifier> "(" args:(argument ,)* ")"
    argument = (argument_name)? value:expression
    argument_name = name:<identifier> "="
    """

@this_is_a_test
def test_grammar_setup():
    log("test_grammar_setup")

    test("grammar_setup", Grammar(test_grammar_spec), """
expression = OneOf(Ref("constant"), Ref("variable"), Ref("brackets"), Ref("operation"), Ref("function"))
constant = OneOf(Type("number"), Type("string"))
variable = name:Type("identifier")
brackets = Keyword("("), expr:Ref("expression"), Keyword(")")
operation = OneOf(Ref("prefix"), Ref("infix"), Ref("postfix"))
prefix = operator:Type("operator"), expr:Ref("expression")
infix = left:Ref("expression"), operator:Type("operator"), right:Ref("expression")
postfix = expr:Ref("expression"), operator:Type("operator")
function = name:Type("identifier"), Keyword("("), args:ZeroOrMore(Ref("argument"), ","), Keyword(")")
argument = Optional(Ref("argument_name")), value:Ref("expression")
argument_name = name:Type("identifier"), Keyword("=")
         """)

#--------------------------------------------------------------------------------------------------
# Terms

class Term:
    def __init__(self):
        self.set_variable = None
        self.term_in_rules = []
    def var(self): return f"{self.set_variable}:" if self.set_variable else ""
    def __repr__(self): return str(self)
    def add_rule(self, rule, iTerm: int):
        self.term_in_rules.append((rule, iTerm))
    
class Terminal(Term): pass

class Type(Terminal):
    def __init__(self, type_name): super().__init__(); self.type_name = type_name
    def __str__(self): return f"{self.var()}Type(\"{self.type_name}\")"

class Keyword(Terminal):
    def __init__(self, word): super().__init__(); self.word = word
    def __str__(self): return f"{self.var()}Keyword(\"{self.word}\")"

class OneOf(Term):
    def __init__(self, terms: List[Term]): super().__init__(); self.terms = terms
    def __str__(self):
        return f"{self.var()}OneOf(" + ", ".join([str(term) for term in self.terms]) + ")"

class Optional(Term):
    def __init__(self, term: Term): super().__init__(); self.term = term
    def __str__(self): return f"{self.var()}Optional({self.term})"

class ZeroOrMore(Term):
    def __init__(self, term: Term, sep: str=""): 
        super().__init__()
        self.term = term; self.sep = Keyword(sep) if sep else None
    def __str__(self): 
        sep = f", \"{self.sep.word}\"" if self.sep else ""
        return f"{self.var()}ZeroOrMore({self.term}{sep})"

class Ref(Term):
    def __init__(self, name): super().__init__(); self.name = name
    def __str__(self): return f"{self.var()}Ref(\"{self.name}\")"

#--------------------------------------------------------------------------------------------------
# Grammar

class Rule:
    def __init__(self, name = "", terms = []):
        self.name = name
        self.terms = terms
    def __str__(self):
        return self.name + " = " + ", ".join([str(term) for term in self.terms])
    def __repr__(self): return str(self)


class Grammar:
    def __init__(self, grammar_spec: str):
        self.rules = {}         # name => Rule
        self.keyword_map = {}   # word => List[List[Rule]]
        self.type_map = {}      # type => List[List[Rule]]
        self.rule_map = {}      # rule_name => List[List[Rule]]
        self.setup(grammar_spec)
        self.setup_map()

    def __str__(self):
        out = ""
        for rule in self.rules.values():
            out += f"{rule}\n"
        return out
    
    def __repr__(self): return str(self)

    # parse a grammar spec (text) 
    def setup(self, gs: str):
        lines = gs.split("\n")
        lines = [line.strip() for line in lines]
        lines = [line for line in lines if line]
        for line in lines:
            rule = parse_rule(line)
            self.rules[rule.name] = rule

    # find all rules that accept (lex) at position (i_term)
    def find_terminal_rules(self, lex: Lex, i_term: int) -> List[Rule]:
        if lex.val in self.keyword_map:
            if i_term >= len(self.keyword_map[lex.val]): return []
            return (self.keyword_map[lex.val])[i_term]
        elif lex.type in self.type_map:
            if i_term >= len(self.type_map[lex.type]): return []
            return (self.type_map[lex.type])[i_term]
        return []
    
    # find rules that accept some ast(rule_name) in position i_term
    def find_nonterminal_rules(self, rule_name: str, i_term: int) -> List[Rule]:
        if rule_name in self.rule_map:
            if i_term >= len(self.rule_map[rule_name]): return []
            return (self.rule_map[rule_name])[i_term]
        return []
    
    # process the grammar to build a fast map from (type/keyword/rule) => (rule, i_term)
    def setup_map(self):
        for rule in self.rules.values():
            for i, term in enumerate(rule.terms):
                key_terms = self.find_key_terms(term)
                for key_term in key_terms:
                    if isinstance(key_term, Keyword):
                        self.update_list(self.keyword_map, key_term.word, i, rule)
                    elif isinstance(key_term, Type):
                        self.update_list(self.type_map, key_term.type_name, i, rule)
                    elif isinstance(key_term, Ref):
                        self.update_list(self.rule_map, key_term.name, i, rule)
        self.add_nodal_rules()

    # where a rule maps to a single nodal rule, transfer its maps
    def add_nodal_rules(self):
        for rule_name, rules in self.rule_map.items():
            # does this map to a nodal rule?
            if len(rules) == 1 and len(rules[0]) == 1:
                rule = rules[0][0]
                if self.is_nodal_rule(rule):
                    self.rule_map[rule_name] = self.rule_map[rule.name]

    # returns true if the rule is "nodal" - is a OneOf with only one term
    def is_nodal_rule(self, rule: Rule) -> bool:
        return len(rule.terms) == 1 and isinstance(rule.terms[0], OneOf)
    
    def update_list(self, m: Dict, key: str, index: int, rule: Rule):
        if key not in m: m[key] = []
        if len(m[key]) <= index:
            n_add = (index + 1) - len(m[key])
            m[key] += [[] for _ in range(n_add)]
        m[key][index].append(rule)

    def find_key_terms(self, term: Term) -> List[Term]:
        if isinstance(term, Type): return [term]
        elif isinstance(term, Keyword): return [term]
        elif isinstance(term, OneOf): 
            key_terms = []
            for t in term.terms:
                key_terms += self.find_key_terms(t)
            return key_terms
        elif isinstance(term, Optional): return [self.find_key_terms(term.term)]
        elif isinstance(term, ZeroOrMore): 
            key_terms = self.find_key_terms(term.term)
            if term.sep: key_terms += [term.sep]
            return key_terms
        elif isinstance(term, Ref): return [term]
        return term
    
    def show_map(self, m: Dict):
        out = ""
        for key, rules in m.items():
            out += f"{key} => "
            for i, rule_list in enumerate(rules):
                if len(rule_list) > 0:
                    for rule in rule_list:
                        full_match = "*" if len(rule.terms) == 1 else ""
                        out += f"{full_match}{rule.name}:{i} "
            out += "\n"
        log(out)

def parse_rule(line: str):
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
        return OneOf([parse_term(option) for option in options])
    
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