# ᕦ(ツ)ᕤ
# grammar.py
# author: asnaroo
# zero to anything

from typing import List, Type, Tuple
from util import *
from typing import get_type_hints

#--------------------------------------------------------------------------------------------------
# Entity is the base class for all AST Nodes

# Entity is the base class; all entities have a name and an alias
class Entity:
    def __init__(self): pass

#--------------------------------------------------------------------------------------------------
# Term: a single term of a rule

class Term:
    def __init__(self, var: str, vals: List[str], dec:str ="", sep:str =""):
        self.var = var      # variable to assign in the AST, if any
        self.vals = vals    # a set of keywords ("word"), types (<type>), or rules (Rule)
        self.dec = dec      # one of "?" (zero or one), "*" (zero or more) or "+" (one or more)
        self.sep = sep      # separator if we are "+" or "*"
        self.initials = []  # keywords that can initiate this term
        self.followers = [] # keywords that can follow this term
        self.leaves = {}    # maps match => rule-names
        self.index = 0      # index in the original term list
        self.rule = None    # the rule we're in, if any
        self.contains_nested_sep = False     # True if this term points to sub-rules with the same separator
    def __str__(self):
        out = " | ".join(self.vals)
        if len(self.vals) > 1: out = f"({out})"
        if self.dec: out += self.dec
        if self.sep: out += self.sep
        if self.var: out = f"{self.var}:{out}"
        return out.replace('"', "'")
    def __repr__(self): return self.__str__()
    def is_keyword(self): return self.vals[0][0] == '"'
    def is_type(self): return self.vals[0][0] == '<'
    def is_terminal(self): return (self.is_keyword() or self.is_type())
    def is_rule(self): return not (self.is_keyword() or self.is_type())
    def is_singular(self): return self.dec == ""
    def is_optional(self): return self.dec == "?"
    def is_list(self): return self.dec and self.dec in "*+"
    def rules(self): return [Grammar.current.rule_named[r] for r in self.vals]


#--------------------------------------------------------------------------------------------------
# Rule: a name and a list of terms

class Rule:
    def __init__(self, name: str, rhs: str, cls=None):
        self.cls = cls
        self.name = name
        self.rhs = rhs
        self.terms = []
        self.initials = []      # keywords that can start this rule
        self.followers = []     # keywords that can follow this rule
        self.leaves = {}        # maps match => rule-names
    def __str__(self):
        return self.name
    def __repr__(self): return self.__str__()
    def dbg(self):
        out = self.name + " := " + " ".join([str(term) for term in self.terms])
        return out
    def is_abstract(self):
        return len(self.terms)==1 and self.terms[0].is_rule() and not self.terms[0].dec
    

#--------------------------------------------------------------------------------------------------
# Grammar

class Grammar:
    def __init__(self): 
        self.rules : List[Rule] = []
        self.rule_named = {}
        self.build()

    def dbg(self):
        out = ""
        for rule in self.rules:
            out += rule.dbg() + "\n"
        return out

    def build(self):
        entity_subclasses = all_subclasses_rec(Entity)
        for cls in entity_subclasses:
            rule = Rule(cls.__name__, cls().rule() if hasattr(cls, "rule") else "", cls)
            self.add_rule(rule)
        for rule in self.rules:
            if len(rule.terms) == 0: self.build_rule(rule)
            else: log("already built:", rule.name)

    
    def build_rule(self, rule: Rule):
        if rule.rhs == "": self.build_abstract(rule)
        else: self.build_concrete(rule)

    @log_disable
    def build_abstract(self, rule: Rule):
        subclasses = all_subclasses_rec(rule.cls)
        log("subclasses:", subclasses)
        vals = [subcls.__name__ for subcls in subclasses]
        rule.terms = [Term(var=None, vals=vals)]
        log(rule.dbg())

    @log_indent
    def build_concrete(self, rule: Rule):
        log("rhs:", rule.rhs)
        term_strs = split_terms(rule.rhs)
        log("term_strs:", term_strs)
        for term_str in term_strs:
            term = self.build_term(rule, term_str)
            rule.terms.append(term)
        pass

    @log_indent
    def build_term(self, rule: Rule, term_str: str) -> Term:
        parts = re.match(r"(\w+):(.+)", term_str)
        var = parts.group(1) if parts else None
        log("var:", var)
        term_str = parts.group(2) if parts else term_str    
        decs = "&?*+,;|"
        dec = ""
        sep = ""
        if len(term_str) > 1 and term_str[-1] in decs:
            dec = term_str[-1]
            term_str = term_str[:-1]
        if len(term_str) > 1 and term_str[-1] in decs:
            sep = dec
            dec = term_str[-1]
            term_str = term_str[:-1]
        log("term_str:", term_str, "dec:", dec, "sep:", sep)
        if term_str.startswith("(") and term_str.endswith(")"):
            return self.build_complex_term(rule, term_str[1:-1], var, dec, sep)
        if term_str.startswith("<") or term_str.startswith("'"):
            vals = [term_str]
        else:
            var, vals = self.find_var_vals(term_str, dec, rule.cls)
        term = Term(var, vals, dec, sep)
        log("simple term:", term)
        return term
    
    @log_indent
    def build_complex_term(self, rule: Rule, term_str: str, var: str, dec: str, sep: str) -> Term:
        if " | " in term_str: # todo: this is kind of dumb
            sub_terms = [val.strip() for val in term_str.split("|")]
            log(log_red("rules case! sub_terms:"), sub_terms)
            vals = []
            for sub_term in sub_terms:
                sub_term = self.build_term(rule, sub_term)
                log("sub_term:", sub_term)
                vals.append(str(sub_term))
            log("vals:", vals)
            return Term(var, vals, dec, sep)
            

        sub_rule_name = rule.name + "_"
        while(sub_rule_name in self.rule_named):
            sub_rule_name += "_"
        sub_rule = Rule(sub_rule_name, term_str, rule.cls)
        self.add_rule(sub_rule, rule.name)
        self.build_rule(sub_rule)
        vals = [ sub_rule.name ]
        return Term(var, vals, dec, sep)

    def add_rule(self, new_rule: Rule, after_rule_name: str=None) -> Rule:
        if after_rule_name:
            after = self.rule_named[after_rule_name]
            i = self.rules.index(after)
            self.rules.insert(i+1, new_rule)
        else:
            self.rules.append(new_rule)
        self.rule_named[new_rule.name] = new_rule
        return new_rule
    
    @log_indent
    def find_var_vals(self, term_str: str, dec: str, cls: Type) -> Tuple[str, str]:
        instance = cls()
        found = hasattr(instance, term_str)
        rule_name = get_attribute_type(cls, term_str)
        log("found:", found)
        log("rule:", rule_name)
        if rule_name == None:
            raise Exception(f"class {cls.__name__} attribute '{term_str}' has no type")
        if dec == "&":
            return (term_str, ["<identifier>"])
        elif rule_name.startswith("List["):
            log("list case!")
            rule_name = rule_name[5:-1]
            log("rule_name:", rule_name)
            return [term_str, [rule_name]]
        else:
            if rule_name == "str": rule_name = "<identifier>"
            return [term_str, [rule_name]]
        raise Exception("find_var_vals: unknown case")


#--------------------------------------------------------------------------------------------------
# helpers

def all_subclasses_rec(cls: Type) -> List[Type]:
    out = all_subclasses(cls)
    for subcls in cls.__subclasses__():
        out += all_subclasses_rec(subcls)
    return out

def all_subclasses(cls: Type) -> List[Type]:
    return [subcls for subcls in cls.__subclasses__()]




    
def split_terms(rhs: str) -> List[str]:
    terms = []
    current_term = ""
    bracket_depth = 0
    in_quotes = False
    for char in rhs:
        if char == ' ' and bracket_depth == 0 and not in_quotes:
            if current_term:
                terms.append(current_term)
                current_term = ""
        else:
            if char == '(' and not in_quotes:
                bracket_depth += 1
            elif char == ')' and not in_quotes:
                bracket_depth -= 1
            elif char == "'":
                in_quotes = not in_quotes
            
            current_term += char
    if current_term:
        terms.append(current_term)
    return terms