# ᕦ(ツ)ᕤ
# grammar.py
# author: asnaroo
# zero to anything

from typing import List, Type, Tuple, Dict
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
    def __init__(self, var: str, vals: List[str], dec:str ="", sep:str ="", ref: str = ""):
        self.var = var      # variable to assign in the AST, if any
        self.vals = vals    # a set of keywords ('word'), types (<type>), or rules (Rule)
        self.dec = dec      # one of "?" (optional), "*" (zero-or-more), "+" (one-or-more)
        self.sep = sep      # separator: one of "", ",", ";" or "|" 
        self.ref = ref      # "", or "&" if the term is a reference to an entity
        self.initials = []  # keywords that can initiate this term
        self.followers = [] # keywords that can follow this term
        self.leaves = {}    # maps match => rule-names
        self.index = 0      # index in the original term list
        self.rule = None    # the rule we're in, if any
        self.contains_nested_sep = False     # True if this term points to sub-rules with the same separator
    def __str__(self):
        out = " | ".join(self.vals)
        if len(self.vals) > 1: out = f"({out})"
        if self.ref: out += self.ref
        if self.dec: out += self.dec
        if self.sep: out += self.sep
        if self.var: out = f"{self.var}:{out}"
        return out.replace('"', "'")
    def __repr__(self): return self.__str__()
    def is_keyword(self): return self.vals[0][0] == "'" if len(self.vals) > 0 else False
    def is_type(self): return self.vals[0][0] == '<' if len(self.vals) > 0 else False
    def is_terminal(self): return (self.is_keyword() or self.is_type())
    def is_rule(self): return not (self.is_keyword() or self.is_type())
    def is_singular(self): return self.dec == ""
    def is_optional(self): return self.dec == "?"
    def is_list(self): return self.dec and self.dec in "*+"
    def is_reference(self): return self.ref == "&"
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
    current: 'Grammar' = None
    def __init__(self, cls: Type):
        Grammar.current = self
        self.rules : List[Rule] = []
        self.rule_named = {}
        self.build(all_subclasses_rec(cls))
        compute_meta_stuff()

    def dbg(self):
        out = ""
        for rule in self.rules:
            out += rule.dbg() + "\n"
        return out

    def build(self, classes: List[Type]):
        for cls in classes:
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
        subclasses = all_subclasses(rule.cls)
        vals = [subcls.__name__ for subcls in subclasses]
        rule.terms = [Term(var=None, vals=vals)]

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
        term_str, dec, sep, ref = analyse_decorators(term_str)
        log("term_str:", term_str, "dec:", dec, "sep:", sep, "ref:", ref)
        if term_str.startswith("(") and term_str.endswith(")"):
            return self.build_complex_term(rule, term_str[1:-1], var, dec, sep, ref)
        if term_str.startswith("<") or term_str.startswith("'"):
            vals = [term_str]
        else:
            var, vals = find_var_vals(term_str, dec, rule.cls)
        term = Term(var, vals, dec, sep, ref)
        log("simple term:", term)
        return term
    
    @log_indent
    def build_complex_term(self, rule: Rule, term_str: str, var: str, dec: str, sep: str, ref: str) -> Term:
        if " | " in term_str: # todo: this is kind of dumb
            sub_terms = [val.strip() for val in term_str.split("|")]
            log("rules case! sub_terms:", sub_terms)
            vals = []
            for sub_term in sub_terms:
                sub_term = self.build_term(rule, sub_term)
                log("sub_term:", sub_term)
                vals.append(str(sub_term))
            log("vals:", vals)
            return Term(var, vals, dec, sep, ref)
        else:
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

@log_indent
def find_var_vals(term_str: str, dec: str, cls: Type) -> Tuple[str, str]:
    rule_name = get_attribute_type(cls, term_str)
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

def analyse_decorators(term_str: str) -> Tuple[str, str, str, str]:
    decs = "&?*+,;|"
    i_char = len(term_str)-1
    decorators = ""
    while i_char >= 0 and term_str[i_char] in decs:
        decorators = term_str[i_char] + decorators
        i_char -= 1
    log("decorators:", decorators)
    term_str = term_str[:i_char+1]
    list_decs = "?*+"
    dec = ""
    sep = ""
    ref = ""
    for ld in list_decs: 
        if ld in decorators: 
            dec = ld
    ref = "&" if "&" in decorators else ""
    seps = ",;|"
    for s in seps:
        if s in decorators:
            sep = s
    return term_str, dec, sep, ref
    
#--------------------------------------------------------------------------------------------------
# second-order grammar properties

# check that the following assumptions made by the parser are true:
# 1- all referred to rules actually exist
# 2- any unbound list terms are the only variable term in the rule
# 3- a term's values are all the same type ("keyword", <type>, or rule)

def get_errors(grammar: Grammar) -> str:
    out = ""
    for rule in grammar.rules:
        unbound_list_terms = []
        variable_terms = []
        for i_term, term in enumerate(rule.terms):
            # 1- check referred-to-rules exist
            for val in term.vals:
                if val[0] and not (val[0] in '"<'):
                    if not (val in Grammar.current.rule_named):
                        out += f"term {i_term} of {rule.name}: can't find referred-to rule '{val}'\n"
            # 2- is the term unbound, and has '+' or '*'?
            if term.dec and (term.dec in "*+") and not term.var:
                unbound_list_terms.append(term)
            # 2.1- is the term a variable term? (i.e. contains identifier or rule)
            if len(term.vals) > 0 and term.vals[0][0] != '"':
                variable_terms.append(term)
            # 3- are all the term's values the same type?
            n_keywords = 0; n_types =0; n_rules =0
            for val in term.vals:
                if val[0] == '"': n_keywords += 1
                elif val[0] == "<": n_types += 1
                else: n_rules += 1
            n_vals = len(term.vals)
            ok = (n_keywords == n_vals) or (n_types == n_vals) or (n_rules == n_vals)
            if not ok:
                out += f"term {i_term} of {rule.name}: values are mixed types\n" 
        if len(unbound_list_terms) > 0 and len(variable_terms) > 1:
            out += f"rule {rule.name} has unbound list terms: {str(unbound_list_terms).replace("[", "").replace("]", "")}\n"
    return out

# computes all nested-separators
def compute_nested_separators():
    for rule in Grammar.current.rules:
        for term in rule.terms:
            found_terms =contains_nested_separator(term)
            if len(found_terms) > 0:
                #log(f"rule {rule.name}: term {term.index} has nested separator {term.sep}")
                term.contains_nested_sep = True

# checks a term to see if it has one or more nested separators
def contains_nested_separator(term: Term) -> List[Term]:
    if term.sep == "": return []
    visited = {}   # map Rule.name => bool
    if not term.is_rule(): return []
    # returns True if the rule, or any reachable sub-rule, contains the separator outside of braces ("{}()")
    def check_nested_separator(rule: Rule, visited: Dict[Rule, bool], sep: str) -> List[Term]:
        if rule.name in visited: return visited[rule.name]
        visited[rule.name] = False
        in_braces = False
        found_terms = []
        for term in rule.terms:
            if not in_braces:
                if term.sep == sep:
                    found_terms.append(term)
                if term.is_rule():
                    for sub_rule in term.rules():
                        new_terms= check_nested_separator(sub_rule, visited, sep)
                        found_terms += new_terms
            if term.is_keyword() and term.is_singular():
                val = term.vals[0][1:-1]
                if val in "{(" : in_braces = True
                elif val in "})": in_braces = False

        visited[rule.name] = found_terms
        return found_terms

    all_sub_terms_found = []
    for sub_rule in term.rules():
        if sub_rule.name in visited: continue
        sub_terms_found = check_nested_separator(sub_rule, visited, term.sep)
        all_sub_terms_found += sub_terms_found
    return all_sub_terms_found
    

# computes the initiators and followers for each rule and term
def compute_meta_stuff():
    done = False
    while not done: done = not (compute_initials())
    done = False
    while not done: done = not (compute_followers())
    finish_compute_followers()
    done = False
    while not done: done = not (compute_leaves())
    compute_indices()
    compute_nested_separators()

def compute_initials() -> bool:
    changed = False
    # all keyword terms get their initials = vals
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_keyword(): term.initials = term.vals
    # first find the first keyword in each rule
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_keyword():
                changed = merge_arrays(rule.initials, term.vals) or changed
            if not term.dec or term.dec == '+':
                break
    # now apply this transitively to the terms:
    # if a term is a bunch of rules, that term's initials come from those rules
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    changed = merge_arrays(term.initials, sub_rule.initials) or changed
    # and finally skim the first term's initials into the parent rule
    for rule in Grammar.current.rules:
        for term in rule.terms:
            changed = merge_arrays(rule.initials, term.initials) or changed
            if not term.dec or term.dec == '+':
                break
    return changed

s_trace = ""

def push_followers_downwards(term, followers, changed) -> bool:
    if not term.is_rule(): return changed
    for sub_rule in term.rules():
        filtered_followers = [f for f in followers if f not in sub_rule.initials]
        changed = merge_arrays(sub_rule.followers, filtered_followers) or changed
    return changed

def compute_followers():
    changed = False
    # set term and rule followers using initials of the next term
    check_term = None
    for rule in Grammar.current.rules:
        for i_term, term in enumerate(rule.terms):
            if term.is_terminal() and term.dec == "": continue
            if (i_term + 1) < len(rule.terms):
                next_term = rule.terms[i_term+1]
                next_initials = next_term.initials
                changed = merge_arrays(term.followers, next_initials) or changed
                changed = push_followers_downwards(term, next_initials, changed)
                if next_term.dec and (i_term + 2) < len(rule.terms):
                    next_next_initials = rule.terms[i_term+2].initials
                    changed = merge_arrays(term.followers, next_next_initials) or changed
                    changed = push_followers_downwards(term, next_next_initials, changed)

    # for the last term in each rule, set its followers to the followers of its rule
    for rule in Grammar.current.rules:
        last_term = rule.terms[-1]
        changed = merge_arrays(last_term.followers, rule.followers)
        if last_term.is_rule():
            changed = push_followers_downwards(last_term, rule.followers, changed)
        if last_term.dec and last_term.dec in '?*' and len(rule.terms)>1:
            second_last_term = rule.terms[-2]
            changed = merge_arrays(second_last_term.followers, rule.followers)
            if second_last_term.is_rule():
                changed = push_followers_downwards(second_last_term, rule.followers, changed)    
    return changed

def finish_compute_followers():
    for rule in Grammar.current.rules:
        for term in rule.terms:
            term.followers = [f for f in term.followers if f not in term.initials]

# compute leaves: for each rule, find {terminal => [rule]}
def compute_leaves() -> bool:
    changed = False
    for rule in Grammar.current.rules:
        term = rule.terms[0]
        if term.is_terminal():
            for val in term.vals:
                changed = merge_dicts(rule.leaves, { val : [rule] }) or changed
    # now transfer those to terms
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    if sub_rule.is_abstract():
                        changed = merge_dicts(term.leaves, sub_rule.terms[0].leaves) or changed
                    else:
                        for val in sub_rule.leaves.keys():
                            changed = merge_dicts(term.leaves, { val : [sub_rule] }) or changed             
    # and then back to the rules
    for rule in Grammar.current.rules:
        term = rule.terms[0]
        if term.is_rule():
            changed = merge_dicts(rule.leaves, term.leaves) or changed
    return changed

# compute indices
def compute_indices():
    for rule in Grammar.current.rules:
        for i_term, term in enumerate(rule.terms):
            term.rule = rule
            term.index = i_term

# merge two dicts (name => [vals]): return true if d1 changed
def merge_dicts(d1: Dict, d2: Dict) -> bool:
    changed = False
    for k, v in d2.items():
        if not k in d1:
            d1[k] = v.copy()
            changed = True
        else:
            changed = merge_arrays(d1[k], v) or changed
    return changed

# merge two arrays: return true if a1 changed
def merge_arrays(a1, a2)->bool:
    changed = False
    for v in a2:
        if not v in a1:
            a1.append(v)
            changed = True
    return changed