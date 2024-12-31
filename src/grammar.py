# ᕦ(ツ)ᕤ
# grammar.py
# author: asnaroo
# zero to anything

from typing import List, Type, Tuple, Dict, Callable, TypeVar
T = TypeVar('T')
from util import *
from typing import get_type_hints
from lexer import Lex

#--------------------------------------------------------------------------------------------------
# Error represents something that went wrong
class Error:
    def __init__(self, message: str, expected: str, got: str, at: str):
        self.message = message
        self.expected = expected
        self.got = got
        self.at = at
    def __str__(self):
        expected = f"(expected {self.expected}, got '{self.got}'" if self.expected else ""
        return f"!!! {self.message} {expected} at {self.at})"
    def __repr__(self): return self.__str__()

#--------------------------------------------------------------------------------------------------
# Entity is the base class for all AST Nodes

# Entity is the base class; all entities have a name and an alias
class Entity:
    def __init__(self): self._error : Error = None
    def __str__(self):
        name = f"({self.short_name()})"
        return f"{self.__class__.__name__}{name}"
    def __repr__(self): return self.__str__()
    def short_name(self)-> str:
        name = ".."
        if hasattr(self, "name"): name = f"{self.name}"
        return name

#--------------------------------------------------------------------------------------------------
# Term: a single term of a rule

class Term:
    def __init__(self, var: str, vals: List[str], dec:str ="", sep:str ="", ref: str = ""):
        self.var = var      # variable to assign in the AST, if any
        self.vals = vals   # a set of keywords ('word'), types (<type>), or rules (Rule)
        self.dec = dec      # one of "?" (optional), "*" (zero-or-more), "+" (one-or-more)
        self.sep = sep      # separator: one of "", ",", ";" or "|" 
        self.ref = ref      # "", or "RuleName" if the term is a reference to an entity
        self.initials = []  # keywords that can initiate this term
        self.followers = [] # keywords that can follow this term
        self.leaves = {}    # maps match => [rule]s
        self.index = 0      # index in the original term list
        self.rule = None    # the rule we're in, if any
        self.contains_nested_sep = False     # True if this term points to sub-rules with the same separator
    def __str__(self):
        if self.ref: out = self.ref + "&"
        else: 
            out = " | ".join(self.vals)
            if len(self.vals) > 1: out = f"({out})"
        if self.dec: out += self.dec
        if self.sep: out += self.sep
        if self.var: out = f"{self.var}:{out}"
        return out.replace('"', "'")
    def __repr__(self): return self.__str__()
    def is_keyword(self): return self.vals[0][0] == '"' if len(self.vals) > 0 else False
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
    def __init__(self, name: str, parent_name: str, rhs: str):
        self.entity_cls = None
        self.name = name
        self.parent_name = parent_name
        self.rhs = rhs
        self.terms = []         # list of Terms
        self.initials = []      # keywords that can start this rule
        self.followers = []     # keywords that can follow this rule
        self.leaves = {}        # maps match => rule-names
        self.complexity: int=0  # estimate of how complex a rule is
    def __str__(self):
        return self.name
    def __repr__(self): return self.__str__()
    def dbg(self):
        parent = f" < {self.parent_name}" if self.parent_name else ""
        out = self.name + f"{parent} := " + " ".join([str(term) for term in self.terms])
        return out
    def is_abstract(self): return self.rhs == ""
    def is_placeholder(self): return self.is_abstract() and (len(self.terms)==0 or len(self.terms[0].vals)==0)

#--------------------------------------------------------------------------------------------------
# Grammar

class Grammar:
    current: 'Grammar' = None
    def __init__(self):
        if Grammar.current != None:
            raise Exception("trying to build grammar twice")
        Grammar.current = self
        self.rules : List[Rule] = []
        self.rule_named = {}                    # rule name => rule
        self.new_rules = []                     # list of rules added since last add()
        self.class_defs = {}                    # class name => text class definition (init method only)
        self.class_types = {}                   # class.attribute => type
        self.classes = {}                       # class name => class
        entity_rule = Rule("Entity", "", "")
        entity_rule.entity_cls = Entity
        self.add_rule(entity_rule)

    def add(self, rules_str: str):
        lines = rules_str.strip().split("\n")
        self.new_rules = []
        for rule_str in lines:
            lhs, rhs = split_str(rule_str, ":=")
            rule_name, parent_name = split_str(lhs, "<")
            rule = Rule(rule_name, parent_name,rhs)
            self.add_rule(rule)
        for rule in self.new_rules:
            if len(rule.terms) == 0:
                self.build_rule(rule)
        for rule in self.new_rules:
            self.connect_parent(rule)
        compute_meta_stuff()

    def build_rule(self, rule: Rule):
        if rule.rhs == "": self.build_abstract_rule(rule)
        else: self.build_concrete_rule(rule)

    def build_abstract_rule(self, rule: Rule):
        rule.terms = [Term(var=None, vals=[], dec="", sep="", ref="")]

    def build_concrete_rule(self, rule: Rule):
        term_strs = split_terms(rule.rhs)
        for term_str in term_strs:
            term = self.build_term(rule, term_str)
            rule.terms.append(term)

    def build_term(self, rule: Rule, term_str: str) -> Term:
        parts = re.match(r"(\w+):(.+)", term_str)
        var = parts.group(1) if parts else None
        term_str = parts.group(2) if parts else term_str 
        term_str, dec, sep, ref = analyse_decorators(term_str)
        if term_str.startswith("(") and term_str.endswith(")"):
            return self.build_complex_term(rule, term_str[1:-1], var, dec, sep, ref)
        else:
            return self.build_simple_term(rule, term_str, var, dec, sep, ref)
        
    def build_simple_term(self, rule: Rule, term_str: str, var: str, dec: str, sep: str, ref: str) -> Term:
        return Term(var, [term_str], dec, sep, ref)
    
    def build_complex_term(self, rule: Rule, term_str: str, var: str, dec: str, sep: str, ref: str) -> Term:
        if " | " in term_str: return self.build_or_list_term(rule, term_str, var, dec, sep, ref)
        else: return self.build_sub_rule_term(rule, term_str, var, dec, sep, ref)

    def build_or_list_term(self, rule: Rule, term_str: str, var: str, dec: str, sep: str, ref: str) -> Term:
        sub_term_strs = [val.strip() for val in term_str.split("|")]
        if sub_term_strs[0][0] in '<"':
            return Term(var, sub_term_strs, dec, sep, ref)
        vals = []
        for sub_term_str in sub_term_strs:
            sub_term = self.build_term(rule, sub_term_str)
            vals.append(str(sub_term))
        return Term(var, vals, dec, sep, ref)
    
    def build_sub_rule_term(self, rule: Rule, term_str: str, var: str, dec: str, sep: str, ref: str) -> Term:
        sub_rule_name = rule.name + "_"
        while(sub_rule_name in self.rule_named): sub_rule_name += "_"
        sub_rule = Rule(sub_rule_name, "", term_str)
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
        self.new_rules.append(new_rule)
        return new_rule
    
    def connect_parent(self, rule: Rule):
        if not rule.parent_name: return
        parent = self.rule_named[rule.parent_name]
        #if not parent.is_abstract(): raise Exception(f"parent {parent.name} of {rule.name} is not abstract")
        if len(parent.terms) == 0:
            parent.terms.append(Term(var=None, vals=[], dec="", sep="", ref=""))
        if not (rule.name in parent.terms[0].vals):
            parent.terms[0].vals.append(rule.name)

    def dbg(self) -> str:
        rules = self.rules
        out = ""
        for rule in rules:
            out += rule.dbg() + "\n"
        return out
    
    #-----------------------------------------------------------------------------------
    # make classes from rules

    def build_classes(self):
        log("building classes ------------------------------")
        rules = [rule for rule in self.rules if not rule.name.endswith("_")]
        log("building:", [rule.name for rule in rules])
        for rule in rules:
            if rule.name == "Entity": continue
            self.build_class(rule)
    
    def set_rule_classes(self, module):
        for rule in self.rules:
            class_name = rule.name.replace("_", "")
            rule.entity_cls = getattr(module, class_name)
            self.classes[class_name] = rule.entity_cls

    def get_class(self, class_name: str) -> Type:
        class_name = class_name.replace("&", "")
        if class_name not in self.classes:
            raise Exception(f"can't find class {class_name}")
        return self.classes[class_name]

    def build_class(self, rule: Rule):
        name = rule.name
        parent = rule.parent_name
        attributes = self.build_class_attributes(rule, {})   # name -> type
        log("attributes:", attributes)
        if not parent: parent = "Entity"
        names = attributes.keys()
        types = attributes.values()
        init_param_list = ", ".join([f"{name}: '{type.replace("&", "")}' =None" for name, type in zip(names, types)])
        class_def = log_deindent(f"""
            class {name}({parent}):
                def __init__(self, {init_param_list}):
                    super().__init__()
            """)
        for attribute_name, attribute_type in attributes.items():
            ref = ""
            py_attribute_type = attribute_type
            if "&" in attribute_type:
                ref = "        # ref"
                py_attribute_type = attribute_type.replace("&", "")
            class_def += f"        self.{attribute_name}: {py_attribute_type} = {attribute_name}{ref}\n"
            self.class_types[f"{name}.{attribute_name}"] = attribute_type
        log(class_def)
        self.class_defs[name] = class_def

    def build_class_attributes(self, rule: Rule, attributes: Dict[str, str]) -> Dict[str, str]:
        if rule.is_abstract(): return {}
        log("rule:", rule.dbg())
        for term in rule.terms:
            if term.var:
                attribute_type = get_attribute_type(term)
                if term.var in attributes:
                    if not (attribute_type in attributes[term.var]):
                        attributes[term.var] += " | " + attribute_type
                else:
                    attributes[term.var] = attribute_type
            elif term.is_rule():
                for sub_rule in term.rules():
                    attributes = self.build_class_attributes(sub_rule, attributes)
            elif not term.is_keyword():
                raise Exception(f"unnamed non-rule type")
        return attributes
    
    # given class name and attribute name, return type
    def get_attribute_type(self, cls: Type, name: str) -> str:
        key = f"{cls.__name__}.{name}"
        if key not in self.class_types: return "None"
        return self.class_types[key]
    
    def write_classes(self, path: str):
        preamble = log_deindent(f"""
            # ᕦ(ツ)ᕤ
            # {path.replace("src/", "")}
            # auto-generated by zeta.py
            # zero to anything

            from grammar import Entity
            from typing import List, Dict, Type
            """).lstrip()
        with open(path, "w") as f:
            f.write(preamble)
            for name, class_def in self.class_defs.items():
                f.write(class_def)
    
    def method(self, cls: Type[T]) -> Callable:
        def decorator(func: Callable) -> Callable:
            class_name = cls.__name__
            
            # Convert standalone function to method
            @wraps(func)
            def method(self, *args, **kwargs):
                return func(self, *args, **kwargs)
                
            # Add the method to the class
            setattr(cls, func.__name__, method)
            
            # Important: return the original function or method
            return method
        
        return decorator
    


        
#--------------------------------------------------------------------------------------------------
# helpers

def split_str(rule_str: str, sep: str) -> Tuple[str, str]:
    rule_str = rule_str.strip()
    i_sep = rule_str.find(sep)
    if i_sep == -1: return rule_str, ""
    return rule_str[:i_sep].strip(), rule_str[i_sep+len(sep):].strip()

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
            elif char == '"':
                in_quotes = not in_quotes
            
            current_term += char
    if current_term:
        terms.append(current_term)
    return terms

def analyse_decorators(term_str: str) -> Tuple[str, str, str, str]:
    decs = "&?*+,.;|"
    i_char = len(term_str)-1
    decorators = ""
    while i_char >= 0 and term_str[i_char] in decs:
        decorators = term_str[i_char] + decorators
        i_char -= 1
    term_str = term_str[:i_char+1]
    list_decs = "?*+"
    dec = ""
    sep = ""
    ref = ""
    for ld in list_decs: 
        if ld in decorators: 
            dec = ld
    seps = ",;|."
    for s in seps:
        if s in decorators:
            sep = s
    if "&" in decorators:
        ref = term_str
        term_str = "<identifier>"
    return term_str, dec, sep, ref

def remove_duplicate_terms(terms: List[Term]) -> List[Term]:
    out = []
    for term in terms:
        already_there = False
        for o in out:
            if o.var == term.var: already_there = True
        if not already_there: out.append(term)
    return out

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


# computes the initiators and followers for each rule and term
def compute_meta_stuff():
    compute_complexity()
    done = False
    while not done: done = not (compute_initials())
    done = False
    while not done: done = not (compute_followers())
    finish_compute_followers()
    done = False
    while not done: done = not (compute_leaves())
    sort_leaves_by_complexity()
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
        if len(rule.terms) == 0: continue
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
        if len(rule.terms) == 0: continue
        term = rule.terms[0]
        if term.is_terminal():
            for val in term.vals:
                changed = merge_dicts(rule.leaves, { val : [rule] }) or changed
    # now transfer those to terms
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    for val in sub_rule.leaves.keys():
                        changed = merge_dicts(term.leaves, { val : [sub_rule] }) or changed             
    # and then back to the rules
    for rule in Grammar.current.rules:
        if len(rule.terms) == 0: continue
        term = rule.terms[0]
        if term.is_rule():
            changed = merge_dicts(rule.leaves, term.leaves) or changed
    return changed

def sort_leaves_by_complexity():
    for rule in Grammar.current.rules:
        for term in rule.terms:
            if not term.is_rule(): continue
            keyword_leaves = [] 
            type_leaves = []
            # first sort the individual rules within each list by complexity
            for key, rule_list in term.leaves.items():
                rule_list.sort(key=lambda x: x.complexity, reverse=True)
                term.leaves[key] = rule_list
                if key.startswith('"'): keyword_leaves.append((key, rule_list))
                else: type_leaves.append((key, rule_list))
            term.leaves = {}
            for k in keyword_leaves: term.leaves[k[0]] = k[1]
            for t in type_leaves: term.leaves[t[0]] = t[1]
                
# compute indices
def compute_indices():
    for rule in Grammar.current.rules:
        for i_term, term in enumerate(rule.terms):
            term.rule = rule
            term.index = i_term

# computes all nested-separators
def compute_nested_separators():
    for rule in Grammar.current.rules:
        for term in rule.terms:
            found_terms =contains_nested_separator(term)
            if len(found_terms) > 0:
                #log(f"rule {rule.name}: term {term.index} has nested separator {term.sep}")
                term.contains_nested_sep = True

# checks a term to see if it has one or more nested separators
def contains_nested_separator(term: Term) -> List[Entity]:
    if term.sep == "": return []
    visited = {}   # map Rule.name => bool
    if not term.is_rule(): return []
    # returns True if the rule, or any reachable sub-rule, contains the separator outside of braces ("{}()")
    def check_nested_separator(rule: Rule, visited: Dict[Rule, bool], sep: str) -> List[Term]:
        if rule.name in visited: return []
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

def compute_complexity():
    visited = {}
    def compute_complexity_rec(rule: Rule, visited: Dict[Rule, int]) -> int:
        if rule.name in visited: return visited[rule.name]
        sum = len(rule.terms)
        visited[rule.name] = sum
        for term in rule.terms:
            if term.is_rule():
                for sub_rule in term.rules():
                    term_complexity = compute_complexity_rec(sub_rule, visited)
                    if term.dec != "": term_complexity *= 2
                    sum += term_complexity
            else:
                term_complexity = 1
                if term.dec != "": term_complexity *= 2
                sum += term_complexity
        visited[rule.name] = sum
        return sum
    for rule in Grammar.current.rules:
        rule.complexity = compute_complexity_rec(rule, visited)
    
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

# returns a list of all rules referred to by (rule)
def find_rules(rule_names: List[str]):
    rules = []
    for rule in Grammar.current.rules:
        if rule.name.replace("_", "") in rule_names:
            rules.append(rule)
    return rules

# returns a type string for a term
def get_attribute_type(term: Term) -> str:
    vb = term.var == "modifier"
    type_name = ""
    if vb: log(term.vals)
    if term.is_keyword() and len(term.vals) > 1:
        type_name = "str"
    elif term.is_type():
        if term.ref: type_name = term.ref + "&"
        else: type_name = "str"
    elif term.is_rule():
        type_name = "|".join([rule.name for rule in term.rules()])
    if term.dec != "" and term.dec in "*+":
        type_name = f"List[{type_name}]"
    return type_name

#-----------------------------------------------------------------------------------------------------------------------
# print out an Entity tree

# detailed printout of every property of an entity
def dbg_entity(e: Entity|List[Entity], indent: int=0) ->str:
    out = ""
    start = "    " * indent
    if isinstance(e, Error): 
        return f"{start}{log_red(e)}\n"
    elif isinstance(e, Entity):
        out += f"{start}{e.__class__.__name__}\n"
        for attr in vars(e):
            if attr == "_error":
                if e._error != None:
                    out += f"{start}    {log_red(e._error)}\n"
                continue
            val = getattr(e, attr)
            type_name = Grammar.current.get_attribute_type(e.__class__, attr)
            if val == None or (isinstance(val, Lex) or isinstance(val, str))or (isinstance(val, List) and len(val)==0):
                ref = ">" if (isinstance(val, Lex) or isinstance(val, str)) and type_name != "str" else ""
                out += f"{start}    {attr}: {type_name.replace("&", "")} ={ref} {val}\n"
            else:
                if isinstance(val, list) and "List[" not in type_name:
                    type_name = f"List[{type_name}]"
                out += f"{start}    {attr}: {type_name.replace("&", "")}"
                if isinstance(val, list) and len(val) > 0:
                    out += "\n"
                    if isinstance(val[0], Entity):
                        for item in val:
                            out += dbg_entity(item, indent+2)
                    else:
                        type_in_brackets = type_name[5:-1]
                        ref = "=> " if type_in_brackets != "str" else ""
                        for item in val:
                            out += f"{start}        {ref}{item}\n"
                else: 
                    out += "\n"
                    out += dbg_entity(val, indent+2)
    return out
