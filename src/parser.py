# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from util import *
from lexer import *
from grammar import *
from typing import List, Dict, Tuple
  
#--------------------------------------------------------------------------------------------------
# Partial Match: builds the AST as it goes
class Partial:
    def __init__(self, rule: Rule, first_item):
        self.rule = rule
        self.i_term = 0
        self.matched_items = [[] for _ in range(len(rule.terms))]
        self.ast = {}
        self.add_item(first_item)

    def __str__(self):
        match = f"{self.i_term}/{len(self.rule.terms)}"
        if self.matched(): match = log_green(match)
        return (f"{match}: {log_grey(str(self.get_ast()))}").replace("'", "")
    
    def get_ast(self) -> Dict:
        return { self.rule.name : self.ast }
    
    # checks each matched item to see how many we can consider matched
    def count_matched(self)->int:
        n_matched = 0
        for i, item in enumerate(self.matched_items):
            term = self.rule.terms[i]
            if isinstance(term, ZeroOrMore) or \
                isinstance(term, Optional) or \
                len(item) > 0:
                n_matched += 1
        return n_matched

    def matched(self)->bool:
        return self.count_matched() == len(self.rule.terms)
    
    def expecting_lex(self)->bool:
        term = self.rule.terms[self.i_term]
        return isinstance(term, Type) or isinstance(term, Keyword)

    # tries to add an item (ast/lex) to the end of the current match, returns success
    def add_item(self, item) -> bool:
        i_term = self.can_add_item(item)
        if i_term == None: return False
        self.add_item_at(item, i_term)
        self.i_term = i_term
        if not (isinstance(self.rule.terms[self.i_term], ZeroOrMore)):
            self.i_term += 1
        return True
    
    # adds an item (ast/lex) at a specific index
    def add_item_at(self, item, i_term):
        term = self.rule.terms[i_term]
        if isinstance(term, ZeroOrMore): self.matched_items[i_term].append(item)
        else: self.matched_items[i_term] = [item]
        if term.set_variable:
            matched = self.matched_items[i_term]
            if isinstance(term, ZeroOrMore):
                self.ast[term.set_variable] = self.matched_items[i_term]
            else:
                self.ast[term.set_variable] = self.matched_items[i_term][0]

    # checks to see if we can add an item (ast/lex) at the current i_term
    def can_add_item(self, item) -> int:
        can_at_iterm = self.can_add_item_at(item, self.i_term)
        if can_at_iterm: return self.i_term
        if self.i_term == len(self.rule.terms) -1: return None
        term = self.rule.terms[self.i_term]
        if isinstance(term, Optional) or isinstance(term, ZeroOrMore):
            if self.can_add_item_at(self, item, self.i_term + 1):
                return self.i_term + 1
        return None

    # checks to see if we can add an item (ast/lex) at a specific i_term
    def can_add_item_at(self, item, i_term):
        term = self.rule.terms[i_term]
        return self.does_item_match_term(item, term)
    
    # checks if item (ast/lex) matches a term definition
    def does_item_match_term(self, item, term):
        if isinstance(term, Type):
            return isinstance(item, Lex) and item.type == term.type_name
        elif isinstance(term, Keyword):
            return isinstance(item, Lex) and item.val == term.word
        elif isinstance(term, Ref) and isinstance(item, dict):
            return list(item.keys())[0]
        elif isinstance(term, OneOf):
            for t in term.terms:
                if self.does_item_match_term(item, t): return True
        elif isinstance(term, Optional):
            return self.does_item_match_term(item, term.term)
        elif isinstance(term, ZeroOrMore): # this one is a little groody, needs more logic
            return self.does_item_match_term(item, term.term) or \
                (term.sep and self.does_item_match_term(item, term.sep))

#--------------------------------------------------------------------------------------------------
# Parser: read one lexeme at a time, generate an AST

@this_is_the_test
def test_parser():
    log("test_parser")
    parser = Parser(test_grammar_spec)
    log("--------------------------------------------------------------------------")
    test("parse_variable", parser.parse("a"), """{'variable': {'name': a}}""")
    log("--------------------------------------------------------------------------")
    test("parse_postfix", parser.parse("a!"), """{'postfix': {'expr': {'variable': {'name': a}}, 'operator': !}}""")
    log("--------------------------------------------------------------------------")
    test("parse_infix", parser.parse("a + b"), """{'infix': {'left': {'variable': {'name': [a]}}, 'operator': [+], 'right': {'variable': {'name': [b]}}}""")

#--------------------------------------------------------------------------------------------------
# Parser: read one lexeme at a time, generate an AST

class Parser:
    def __init__(self, grammar_spec: str):
        self.grammar = setup_grammar(grammar_spec)
        self.setup_map()
        self.show_map(self.rule_map)

    def parse(self, code: str) -> Dict:
        ls = lexer(Source(code = code))
        pms = []

        for i, lex in enumerate(ls):
            log("\nstep", i, lex.type, f"({lex.val})")

            # replace matched rules with new ones
            pms = self.replace_matched(pms)
            self.show_partials("replaced", pms)

            # try and add the new lex to the resulting partials
            pms = self.add_lex_to_partials(lex, pms)
            self.show_partials("added", pms)

            # add new partials
            pms += self.find_new_partials(lex)
            self.show_partials("new", pms)
            
        log("")

        return self.best_match(pms)
    
    # looks in the rule map for matched rules, replaces them with new partials
    def replace_matched(self, pms: List[Partial]) -> List[Partial]:
        ppms = []
        for pm in pms:
            if not pm.matched(): ppms.append(pm); continue
            rules = self.find_nonterminal_rules(pm.rule.name, 0)
            for rule in rules:
                ppms.append(Partial(rule, pm.get_ast()))
        return ppms
    
    # tries to add the next lex to each pm
    def add_lex_to_partials(self, lex: Lex, pms: List[Partial]) -> List[Partial]:
        ppms = []
        for pm in pms:
            if pm.matched(): ppms.append(pm); continue
            if not pm.expecting_lex(): ppms.append(pm); continue
            if pm.add_item(lex): ppms.append(pm)
        return ppms
    
    def best_match(self, pms: List[Partial]) -> Dict:
        matched = [pm for pm in pms if pm.matched()]
        if len(matched) == 0: return { '_error' : 'no match' }
        if len(matched) > 1: return { '_error' : 'ambiguous' }
        return matched[0].get_ast()
    
    def show_partials(self, label, pms: List[Partial]):
        log(label, ":")
        for pm in pms:
            log("   ", pm)
    
    def find_new_partials(self, lex: Lex) -> List[Partial]:
        rules = self.find_terminal_rules(lex, 0)
        pms = []
        for rule in rules:
            pms.append(Partial(rule, lex))
        return pms

    # returns true if the rule is "nodal" - is a OneOf with only one term
    def is_nodal_rule(self, rule: Rule) -> bool:
        return len(rule.terms) == 1 and isinstance(rule.terms[0], OneOf)

    # find all rules that accept (lex) at position (i_term)
    def find_terminal_rules(self, lex: Lex, i_term: int) -> List[Rule]:
        if lex.val in self.keyword_map:
            if i_term >= len(self.keyword_map[lex.val]): return []
            return (self.keyword_map[lex.val])[i_term]
        elif lex.type in self.type_map:
            if i_term >= len(self.type_map[lex.type]): return []
            return (self.type_map[lex.type])[i_term]
        return []
    
    # find rules that accept some ast(rule) in position i_term
    def find_nonterminal_rules(self, rule_name: str, i_term: int) -> List[Rule]:
        if rule_name in self.rule_map:
            if i_term >= len(self.rule_map[rule_name]): return []
            return (self.rule_map[rule_name])[i_term]
        return []
    
    # process the grammar to build a fast map from (type/keyword/rule) => (rule, i_term)
    def setup_map(self):
        self.keyword_map = {}   # word => List[List[Rule]]
        self.type_map = {}      # type => List[List[Rule]]
        self.rule_map = {}      # rule_name => List[List[Rule]]
        for rule in self.grammar.rules.values():
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

