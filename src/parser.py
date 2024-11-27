# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from typing import Type
from util import *
from lexer import *
from grammar import *

#--------------------------------------------------------------------------------------------------
# Parser

# main parser routines:

# given a rule and a reader, parse the rule and return an entity
@log_indent
def parse_rule(rule: Rule, reader: Reader) -> Entity:
    children = parse_terms(rule, reader)
    check_child_errors(rule, children)
    entity = merge_children(rule, children, rule.entity_cls())
    return post_parse(entity)

# parse each term in the rule, returns list of results
def parse_terms(rule: Rule, reader: Reader) -> List[Term]:
    children = []
    for term in rule.terms:
        child = parse_term(term, reader)
        children.append(child)
        if has_errors(child) and not term.is_optional(): break
    return children

# dispatch term parser to the correct case (take term.dec into account)
def parse_term(term: Term, reader: Reader) -> Entity:
    if term.is_singular(): return parse_singular_term(term, reader)
    elif term.is_optional(): return parse_optional(term, reader.scan(term.followers))
    else: return parse_list_term(term, reader.scan(term.followers))

# dispatch singular term parser to the correct case (ignore term.dec)
def parse_singular_term(term: Term, reader: Reader) -> Dict:
    if term.is_terminal() or term.is_reference(): return parse_terminal(term, reader)
    elif term.is_rule(): return parse_rule_term(term, reader.scan(term.followers))
    else: raise Exception(f"unknown term type: {term.vals}")

# simplest case: parse a terminal singular term
@log_indent
def parse_terminal(term: Term, reader: Reader) -> Union[Lex, Error]:
    if reader.eof():
        return parse_error("premature end", term, reader)
    vals = term.vals
    if term.is_reference():
        log(log_green("term is a reference term!"))
        vals = ["<identifier>"]
    return reader.next() if reader.matches(vals) else parse_error("mismatch",term, reader)

# term is a list of rules: rule1 | rule 2..; try each in turn until one succeeds
@log_indent
def parse_rule_term(term: Term, reader: Reader) -> Entity:
    if reader.eof():
        return parse_error("premature end", term, reader)
    rules = reduce_rules(term, reader.peek())
    log("rules:", rules)
    if len(rules) == 0: return parse_error("no matched rules", term, reader)
    entity = None
    for rule in rules:
        pos = reader.pos()
        entity = parse_rule(rule, reader)
        if not has_errors(entity): return entity
        reader.restore(pos)
    if entity: return entity
    return parse_error("no matched rules", term, reader)

# parse an optional term, return None if eof or error
@log_indent
def parse_optional(term: Term, reader: Reader) -> Entity:
    if reader.eof(): 
        log(log_red("optional: eof"))
        return None
    pos = reader.pos()
    child = parse_singular_term(term, reader)
    if has_errors(child): reader.restore(pos)
    return child

@log_indent
def parse_list_term(term: Term, reader: Reader) -> Dict:
    items = []
    pos = reader.pos()
    before_sep_pos = pos
    while not reader.eof():
        term_reader = scan_separator(reader, term)
        child = parse_singular_term(term, term_reader)
        items.append(child)
        if has_errors(child):
            log(log_red("last item has errors!"))
            items = truncate_list(term, reader, items, pos)
            log(f"reader after restore: {reader}")
            break
        if reader.eof(): break
        pos = reader.pos()
        items, cont = parse_separator(term, reader, items, before_sep_pos)
        if not cont: break
        before_sep_pos = pos
    return items

#--------------------------------------------------------------------------------------------------
# "below the line" parser functions

# post-parse: hand the finished entity to its checker to handle things we can't express in the grammar
def post_parse(entity: Entity) -> Entity:
    if isinstance(entity, Error): return entity
    msg = entity.post_parse_check()
    if msg=="": return entity
    log(log_red(f"post-parse error: {msg}"))
    lex = find_first_lex(entity)
    return Error(msg, "", "", lex.location() if lex else "")

# search through for the first lex you find in the entity tree, return its location
def find_first_lex(entity: Entity) -> Lex:
    if entity == None: return None
    if isinstance(entity, Lex): return entity
    if isinstance(entity, Error): return None
    if isinstance(entity, List):
        for item in entity:
            lex = find_first_lex(item)
            if lex: return lex
        return None
    for attr in vars(entity):
        lex = find_first_lex(getattr(entity, attr))
        if lex: return lex
    return None

# check for errors in back-trackable children (needs a little work)
@log_indent
def check_child_errors(rule: Rule,children: List[Dict]):
    for i_term, term in enumerate(rule.terms):
        if i_term >= len(children): break
        if not term.is_optional(): continue
        child = children[i_term]
        if not has_errors(child): continue
        if (i_term+1) < len(children):
            if not has_errors(children[i_term+1]):
                log(log_red("dropping errors from optional term"))
                children[i_term] = None
        else:
            log(log_red("optional term at end of rule has errors!"))
            log(child)
            if has_errors(child):
                count = sum(1 for key in child if key not in ["_rule", "_errors"])
                log(log_red("dropping errors from optional term"))
                if count == 0: children[i_term] = None

# merge children into the parent entity
@log_indent
def merge_children(rule: Rule, children: List[Entity], parent: Entity) -> Entity:
    if rule.is_abstract():
        if len(children) != 1: raise Exception("abstract rule must have only one matched child!")
        if isinstance(children[0], Error): return children[0]
        if not isinstance(children[0], rule.entity_cls):
            raise Exception(f"First child ({children[0].__class__.__name__}) is not a subclass of the rule's class ({rule.entity_cls.__name__})!")
        return children[0]
    else:
        for i_term, term in enumerate(rule.terms):
            if i_term < len(children): 
                merge_child(term, children[i_term], parent)
        return parent

# merge single child into parent entity
@log_indent
def merge_child(term: Term, child: Entity, parent: Entity|Lex|List):
    if term.var != None:
        if not hasattr(parent, term.var):
            log(log_red(f"parent {parent} has no attribute {term.var}"))
            log_exit()
        log(f"setting attribute {term.var} to {child}")
        setattr(parent, term.var, child)
        return
    if child == None: return
    if isinstance(child, Error): 
        parent._error = child
        return
    elif isinstance(child, Lex):
        if term.is_keyword():
            return
        raise Exception("unnamed lex-merge not implemented yet")
    elif isinstance(child, Entity):
        log("merging child attributes into parent!")
        for attr in vars(child):
            if getattr(child, attr) == None: continue
            if hasattr(parent, attr):
                if getattr(parent, attr) is not None:
                    log(log_red(f"parent attribute {attr} is already set to {getattr(parent, attr)}; conflict"))
                    raise Exception("conflict found in merge-child!")
                log(f"setting parent attribute {attr} to {getattr(child, attr)}")
                setattr(parent, attr, getattr(child, attr))
        return
    raise Exception("not implemented yet")

# recursively search through all properties, return true if _errors exist
#@log_indent
def has_errors(entity: Entity | Lex | None) -> bool:
    if entity == None: return False
    if isinstance(entity, Lex): return False
    if isinstance(entity, Error): return True
    if isinstance(entity, List):
        for item in entity: 
            if has_errors(item): return True
        return False
    if hasattr(entity, "_error") and entity._error != None: return True
    for attr in vars(entity):
        if isinstance(getattr(entity, attr), list):
            if any(has_errors(item) for item in getattr(entity, attr)):
                return True
        elif isinstance(getattr(entity, attr), Entity):
            if has_errors(getattr(entity, attr)):
                return True
    
    return False

# return an Error
def parse_error(message: str, term: Term, reader: Reader) -> Dict:
    err = Error(message, expected= str(term), got = str(reader.peek_unrestricted()), at= reader.location())
    log(log_red(f"{err}"))
    return err

# reduce a term to a list of rules
def reduce_rules(term: Term, lex: Lex) -> List[Rule]:
    rules = term.rules()
    if len(rules) == 1: return rules
    leaf_rules = []
    for key, rules in term.leaves.items():
        if lex_matches(lex, [key]):
            leaf_rules += rules
            if key.startswith('"'): break   # i.e. a keyword match prevents all others
    return leaf_rules

def scan_separator(reader: Reader, term: Term) -> Reader:
    term_reader = reader.scan([f"'{term.sep}'"]) if (term.sep and term.contains_nested_sep == False) else reader
    if term.contains_nested_sep: term_reader.set_nested_separator(term.sep)
    return term_reader

#@log_indent
def truncate_list(term: Term, reader: Reader, items: List[Dict], pos: int) -> List[Dict]:
    if reader.nested_sep == None: return items
    if term.dec == "+" and len(items) == 1: return items
    log(log_red("truncating list!"))
    items = items[:-1]
    reader.restore(pos)
    log(f"reader after restore: {reader}")
    return items

#@log_indent
def parse_separator(term: Term, reader: Reader, items: List[Dict], restore_pos: int) -> Tuple[List[Dict], bool]:
    if term.sep == "": return items, True
    if lex_matches(reader.peek(), [f"'{term.sep}'"]):
        reader.next()
        return items, True
    if term.sep == ";": return items, True
    log(log_red(f"separator error: expected {term.sep} but got {reader.peek()}"))
    items = truncate_list(term, reader, items, restore_pos)
    return items,False

#--------------------------------------------------------------------------------------------------
# test routine: returns a nicely formatted AST

def parse_code(code: str, rule_name: str) -> str:
    ls = lexer(Source(code=code))
    reader = Reader(ls)
    rule = Grammar.current.rule_named[rule_name]
    ast= parse_rule(rule, reader)
    return dbg_entity(ast)
    
