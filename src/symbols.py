# ᕦ(ツ)ᕤ
# symbols.py
# author: asnaroo
# zero to anything

from typing import Type, Dict, Any, List, Set
from grammar import Entity, Grammar, dbg_entity
from parser import print_code_formatted
from util import *
from lexer import Lex

#--------------------------------------------------------------------------------------------------
# symbol tables and that

class SymbolItem:
    def __init__(self, scope: Any, element: Any, tag: Dict):
        self.scope = scope
        self.element = element
        self.tag = tag
    def __str__(self):
        tag = "" if self.tag is None else f"{self.tag}"
        return f"{self.element}{tag} in {self.scope}"
    def __repr__(self): return self.__str__()

class SymbolTable:
    def __init__(self):
        self.symbols = {}   # name.scope_type.scope_id => { element, tag }
    
    
    def add(self, name: str, element: Any, scope: Any, alias=None, tag: Dict=None):
        #log(f"  add {name} [{type(element).__name__}] to {scope} called by {caller()}")
        if name is None: return
        if isinstance(name, Lex): name = name.val
        item = SymbolItem(scope, element, tag)
        self.add_item(name, item)
        if alias != None: 
            if isinstance(alias, Lex): alias = alias.val
            self.add_item(alias, item)

    def replace(self, name: str, element: Any, scope: Any, alias=None, tag: Dict=None):
        if name is None: return
        if isinstance(name, Lex): name = name.valitem = SymbolItem(scope, element, tag)
        item = SymbolItem(scope, element, tag)
        self.replace_item(name, item)
        if alias != None: 
            if isinstance(alias, Lex): alias = alias.val
            self.replace_item(alias, item)

    def add_item(self, name: str, item: SymbolItem):
        if not name in self.symbols: self.symbols[name] = []
        if item not in self.symbols[name]:
            #log(f"added '{name}' => {item}")
            self.symbols[name] = [item] + self.symbols[name] # add to start; most recent takes precedence

    def replace_item(self, name: str, item: SymbolItem):
        if not name in self.symbols: self.add_item(name, item)
        list = self.symbols[name]
        # find list-item whose .element is same type as item.element; remove it
        for i in range(len(list)):
            if list[i].element == item.element:
                list.pop(i)
                break
        list.insert(0, item)

    def find_single_item(self, name: Lex, of_type: Any, scope: Any, errors: List[str]) -> SymbolItem:
        if isinstance(name, of_type): return name # already matched
        location = f" at {name.location()}" if isinstance(name, Lex) else f" type {type(name).__name__}({caller(1)})"
        found = self.find(name, of_type, scope)
        report = ""
        if len(found) == 0:
            #log(log_red(f"no {of_type.__name__} '{name}' in scope {scope}{location}{report}"))
            errors.append(f"no {of_type.__name__} '{name}' in scope {scope}{location}{report}")
            if str(name).startswith("VariableDef"): log_exit("weird")        
        elif len(found) > 1:
            errors.append(f"multiple matches for {of_type.__name__} '{name}' in scope {scope}{location}{report}")
        else:
            return found[0]

    def find_single(self, name: Lex, of_type: Any, scope: Any, errors: List[str]) -> SymbolItem:
        if isinstance(name, of_type): return name # already matched
        found= self.find_single_item(name, of_type, scope, errors)
        return found.element if found else None

    def find(self, name: str|Lex, of_type: Any|None, scope: Any|None) -> List[SymbolItem]:
        if isinstance(name, Lex): name = name.val
        if not name in self.symbols: return []
        result= [item for item in self.symbols[name] if self.scope_can_see(scope, item.scope)]
        if of_type is not None:
            result = [item for item in result if isinstance(item.element, of_type)]
        return result
    
    def scope_can_see(self, scope1: Any, scope2: Any) -> bool:
        if scope1 is None or scope2 is None: return True
        if hasattr(scope1, "can_see_scope"):
            return scope1.can_see_scope(scope2, self)
        return scope1 == scope2

    # run recursively through all properties of an Entity, collecting a symbol table
    def add_symbols(self, e: Entity, scope: Any):
        self.add_symbols_rec(e, scope)

    def add_symbols_rec(self, e: Entity, scope: Any):
        if hasattr(e, "add_symbols"):
            e.add_symbols(scope, self)
            if hasattr(e, "get_scope"):
                scope = e.get_scope()
        elif self.add_symbols_for_entity(e, scope):
            scope = e
        for attr in vars(e):
            if attr.startswith("_"): continue # weeny-bit hacky; "_property" is a property added by a method, that wasn't in the original class def
            value = getattr(e, attr)
            if isinstance(value, Entity):
                self.add_symbols_rec(value, scope)
            elif isinstance(value, List):
                for item in value:
                    if isinstance(item, Entity):
                        self.add_symbols_rec(item, scope)

    # this is actually a hack, but it's a quick way to avoid having to add a method to every named entities
    def add_symbols_for_entity(self, e: Entity, scope: Any) -> bool:
        added = False
        if hasattr(e, "name"):
            self.add(e.name, e, scope)
            added = True
        if hasattr(e, "alias"):
            self.add(e.alias, e, scope)
            added = True
        return added

    def dbg(self) -> str:
        out = ""
        for name, items in self.symbols.items():
            out += f'"{name}" => '
            for item in items:
                out += str(item) + "; "
            out = out[:-2]
            out += "\n"
        return out
    
    #@log_suppress
    def resolve_symbols(self, e: Entity, scope: Any, errors: List[str], found: List[str], visited: Set, indent: int = 0):
        start = "  " * indent
        if e in visited: return
        visited.add(e)
        log(f"{start}{e} in {scope}")
        if not isinstance(e, Entity): 
            raise Exception(f"resolve_symbols: not an entity ({e})")
        disallow = hasattr(e, "disallow_resolve_children") and e.disallow_resolve_children()
        if not disallow:
            for attr in vars(e):
                self.resolve_reference(e, attr, scope, errors, found, visited, indent)
            for attr in vars(e):
                if attr.startswith("_") and attr != "_error":
                    attr_val = getattr(e, attr)
                    if attr_val is None: continue
                    if not isinstance(attr_val, List): attr_val = [attr_val]
                    for val in attr_val: 
                        if isinstance(val, Entity): 
                            self.resolve_symbols(val, scope, errors, found, visited, indent+1)
            if hasattr(e, "get_scope"):
                #log(f"{start}calling get_scope on {e} in {scope}")
                scope = e.get_scope() or scope
            #log(f"{start}resolving attrs of {e} in {scope}")
            for attr in vars(e):
                self.resolve_attr_rec(e, attr, scope, errors, found, visited, indent)
        if hasattr(e, "resolve"):
            e.resolve(self, scope, errors, found)

    def resolve_reference(self, e: Entity, attr: str, scope: Any, errors: List[str], found: List[str], visited: Set, indent: int = 0):
        start = "  " * indent
        attr_type = Grammar.current.get_attribute_type(e.__class__, attr)
        is_reference = "&" in attr_type
        if not is_reference: return
        attr_val = getattr(e, attr)
        if attr_val is None: return
        attr_type = attr_type.replace("&", "")
        is_list = "List[" in attr_type
        attr_type = attr_type.replace("List[", "").replace("]", "")
        attr_class = Grammar.current.get_class(attr_type)
        vals = attr_val if is_list else [attr_val]
        resolved_vals = []
        for val in vals:
            resolved = self.find_single(val, attr_class, scope, errors)
            if isinstance(resolved, Entity):
                log(log_green(f"{start}resolved '{attr_val}' => {resolved} in {scope}"))
                resolved_vals.append(resolved)
            else:
                log(log_red(f"{start}failed to resolve '{attr_val}' in {scope}"))
                resolved_vals.append(val)
        if is_list: setattr(e, attr, resolved_vals)
        else: setattr(e, attr, resolved_vals[0])

    def resolve_attr_rec(self, e: Entity, attr: str, scope: Any, errors: List[str], found: List[str], visited: Set, indent: int = 0):
        attr_type = Grammar.current.get_attribute_type(e.__class__, attr)
        attr_val = getattr(e, attr)
        attr_actual_type = type(attr_val).__name__
        if attr_val is None: return
        if attr_type is "str": return
        is_reference = "&" in attr_type
        if is_reference: return
        is_list = "List[" in attr_type
        vals = attr_val if is_list else [attr_val]
        for val in vals:
            if isinstance(val, Entity):
                self.resolve_symbols(val, scope, errors, found, visited, indent+1)
        
