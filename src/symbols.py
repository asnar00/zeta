# ᕦ(ツ)ᕤ
# symbols.py
# author: asnaroo
# zero to anything

from typing import Type, Dict, Any, List
from grammar import Entity, Grammar
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
    
    def add(self, name: str, element: Any, scope: Any, alias=None, tag: Dict=None) -> str:
        if name is None: return
        if isinstance(name, Lex): name = name.val
        item = SymbolItem(scope, element, tag)
        self.add_item(name, item)
        if alias != None: 
            if isinstance(alias, Lex): alias = alias.val
            self.add_item(alias, item)

    def add_item(self, name: str, item: SymbolItem):
        if not name in self.symbols: self.symbols[name] = []
        if item not in self.symbols[name]:
            self.symbols[name] = [item] + self.symbols[name] # add to start; most recent takes precedence

    def find(self, name: str|Lex, scope: Any|None=None) -> List[SymbolItem]:
        if isinstance(name, Lex): name = name.val
        if not name in self.symbols: return []
        return [item for item in self.symbols[name] if self.scope_can_see(scope, item.scope)]
    
    def scope_can_see(self, scope1: Any, scope2: Any) -> bool:
        if scope1 is None: return True
        if hasattr(scope1, "inherits_from"):
            return scope1.inherits_from(scope2)
        return scope1 == scope2

    # run recursively through all properties of an Entity, collecting a symbol table
    def add_symbols(self, e: Entity, scope: Any):
        self.add_symbols_rec(e, scope)

    def add_symbols_rec(self, e: Entity, scope: Any):
        if hasattr(e, "add_symbols"):
            e.add_symbols(scope, self)
            if hasattr(e, "is_scope") and e.is_scope() == True:
                scope = e
        elif self.add_symbols_for_entity(e, scope):
            scope = e
        for attr in vars(e):
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
    
    def resolve_symbols(self, e: Entity, scope: Any=None):
        if hasattr(e, "resolve"):
            err = e.resolve(self, scope)
            if err != "": log(log_red(f"{e.__class__.__name__}.resolve: {err}"))
        if hasattr(e, "is_scope") and e.is_scope() == True:
            scope = e
        for attr in vars(e):
            if attr == "_error": continue
            attr_type = Grammar.current.get_attribute_type(e.__class__, attr)
            attr_value = getattr(e, attr)
            attr_actual_type = type(attr_value)
            if isinstance(attr_value, List):
                for item in attr_value:
                    if isinstance(item, Entity):
                        self.resolve_symbols(item, scope)
            elif isinstance(attr_value, Entity):
                self.resolve_symbols(attr_value, scope)
            elif isinstance(attr_value, Lex) and attr_type != "str":
                log(log_green(f'{attr} = "{attr_value}" ({attr_type})'))
                str_value = attr_value.val
                found = self.find(str_value)
                if len(found) == 0:
                    log(log_red(f"can't find {attr_value}"))
                else:
                    log(log_green(f"found {found}"))
            
        
    



