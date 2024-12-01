# ᕦ(ツ)ᕤ
# symbols.py
# author: asnaroo
# zero to anything

from typing import Type, Dict, Any, List
from grammar import Entity

#--------------------------------------------------------------------------------------------------
# symbol tables and that

class SymbolItem:
    def __init__(self, scope: Any, element: Any, tag: Dict):
        self.scope = scope
        self.element = element
        self.tag = tag
        
class SymbolTable:
    def __init__(self):
        self.symbols = {}   # name.scope_type.scope_id => { element, tag }
    
    def add(self, name: str, element: Any, scope: Any, tag: Dict=None) -> str:
        if name is None: return
        item = SymbolItem(scope, element, tag)
        if not name in self.symbols: self.symbols[name] = []
        self.symbols[name] = [item] + self.symbols[name] # add to start; first takes precedence

    def find(self, name: str, scope: Any|None) -> List[SymbolItem]:
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
                tag = "" if item.tag is None else f"{item.tag}"
                out += f"{item.element}{tag} in {item.scope}; "
            out = out[:-2]
            out += "\n"
        return out
    



