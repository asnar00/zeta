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
    
    def add(self, name: str, scope: Any, element: Any, tag: Dict) -> str:
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
def build_symbol_table(e: Entity, scope: Any) -> SymbolTable:
    table = SymbolTable()
    build_symbol_table_rec(table, e, scope)
    return table

def build_symbol_table_rec(table: SymbolTable, e: Entity, scope: Any):
    if e.hasattr("build_symbols"):
        e.build_symbols(table, scope)
        scope = e
    for name, value in e.properties.items():
        if isinstance(value, Entity):
            build_symbol_table_rec(table, value)
        elif isinstance(value, List):
            for item in value:
                if isinstance(item, Entity):
                    build_symbol_table_rec(table, item, scope)

    



