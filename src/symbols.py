# ᕦ(ツ)ᕤ
# symbols.py
# author: asnaroo
# zero to anything

from typing import Type, Dict, Any, List, Set
from grammar import Entity, Grammar
from entity import dbg_entity
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
            return scope1.can_see_scope(scope2)
        return scope1 == scope2
    
    def dbg(self) -> str:
        out = ""
        for name, items in self.symbols.items():
            out += f'"{name}" => '
            for item in items:
                out += str(item) + "; "
            out = out[:-2]
            out += "\n"
        return out
    
    def report(self, report_fn):
        for name, items in self.symbols.items():
            out = f'"{name}" => '
            for item in items:
                out += str(item) + "; "
            if out.endswith("; "): out = out[:-2]
            report_fn(out)


    