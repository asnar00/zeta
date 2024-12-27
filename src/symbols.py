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
        location = f" at {name.location()}" if isinstance(name, Lex) else ""
        found = self.find(name, of_type, scope)
        report = ""
        if len(found) == 0:
            errors.append(f"no {of_type.__name__} '{name}' in scope {scope}{location}{report}")
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
        if hasattr(scope1, "inherits_from"):
            return scope1.inherits_from(scope2)
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
    
    def resolve_symbols(self, e: Entity, scope: Any, errors:List[str]):
        original_scope = scope
        if hasattr(e, "get_scope"):
            scope = e.get_scope()
        if not hasattr(e, "disallow_resolve_children"):
            for attr in vars(e):
                if attr == "_error": continue
                attr_type = Grammar.current.get_attribute_type(e.__class__, attr)
                attr_value = getattr(e, attr)
                attr_actual_type = type(attr_value)
                if isinstance(attr_value, List):
                    for item in attr_value:
                        if isinstance(item, Entity):
                            self.resolve_symbols(item, scope, errors)
                elif isinstance(attr_value, Entity):
                    self.resolve_symbols(attr_value, scope, errors)
                elif isinstance(attr_value, Lex) and attr_type != "str":
                    str_value = attr_value.val
                    attr_class = Grammar.current.get_class(attr_type)
                    found = self.find(str_value, attr_class, scope)
                    if len(found) == 0:
                        errors.append(f"can't find {attr_value} in {scope}")
                    elif len(found) > 1:
                        found_class = Grammar.current.get_class(attr_type)
                        errors.append(f"multiple matches for {attr_value}[{attr_type}] in {scope}")
                    else:
                        #log(log_green(f"found {found}"))
                        setattr(e, attr, found[0].element)
        if hasattr(e, "resolve"):
            cont = e.resolve(self, original_scope, errors)
            
        
    



