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
        cf = f"{e} in {scope}"
        log(f"{start}{cf}")
        vb = (cf == "ResultVariableDef(..) in FunctionDef(Math.(number)+(number))")
        if vb: log_clear(); log(log_orange("---------------"))
        if not isinstance(e, Entity): 
            raise Exception(f"resolve_symbols: not an entity ({e})")
        disallow = hasattr(e, "disallow_resolve_children") and e.disallow_resolve_children()
        if not disallow:
            for attr in vars(e):
                self.resolve_reference(e, attr, scope, errors, found, visited, vb, indent)
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

    def resolve_reference(self, e: Entity, attr: str, scope: Any, errors: List[str], found: List[str], visited: Set, vb: bool=False,indent: int = 0):
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
                log(log_red(f"{start}failed to resolve '{attr_val}' class {attr_class.__name__} in {scope}"))
                if vb: 
                    log(log_orange(f"diagnostic: {attr_val} in scope {scope}"))
                    items = self.find(val, attr_class, None)
                    for item in items:
                        log(log_orange(f"{start}        {item}"))
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
        
