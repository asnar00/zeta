   def resolve_symbols(self, e: Entity, scope: Any, errors:List[str], found:List[str], visited: List[Entity]=[]):
        if e in visited: return
        visited.append(e)
        original_scope = scope
        if hasattr(e, "get_scope"):
            scope = e.get_scope(self)
        disallow = hasattr(e, "disallow_resolve_children") and e.disallow_resolve_children()
        if not disallow:
            for attr in vars(e):
                if attr == "_error": continue
                attr_type = Grammar.current.get_attribute_type(e.__class__, attr)
                attr_value = getattr(e, attr)
                attr_actual_type = type(attr_value)
                if isinstance(attr_value, List):
                    resolved_list = []
                    for item in attr_value:
                        if isinstance(item, Entity):
                            self.resolve_symbols(item, scope, errors, found, visited)
                        elif isinstance(item, Lex):
                            actual_type = attr_type.replace("List[", "").replace("&]", "")
                            resolved_item = self.resolve_item(attr, item, actual_type, scope, errors, found)
                            if resolved_item==None:
                                resolved_list.append(log_red(str(item)))
                            else:
                                resolved_list.append(resolved_item)
                    if len(resolved_list) > 0:
                        setattr(e, attr, resolved_list)
                elif isinstance(attr_value, Entity):
                    self.resolve_symbols(attr_value, scope, errors, found, visited)
                elif isinstance(attr_value, Lex) and attr_type != "str":
                    item = self.resolve_item(attr, attr_value, attr_type, scope, errors, found)
                    if item: setattr(e, attr, item)
        if hasattr(e, "resolve"):
            cont = e.resolve(self, original_scope, errors, found)
            
    def resolve_item(self, attr: str, attr_value: Lex, attr_type: str, scope: Any, errors: List[str], found: List[str]) -> Any:
        location = attr_value.location()
        str_value = attr_value.val
        attr_class = Grammar.current.get_class(attr_type)
        items = self.find(str_value, attr_class, scope)
        if len(items) == 0:
            log(log_red(f"can't find {attr_value} in {scope}"))
            errors.append(f"can't find {attr_value} in {scope}")
            return None
        elif len(items) > 1:
            log(log_red(f"multiple matches for {attr_value}[{attr_type}] in {scope}"))
            errors.append(f"multiple matches for {attr_value}[{attr_type}] in {scope}")
            return None
        else:
            found.append(f"found {attr}:{items[0]} at {location}")
            return items[0].element
