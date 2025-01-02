#@log_suppress
    def build_symbol_table(self, ast: Entity) -> SymbolTable:
        st = SymbolTable()
        log("adding symbols for ast")
        st.add_symbols(ast, None)
        #log(st.dbg())
        return st
       
    def resolve_symbols(self, cp: CompiledProgram):
        errors = []
        found = []
        visited = set()
        for key, items in cp.st.symbols.items():
            for item in items:
                cp.st.resolve_symbols(item.element, item.scope, errors, found, visited)
        cp.st.resolve_symbols(cp.ast, None, errors, found, visited)
        log(print_code_formatted(cp.ast, use_aliases=False))
        if len(errors) > 0:
            log("errors:", "\n".join(errors))
            log_exit("errors found")

    def check_types(self, cp: CompiledProgram):
        log("checking types------------------------------------------------------------")
        errors = []
        self.check_types_rec(cp.ast, None, errors, cp.st, set())
        if len(errors) > 0:
            log("errors:", "\n".join(errors))
            log_exit("errors found")
    
    def check_types_rec(self, e: Entity, scope: Any, errors: List[str], symbol_table: SymbolTable, visited: Set, indent: int = 0):
        start = "  " * indent
        if e in visited: return
        if not isinstance(e, Entity): return
        visited.add(e)
        #log(f"{start}{e} in {scope}")
        if hasattr(e, "get_scope"): scope = e.get_scope() or scope
        for attr in vars(e):
            attr_val = getattr(e, attr)
            if attr_val is None: continue
            if not isinstance(attr_val, List): attr_val = [attr_val]
            if len(attr_val) > 0 and isinstance(attr_val[0], Entity):
                for item in attr_val:
                    self.check_types_rec(item, scope, errors, symbol_table, visited, indent+1)
        if hasattr(e, "check_type"):
            e.check_type(symbol_table, scope, errors)
