
#--------------------------------------------------------------------------------------------------
# AST Node

class Node:
    def __init__(self, name: str, lex: Lex = None, nodes: List['Node'] = None, error: str= ""):
        self.name = name; self.lex = lex; self.nodes = nodes or []; self.error = error
        self.length = self.compute_length()
    def __str__(self): 
        out = f"{self.name}: " if self.name else ""
        if self.lex: out += f"'{self.lex}' "
        if self.error: out += f"!! {self.error}"
        return out
    def __repr__(self): return self.__str__()
    def compute_length(self):
        if self.lex: return 1
        return sum([node.compute_length() for node in self.nodes])

def readout(node: Node, indent: int=0) -> str:
    out = "  "*indent + str(node) + "\n"
    for node in node.nodes:
        out += readout(node, indent+1)
    return out

def first_error_node(node: Node) -> Node:
    if isinstance(node, Lex): return ""
    if node.error: return node
    for n in node.nodes:
        error = first_error_node(n)
        if error: return error
    return ""

#--------------------------------------------------------------------------------------------------
# Parser
@log_indent
def parse_rule(rule: Rule, reader: Reader, end: int) -> Node:
    nodes = []
    for term in rule.terms:
        if term.is_terminal():
            match = reader.matches(term.terminals(), end)
            node = Node(term, match)
            nodes.append(node)
        else:
            nodes.append(parse_complex_term(term, reader, end))

def parse_complex_term(term: Term, reader: Reader, end: int) -> Node:
    min, max = get_min_max(term.dec)
    end = parse_scan_forward(term, reader, end)
    nodes = []
    while reader.pos < end and (max==None or len(nodes) < max):
        nodes.append(parse_term(term, reader, end))
        parse_separator(term, reader, end)
        
def parse_separator(term: Term, reader: Reader, end: int):
    if term.sep:
        if lex_matches(reader.peek(end), [f'"{term.sep}"']): reader.next(end)

def parse_error(term: Term, reader: Reader) -> Node:
    return Node(term.var or "", error=error_message(term, term.rule, reader.ls, reader.pos))

@log_indent
def parse_scan_forward(term: Term, reader: Reader, end: int) -> int:
    if term.is_terminal(): return reader.pos + 1
    if term.followers: return scan_forward(reader, end, term.followers)
    raise Exception(f"no followers for {term.name}")
