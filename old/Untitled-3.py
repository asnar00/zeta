
class Node:
    def __init__(self, rule: str, nodes: List[Union[Dict, Lex]], error: str=None):
        self.rule = rule; self.nodes = nodes; self.error = error or ""
    def __str__(self): 
        val = ""
        if len(self.nodes) ==1 and isinstance(self.nodes[0], Lex):
            val = " '" + str(self.nodes[0]) + "'"
        rule = f"{self.rule}" if self.rule else ""
        return f"{rule}: {val}{self.error or ""}"
    def __repr__(self): return self.__str__()

def readout(obj: Union[Node, Lex], indent: int=0) -> str:
    if isinstance(obj, Lex): return "    " * indent + "'" + str(obj) + "'" + "\n"
    out = "    " * indent + obj.rule + " " + obj.error 
    out += "\n"
    for node in obj.nodes:
        out += readout(node, indent+1)
    return out

def error(obj: Union[Node, Lex]) -> bool:
    if isinstance(obj, Lex): return False
    return obj.error != None

def can_continue(node, term: Term) -> bool:
    if node.error:
        if term.dec and term.dec in "*?": return True
        elif term.dec == "+": return len(node.nodes) > 0
        else: return False
    return True

#--------------------------------------------------------------------------------------------------
# Parser itself

def parse(code: str, rule_name: str="feature") -> Node:
    ls = lexer(Source(code=code))
    reader = Reader(ls)
    rule = s_grammar.rule_named[rule_name]
    node = parse_rule(rule, reader)
    return readout(node)

@log_indent
def parse_rule(rule: Rule, reader: Reader) -> Dict:
    nodes = []
    child = ""
    for term in rule.terms:
        child = parse_term(term, rule, reader)
        if error(child) and not can_continue(child, term): break
        nodes.append(child)
    error_message = child.error if child and error(child) else ""
    return Node(rule.name, nodes, error=error_message)

@log_indent
def parse_term(term: Term, rule: Rule, reader: Reader) -> Node:
    if term.dec == "": return parse_single_term(term, rule, reader)
    elif term.dec == "*": return parse_var_term(term, rule, reader, 0, None)
    elif term.dec == "+": return parse_var_term(term, rule, reader, 1, None)
    elif term.dec == "?": return parse_var_term(term, rule, reader, 0, 1)
    else: raise Exception(f"unknown dec {term.dec}")

@log_indent
def parse_var_term(term: Term, rule: Rule, reader: Reader, min: int, max: int) -> Node:
    nodes = []
    while (max==None or len(nodes) < max):
        node = parse_single_term(term, rule, reader)
        nodes.append(node)
        if error(node): break
    return Node(term.dec, nodes)

@log_indent
def parse_single_term(term: Term, rule: Rule, reader: Reader) -> Node:
    if term.is_terminal():
        if lex_matches(reader.peek(), term.vals): return reader.next()
        else: return Node("", [], error_message(term, rule, reader.ls, reader.pos))
    rules = reduce_rules(term, reader.peek())
    if len(rules) == 0: 
        return Node("", [], error_message(term, rule, reader.ls, reader.pos))
    start_pos = reader.pos
    node = None
    for rule in rules:
        node = parse_rule(rule, reader)
        if not node.error: return node
        reader.pos = start_pos
    error = node.error if node else error_message(term, rule, reader.ls, reader.pos)
    return Node("", [], error=error)

#--------------------------------------------------------------------------------------------------
# Tests

@this_is_the_test
def test_parser():
    log("test_parser")
    grammar = build_grammar(s_zero_grammar_spec)
    test("feature", parse("feature MyFeature extends Another{ > a }"))