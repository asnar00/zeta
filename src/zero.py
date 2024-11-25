# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from util import *
from parser import *
from typing import List, Dict, Tuple, Union

#--------------------------------------------------------------------------------------------------
s_test_program = """
feature Hello
    type str | string =
        char c$
    string out$ | output$
    on (string result$, int yi) << hello(string name)
        int my_var = 0
        result$ << "hello \(name)"
    replace run()
        out$ << hello("world")
"""

@log_disable
def parse(code: str) -> Dict:
    ls = lexer(Source(code=code))
    reader = Reader(ls)
    return parse_rule(Grammar.current.rule_named["feature"], reader)

@this_is_the_test
def test_zero():
    log("test_zero")
    grammar = build_grammar(s_zero_grammar_spec)
    log_max_depth(3)
    ast = parse(s_test_program)
    log(format(ast))
    log_exit()


def someFunc():
    ast = parse(s_test_program, "feature")
    log(ast)
    log_exit()
    if "_error" in ast:
        log(ast)
        log_exit()
    log_clear()
    st = build_feature_symbol_tables(ast)
    test("symbol_tables", show_symbol_tables(ast), """
feature st: 
  "str" => type:0
  "string" => type:0
  "out$" => variable:0
  "output$" => variable:0
  "hello" => function:0
  "run" => function:0
function st: hello_
  "result$" => result_vars:0
  "name" => variable:0
  "my_var" => c_name_type:1
  "str" => type:0
  "string" => type:0
  "out$" => variable:0
  "output$" => variable:0
  "hello" => function:0
  "run" => function:0
function st: run_
  "str" => type:0
  "string" => type:0
  "out$" => variable:0
  "output$" => variable:0
  "hello" => function:0
  "run" => function:0
         """)

#--------------------------------------------------------------------------------------------------
# helpers

# merge an item into a dictionary mapping str-> list[x]
def merge(dict, key, item):
    if not isinstance(key, str): key = str(key)
    if key in dict:
        if not (item in dict[key]):
            dict[key].append(item)
    else:
        dict[key] = [item]

# function shortname
def function_shortname(function_ast) -> str:
    name = ""
    for item in function_ast["signature"]["_list"]:
        if item["_type"] in ["word", "operator"]:
            name += str(item["_lex"])
        else: name += "_"
    return name

#--------------------------------------------------------------------------------------------------
# symbol table: maps name => (item, index)

# sets feature_ast["_st"] to a symbol table of all feature-scope stuff
def build_feature_symbol_tables(feature_ast):
    st = {}
    for component in feature_ast["body"]:
        _type = component["_type"]
        if _type == "type": build_type_st(st, component)
        elif _type == "variable": build_variable_st(st, component)
        elif _type == "function": build_function_st(st, component)
    feature_ast["_st"] = st
    # merge down feature scope into each function scope
    for component in feature_ast["body"]:
        if component["_type"] == "function":
            merge_st(component["_st"], st)

def show_symbol_tables(feature_ast: Dict) -> str:
    out = show_st("feature st: ", feature_ast["_st"])
    for component in feature_ast["body"]:
        if component["_type"] == "function":
            out += show_st(f"function st: {function_shortname(component)}", component["_st"])
    return out

# updates feature's symbol table with function names/aliases
# and computes internal sts for the function
def build_function_st(st, function_ast) -> Dict:
    # the function words
    for i, item in enumerate(function_ast["signature"]["_list"]):
        if item["_type"] in ["word", "operator"]:
            merge(st, item["_lex"], (function_ast, i))
    function_st = {}
    # the function result
    if "result" in function_ast:
        result = function_ast["result"]
        log(format(result))
        for item in result["_list"]:
            type = item["type"]
            names = item["names"] # declaring multiple vars
            for name in names:
                variable = { "_type": "variable", "type": type, "name": name["name"] }
                if "alias" in name: variable["alias"] = name["alias"]
                log(variable)
        log_exit()
            
        add_multiple_to_st(function_st, item["names"], result)
    # the function parameters
    for i, item in enumerate(function_ast["signature"]["_list"]):
        if item["_type"] == "param_group":
            for param in item["_list"]:
                add_multiple_to_st(function_st, param["names"], param, 0)
    # and finally the statements
    for i, statement in enumerate(function_ast["body"]["_list"]):
        lhs = statement["lhs"]
        if "names" in lhs:
            add_multiple_to_st(function_st, lhs["names"], lhs, i+1)
    function_ast["_st"] = function_st
    return st

# updates feature's symbol table, and sets an internal one within the type
def build_type_st(st, type_ast) -> Dict:
    add_to_st(st, type_ast, type_ast)
    type_st = {}
    for property in type_ast["properties"]:
        add_multiple_to_st(type_st, property["names"], property)
    type_ast["_st"] = type_st
    return st

# updates feature's symbol table with variable names/aliases
def build_variable_st(st, variable_ast) -> Dict:
    add_multiple_to_st(st, variable_ast["names"], variable_ast)
    return st

# given a list of name/alias, add each of them to the st
def add_multiple_to_st(st, names: List[Dict], item_to_add, index:int =0) -> Dict:
    for name in names:
        add_to_st(st, name, item_to_add, index)
    return st

# adds a single name/alias to the st
def add_to_st(st, component, item_to_add, index:int =0) -> Dict:
    name = component["name"]
    merge(st, name, (item_to_add, index))
    if "alias" in component:
        merge(st, component["alias"], (item_to_add, index))

# compact printout of symbol table
def show_st(label, st: Dict)-> str:
    out = label + "\n"
    for key, list in st.items():
        out += f'  "{key}" => '
        for item in list:
            out += f"{item[0]["_type"]}:{item[1]}"
        out += "\n"
    return out

# merge child_st into parent_st, child_st gets modified, and takes precedence
def merge_st(child_st: Dict, parent_st: Dict):
    for key, list in parent_st.items():
        if not key in child_st:
            child_st[key] = list

#--------------------------------------------------------------------------------------------------
# resolve expression

# resolve expressions to function calls and variable references
def resolve_expression(expr_ast : Dict):
    pass