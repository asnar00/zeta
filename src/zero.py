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
    on (string result$) << hello(string name)
        int my_var = 0
        result$ << "hello \(name)"
    replace run()
        out$ << hello("world")
"""

@this_is_the_test
def test_zero():
    log("test_zero")
    ast = parse(s_test_program, "feature")
    if "_error" in ast:
        log(ast)
        log_exit()
    log_clear()
    st = build_feature_symbol_tables(ast)

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
    log("----")
    show_st("feature st: ", st)

# updates feature's symbol table with function names/aliases
# and computes internal sts for the function
def build_function_st(st, function_ast) -> Dict:
    log("function:",  function_shortname(function_ast))
    # the function words
    for i, item in enumerate(function_ast["signature"]["_list"]):
        if item["_type"] in ["word", "operator"]:
            merge(st, item["_lex"], (function_ast, i))
    function_st = {}
    # the function result
    if "result" in function_ast:
        result = function_ast["result"]
        for item in result["_list"]:
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
            log("lhs:", lhs)
            add_multiple_to_st(function_st, lhs["names"], lhs, i+1)
    show_st("function st:", function_st)
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
    log(out)

# merge child_st into parent_st, child_st gets modified, and takes precedence
def merge_st(child_st: Dict, parent_st: Dict):
    for key, list in parent_st.items():
        if not key in child_st:
            child_st[key] = list
