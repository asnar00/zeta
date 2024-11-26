# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from util import *
from parser10 import *
from typing import List, Dict, Tuple, Union

#--------------------------------------------------------------------------------------------------
s_test_program = """
feature Hello
    type str | string =
        char c$
    string out$ | output$
    on (string result$, int yi) << hello(string name)
        int my_var = yi + 20
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
    if has_errors(ast):
        errors = get_ast_errors(ast)
        log("errors:")
        for error in errors:
            log(error)
        log_exit()
    log_clear()
    test_symbol_tables(ast)
    test_resolve_expressions(ast)
    
#--------------------------------------------------------------------------------------------------
# symbol tables

def test_symbol_tables(ast):
    log("test_symbol_tables")
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
"result$" => variable:0
"yi" => variable:0
"name" => variable:0
"my_var" => variable:1
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
        if item["_rule"] in ["word", "operator"]:
            name += str(item["_val"])
        else: name += "_"
    return name

#--------------------------------------------------------------------------------------------------
# symbol table: maps name => (item, index)

# sets feature_ast["_st"] to a symbol table of all feature-scope stuff
@log_indent
def build_feature_symbol_tables(feature_ast):
    st = {}
    for component in feature_ast["body"]["_list"]:
        _rule = component["_rule"]
        if _rule == "type": build_type_st(st, component)
        elif _rule == "variable": build_variable_st(st, component)
        elif _rule == "function": build_function_st(st, component)
    feature_ast["_st"] = st
    # merge down feature scope into each function scope
    for component in feature_ast["body"]["_list"]:
        if component["_rule"] == "function":
            merge_st(component["_st"], st)
    return st

def show_symbol_tables(feature_ast: Dict) -> str:
    out = show_st("feature st: ", feature_ast["_st"])
    for component in feature_ast["body"]["_list"]:
        if component["_rule"] == "function":
            out += show_st(f"function st: {function_shortname(component)}", component["_st"])
    return out

# updates feature's symbol table with function names/aliases
# and computes internal sts for the function
@log_indent
def build_function_st(st, function_ast) -> Dict:
    # the function words
    for i, item in enumerate(function_ast["signature"]["_list"]):
        if item["_rule"] in ["word", "operator"]:
            merge(st, item["_val"], (function_ast, i))
    function_st = {}
    # the function result
    if "result" in function_ast:
        result = function_ast["result"]
        log(log_green("result:"))
        log(format(result))
        for item in result["_list"]:
            type = item["type"]
            names = item["names"]["_list"] # declaring multiple vars
            for name in names:
                variable = { "_rule": "variable", "type": type, "name": name["name"] }
                if "alias" in name: variable["alias"] = name["alias"]
                add_to_st(function_st, variable, variable)
    # the function parameters
    for i, item in enumerate(function_ast["signature"]["_list"]):
        if item["_rule"] == "param_group":
            for param in item["_list"]:
                add_multiple_to_st(function_st, param["names"]["_list"], param, 0)
    # and finally the statements
    for i, statement in enumerate(function_ast["body"]["_list"]):
        lhs = statement["lhs"]
        if "names" in lhs:
            variable = { "_rule" : "variable", "type" : lhs["type"], "names" : lhs["names"]}
            log(log_green("lhs:"), variable)
            add_multiple_to_st(function_st, lhs["names"]["_list"], variable, i+1)
    function_ast["_st"] = function_st
    return st

# updates feature's symbol table, and sets an internal one within the type
@log_indent
def build_type_st(st, type_ast) -> Dict:
    add_to_st(st, type_ast, type_ast)
    type_st = {}
    for property in type_ast["properties"]["_list"]:
        add_multiple_to_st(type_st, property["names"]["_list"], property)
    type_ast["_st"] = type_st
    return st

# updates feature's symbol table with variable names/aliases
@log_indent
def build_variable_st(st, variable_ast) -> Dict:
    add_multiple_to_st(st, variable_ast["names"]["_list"], variable_ast)
    return st

# given a list of name/alias, add each of them to the st
@log_indent
def add_multiple_to_st(st, names: List[Dict], item_to_add, index:int =0) -> Dict:
    for name in names:
        add_to_st(st, name, item_to_add, index)
    return st

# adds a single name/alias to the st
@log_indent
def add_to_st(st, component, item_to_add, index:int =0) -> Dict:
    name = component["name"]
    merge(st, name, (item_to_add, index))
    if "alias" in component:
        merge(st, component["alias"], (item_to_add, index))
    return st

# compact printout of symbol table
def show_st(label, st: Dict)-> str:
    out = label + "\n"
    for key, list in st.items():
        out += f'   "{key}" => '
        for item in list:
            out += f"{item[0]["_rule"]}:{item[1]}"
        out += "\n"
    return out

# merge child_st into parent_st, child_st gets modified, and takes precedence
def merge_st(child_st: Dict, parent_st: Dict):
    for key, list in parent_st.items():
        if not key in child_st:
            child_st[key] = list

#--------------------------------------------------------------------------------------------------
# resolve expressions

def test_resolve_expressions(ast):
    log("test_resolve_expressions")
    log_max_depth(20)
    resolve_expressions(ast, ast["_st"])

def resolve_expressions(ast, st):
    if isinstance(ast, Dict):
        new_st = ast["_st"] if "_st" in ast else st
        if "_rule" in ast and ast["_rule"] == "expression": 
            resolve_expression(ast, new_st)
        else:
            for key, value in ast.items():
                if key == "_rule": continue
                if isinstance(value, Dict):
                    resolve_expressions(value, new_st)
                elif isinstance(value, List):
                    for item in value:
                        resolve_expressions(item, new_st)


# resolve expressions to function calls and variable references
@log_indent
def resolve_expression(ast, st):
    log(f"expression: {ast}")
    log(f"st: {dbg_st(st)}")
    items = ast["_list"]
    function_found = None
    function_index = None
    for item in items:
        if item["_rule"] == "brackets":
            resolve_expressions(item, st)
        elif item["_rule"] == "word":
            log("word:", item["_val"])
            key = str(item["_val"])
            if not (key in st):
                log(log_red(f"word not found: {item["_val"].dbg()}"))
                item["_error"] = [{ "_err" : "unknown word", "_got" : key, "_at" : item["_val"].location()}]
            else:
                list = st[key]
                if len(list) > 1:log(log_red("ambiguous word"))
                object = list[0][0]
                index = list[0][1]
                object_rule = object["_rule"]
                log(log_green(f"word found! {object["_rule"]}:{index}"))
                if object_rule == "variable":
                    item["_variable"] = key
                elif object_rule == "function":
                    function_name = function_shortname(object)
                    item["_function"] = function_name
                    item["_index"] = index
                    if function_found is None:
                        function_found = function_name
                        function_index = index
                    else:
                        if function_name != function_found or index != function_index + 1:
                            log(log_red("ambiguous function"))
                            item["_error"] = [{ "_err" : "ambiguous function", "_expected" : function_found, "_got" : function_name, "_at" : item["_val"].location()}]
                        else:
                            function_index = index

def dbg_st(st):
    out = "("
    for key, list in st.items():
        out += f"{key} "
    return out + ")"
