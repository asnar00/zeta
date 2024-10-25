# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from util import *
from parser import *
from typing import List, Dict, Tuple, Union
import json

#--------------------------------------------------------------------------------------------------
s_test_program = """
feature Hello
    type str | string =
        char c$
    string out$ | output$
    on hello(string name)
        out$ << "hello \(name)"
    replace run()
        hello("world")
"""

@this_is_the_test
def test_zero():
    log("test_zero")
    ast = parse(s_test_program, "feature")
    if "_error" in ast:
        log(ast)
        log_exit()
    log_clear()
    analyse_ast(ast)

#--------------------------------------------------------------------------------------------------
# helpers

def format_ast(ast) -> str:
    json_string = json.dumps(ast, default=lambda x: x.dbg() if isinstance(x, Lex) else str(x), indent=4)
    return json_string


# merge an item into a dictionary mapping str-> list[x]
def merge(dict, key, item):
    if key in dict:
        if not (item in dict[key]):
            dict[key].append(item)
    else:
        dict[key] = [item]

#--------------------------------------------------------------------------------------------------
# semantic analysis here

def analyse_ast(ast: Dict):
    analyse_types(ast)
    analyse_variables(ast)
    analyse_functions(ast)
    analyse_tests(ast)

# extract types, add them to the index
def analyse_types(ast):
    ast["types"] = {}
    for c in ast["body"]:
        if not (c["_type"] == "type"): continue
        name = str(c["name"])
        merge(ast["types"], name, c)
        if "alias" in c:
            alias = str(c["alias"])
            merge(ast["types"], alias, c)

# variable declarations, enter them into feature scope
def analyse_variables(ast):
    ast["variables"] = {}
    for c in ast["body"]:
        if not (c["_type"] == "variable"): continue
        for n in c["names"]:
            name = str(n["name"])
            merge(ast["variables"], name, c)
            if "alias" in n:
                alias = str(n["alias"])
                merge(ast["variables"], alias, c)

# function declarations, enter into feature scope and analyse body
def analyse_functions(ast):
    ast["functions"] = {}
    for c in ast["body"]:
        if not (c["_type"] == "function"): continue
        name = function_name(c["signature"])
        merge(ast["functions"], name, c)
        analyse_function(c)

# test declarations, add them to feature test array
def analyse_tests(ast):
    ast["tests"] = []
    for c in ast["body"]:
        if not (c["_type"] == "test"): continue
        ast["tests"].append(c)

# given a function signature, derive a simple name
def function_name(sig: Dict):
    name = ""
    for item in sig["_list"]:
        if item["_type"] in ["word", "operator"]:
            name += str(item["_lex"])
        else:
            name += "()" # todo: actual parameter types should go here
    return name

#--------------------------------------------------------------------------------------------------

# function analysis: where the real work happens
def analyse_function(f: Dict):
    log(format_ast(f))
    scope = {}      # maps name => (variable, i_statement)
    scope = get_parameters(f)
    pass

# get parameters of function into a scope (name => (variable, i_statement))
def get_parameters(f: Dict) -> Dict:
    pass