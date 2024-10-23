# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from util import *
from parser import *
from typing import List, Dict, Tuple, Union

s_test_program = """
feature Hello
    string out$
    on hello()
        out$ << "hello world"
    on run()
        hello()
"""

def test_zero():
    log("test_zero")
    ast = parse(s_test_program, "feature")
    log_clear()
    ast = analyse_ast(ast)
    log(format_ast(ast))

def format_ast(ast: Dict, indent: int =0) -> str:
    out = ""
    type = node_type(ast)
    if not type: raise Exception("format_ast: unknown node type")
    out += f"{indent*" "}_{type}: "
    rest = first_item(ast)
    if isinstance(rest, Dict):
        out += "\n"
        for key, item in rest.items():
            out += f'{(indent+2)*" "}{key}: '
            if isinstance(item, Lex): out += f'{item.val}'
            elif isinstance(item, List):
                if len(item)==0: out += "[]"
                else:
                    out += "[\n"
                    for sub_item in item:
                        sub_str = format_ast(sub_item, indent+4)
                        out += f'{sub_str}'
                    out += f'{(indent+2)*" "}]'
            else:
                out += "\n"
                out += format_ast(item, indent+4)
            out += "\n"
    elif isinstance(rest, List):
        if len(rest)==0: out += "[]"
        else:
            out += "["
            out += "\n"
            for sub_item in rest:
                sub_str = format_ast(sub_item, indent+4)
                out += f'{sub_str}\n'
            out += f'\n{(indent)*" "}]'
    elif isinstance(rest, Lex):
        out += f'{rest.val}'
    return out

def node_type(ast: Dict) -> str:
    first_key= list(ast.keys())[0]
    return first_key[1:] if first_key.startswith("_") else None

def first_item(ast: Dict) -> str:
    return ast[list(ast.keys())[0]]

def analyse_ast(ast: Dict) -> Dict:
    type = node_type(ast)
    if type == "feature":
        return analyse_feature(ast)
    elif type == "function":
        return analyse_function(ast)
    elif type == "expression":
        return analyse_expression(ast)
    
def analyse_feature(ast: Dict) -> Dict:
    return ast

def analyse_function(ast: Dict) -> Dict:
    return ast

def analyse_expression(ast: Dict) -> Dict:
    return ast



