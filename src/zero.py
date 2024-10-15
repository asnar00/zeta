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

@this_is_the_test
def test_zero():
    log("test_zero")
    ast = parse(s_test_program, "feature")