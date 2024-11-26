# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from typing import Type
from util import *
from lexer import *
from grammar import *
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------

def parse_code(code: str, cls: Type[Entity]) -> Entity:
    ls = lexer(Source(code=code))
    reader = Reader(ls)
    return "not implemented"
