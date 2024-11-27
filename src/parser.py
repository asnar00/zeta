# ᕦ(ツ)ᕤ
# parser.py
# author: asnaroo
# zero to anything

from typing import Type
from util import *
from lexer import *
from grammar import *

#--------------------------------------------------------------------------------------------------
# Parser

# main parser routines:

@log_indent
def parse_rule(rule: Rule, reader: Reader, entity: Entity):
    pass


#--------------------------------------------------------------------------------------------------

def parse_code(code: str, cls: Type[Entity]) -> Entity:
    ls = lexer(Source(code=code))
    reader = Reader(ls)
    return parse_rule(cls.rule(), reader, cls())
