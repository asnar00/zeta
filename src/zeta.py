# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo
# zero to everything

from util import *
from parser import *

#--------------------------------------------------------------------------------------------------
# zero grammar

@this_is_the_test
def test_zero():
    print("test_zero")

class Zero(Language):
    def ext(self): return ".zero.md"
    def indent_char(self): return "" 
    def name_type(self): return True

    def feature(self): return label("feature", sequence(
            set("name", identifier()),
            optional(sequence(keyword("extends"), set("parent", identifier()))),
            block(list(self.component()))))
    def component(self): return any(self.test(), self.function(), self.struct(), self.local_variable())

    def test(self): return label("test", sequence(
            keyword(">"), set("eval", self.expression()),
            optional(keyword("=>"), set("expect", upto(":newline")))))
    
    def function(self): return label("function", sequence(
            set("modifier", enum("on", "replace", "after", "before")),
            set("result", brackets(self.name_type())),
            set("assign", enum("=", "<<")),
            set("signature", self.signature()),
            set("body", block(list(self.statement())))))
    
    def name_type(self): return any(
            sequence(set("name", identifier()), keyword(":"), set("type", identifier())),
            sequence(set("type", identifier()), set("name", identifier())))
    
    def signature(self): return brackets(list(any(
            set("word", identifier()),
            set("params", list(self.variable_decl(), ",")))))

    def variable_decl(self): return sequence(
            self.name_type(),
            optional(sequence(keyword("="), set("value", self.expression()))))
    
    def statement(self): return sequence(
            set("lhs", self.maybe_new_variable()),
            set("assign", enum("=", "<<")),
            set("rhs", self.expression()))
    
    def maybe_new_variable(self): return any(
            self.name_type(), set("name", identifier()))
    
    def expression(self): return any(self.constant(), self.variable(), self.function_call())

    def function_call(self): return list(any(
            set("word", identifier()),
            set("params", list(self.param_call(), ","))))
    
    def param_call(self): return sequence(
            optional(sequence(set("name", identifier()), keyword("="))), 
            set("value", self.expression()))
    
    def constant(self): return any(match_type("num"), match_type("str"))

    def variable(self): return identifier()

#--------------------------------------------------------------------------------------------------

def main():
    print("----------------------------------------------------------------")
    print("ᕦ(ツ)ᕤ zeta.py")
    test_run_all()

if __name__ == "__main__":
    main()
    print("done.")
