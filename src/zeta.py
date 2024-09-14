# ᕦ(ツ)ᕤ
# zeta.py
# author: asnaroo
# zero to anything

from util import *
from parser import *

#--------------------------------------------------------------------------------------------------
# zero grammar

@this_is_the_test
def test_zero():
    log("test_zero")
    zero = Zero()
    test("variable", parse(zero.variable(), "x"), """
         {'_type': 'variable', 'name': x}                       """)
    test("constant_num", parse(zero.constant(), "123"), """
         {'_type': 'constant', 'value': 123}                    """)
    test("constant_str", parse(zero.constant(), '"abc\"'), """
         {'_type': 'constant', 'value': "abc"}                  """)
    test("param_call_var", parse(zero.param_call(), "a"), """
         {'value': {'_type': 'variable', 'name': a}}            """)
    test("param_call_str", parse(zero.param_call(), "str = \"hello world\""), """
        {'name': str, 'value': {'_type': 'constant', 'value': \"hello world\"}} """)
    test("param_call_num", parse(zero.param_call(), "index = 123"), """
        {'name': index, 'value': {'_type': 'constant', 'value': 123}} """)
    
    

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
            set("word", any(identifier(), self.operator())),
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
    
    def expression(self): return any(self.variable(), self.constant(), self.function_call())

    def function_call(self): return list(any(
            set("word", any(identifier(), operator())),
            set("params", brackets(list(self.param_call(), ",")))))
    
    def param_call(self): return sequence(
            optional(sequence(set("name", identifier()), keyword("="))), 
            set("value", recurse(self.expression)))
    
    def constant(self): return label("constant", set("value", any(match_type("num"), match_type("str"))))

    def variable(self): return label("variable", set("name", identifier()))

#--------------------------------------------------------------------------------------------------

def main():
    print("----------------------------------------------------------------")
    print("ᕦ(ツ)ᕤ zeta.py")
    test_run_all()

if __name__ == "__main__":
    main()
    print("done.")
