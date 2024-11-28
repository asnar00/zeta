# ᕦ(ツ)ᕤ
# zero.py
# author: asnaroo
# zero to anything

from typing import List
from util import *
from lexer import *
from grammar import *
from parser import *

#--------------------------------------------------------------------------------------------------
# modular Zero grammar using auto-generated classes

#--------------------------------------------------------------------------------------------------
# feature clause

def define_feature(grammar: Grammar):
    grammar.add("""
        NameDef := name:<identifier> ("|" alias:<identifier>)?
        Feature := "feature" name:NameDef ("extends" parent:Feature&)? "{" components:Component*; "}"
        Component
                """)

    test("feature_0", parse_code("", "Feature"), """
        Feature
            !!! premature end (expected 'feature', got 'None' at <eof>)
            name: NameDef = None
            parent: Feature = None
            components: List[Component] = None
        """)
    
    test("feature_1", parse_code("feature", "Feature"), """
        Feature
            name: NameDef
                !!! premature end (expected name:NameDef, got 'None' at <eof>)
            parent: Feature = None
            components: List[Component] = None
        """)

    test("feature_2", parse_code("feature MyFeature", "Feature"), """
        Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature = None
            components: List[Component] = None
        """)

    test("feature_3", parse_code("feature MyFeature extends", "Feature"), """
        Feature
            !!! mismatch (expected '{', got 'extends' at :...:19)
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature
                !!! premature end (expected parent:Feature&, got 'None' at <eof>)
            components: List[Component] = None
        """)
    
    test("feature_4", parse_code("feature MyFeature extends Another", "Feature"), """
        Feature
            !!! premature end (expected '{', got 'None' at <eof>)
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature => Another
            components: List[Component] = None        
        """)
    
    test("feature_5", parse_code("feature MyFeature {}", "Feature"), """
        Feature
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature = None
            components: List[Component] = []
        """)
    
    test("feature_6", parse_code("feature MyFeature extends Another {}", "Feature"), """
        Feature
            name: NameDef
                NameDef
                    name: str = MyFeature
                    alias: str = None
            parent: Feature => Another
            components: List[Component] = []    
        """)

#--------------------------------------------------------------------------------------------------
# main

@this_is_the_test
def test_zero_grammar():
    log("test_zero_grammar")
    grammar = Grammar()
    define_feature(grammar)