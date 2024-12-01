# ᕦ(ツ)ᕤ
# classes.py
# author: asnaroo
# zero to anything

from typing import List, Type, Dict
from util import *

#--------------------------------------------------------------------------------------------------
# manages auto-generated classes

class ClassManager:
    def __init__(self):
        self.namespace = {'List': List}       # holds all classes created from rules
        self.class_methods = {}     # class => [method_def]
        self.class_types = {}       # class.name => type name

    # given { name: type, ... }, builds a class and returns it
    def build_class(self, name: str, parent: str, attributes: Dict[str, str]) -> Type:
        class_def = log_deindent(f"""
            class {name}({parent}):
                def __init__(self):
                    super().__init__()
            """)
        for attribute_name, attribute_type in attributes.items():
            ref = ""
            if "&" in attribute_type:
                ref = "        # ref"
                attribute_type = attribute_type.replace("&", "")
            class_def += f"        self.{attribute_name}: {attribute_type} = None{ref}\n"
            self.class_types[f"{name}.{attribute_name}"] = attribute_type
        exec(class_def, self.namespace)
        cls = self.namespace[name]
        return cls
    
    # given class name, returns the class
    def find_class(self, name: str) -> Type:
        return self.namespace[name]
    
    # pokes a class into the namespace
    def add_class(self, cls: Type):
        self.namespace[cls.__name__] = cls
    
    # given class name and attribute name, return type
    def get_attribute_type(self, cls: Type, name: str) -> str:
        return self.class_types[f"{cls.__name__}.{name}"]
    
    # adds a method to a class; gets automatically re-added if the class is rebuilt
    def add_method_to_class(self, class_name: str, method_def: str):
        method_def = log_deindent(method_def)
        if not class_name in self.class_methods: self.class_methods[class_name] = []
        self.class_methods[class_name].append(method_def)
        method_name = self.get_method_name(method_def)
        try:
            exec(method_def, self.namespace)  # adds method to existing namespace
            setattr(self.namespace[class_name], method_name, self.namespace[method_name])
        except Exception as e:
            log(f"error adding method {method_name} to class {class_name}: {e}")
            log(method_def)
            log_exit()

    def get_method_name(self, method_def: str) -> str:
        pattern = r'\s*def\s+(\w+)\('
        match = re.search(pattern, method_def)
        if match: method_name = match.group(1)
        else: raise Exception(f"can't find method name in {method_def}")
        return method_name

