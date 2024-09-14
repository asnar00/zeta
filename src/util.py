# ᕦ(ツ)ᕤ
# util.py
# author: asnaroo
# proper logging, call-stack inspection, exception handling, testing, file system

import os
import re
from typing import Any, get_type_hints
import inspect
import traceback
import sys

#------------------------------------------------------------------------------
# logging

# set if logging is enabled
s_log: bool = False

# returns True if logging is enabled
def log_enabled() -> bool:
    return s_log

# log: prints stuff if logging is enabled
def log(*args):
    if s_log:
        print(*args)

# log_enable is a decorator that turns on logging within a function
def log_enable(fn):
    def wrapper(*args, **kwargs):
        global s_log
        old_log = s_log
        s_log = True
        result = fn(*args, **kwargs)
        s_log = old_log
        return result
    return wrapper

# log_disable is a decorator that turns off logging within a function
def log_disable(fn):
    def wrapper(*args, **kwargs):
        global s_log
        old_log = s_log
        s_log = False
        result = fn(*args, **kwargs)
        s_log = old_log
        return result
    return wrapper

# turns logging on or off, returns previous logging state
def log_set(on: bool) -> bool:
    global s_log
    old_log = s_log
    s_log = on
    return old_log

# clear the log : handy because we're constantly re-running
def log_clear():
    os.system('clear')  # For Linux/macOS

log_clear()
# returns a grey-coloured foreground, black background
def log_grey(str) -> str:
    return f'\033[30;1m{str}\033[0m'

# returns red-coloured foreground, black background
def log_red(str) -> str:
    return f'\033[31;1m{str}\033[0m'

# returns a green foreground, black backgroud
def log_green(str) -> str:
    return f'\033[32;1m{str}\033[0m'

# returns a grey-coloured background, white foreground
def log_grey_background(str) -> str:
    return f'\033[47;30m{str}\033[0m'

# strips all colour-related codes and dynamic things out of a string (so we can compare test results)
def log_strip(str) -> str:
    str = re.sub(r'\033\[[0-9;]*m', '', str)
    str = re.sub(r':\d+:', ':...:', str) # strip out :number:, replace with :...:
    str = re.sub(r"0x[0-9a-f]+", "...", str)  # replace any hex-addresses ("0x....") with "..."
    str = re.sub(r"\n +", "\n", str) # replace (cr followed by multiple spaces) with (cr)
    return str.strip()

# log_disclose shows CRs and tabs and spaces within a string
def log_disclose(str) -> str:
    return str.replace('\n', '↩︎\n').replace('\t', '▶︎').replace(' ', '_')

# returns a length-limited string version of anything
def log_short(obj: Any, maxLen=32) -> str:
    if obj == None: return "None"
    s = str(obj)
    m = re.match(r"<__main__\.(\w+)", s)    # if s matches "<__main__.ClassName", return "ClassName"
    if m: s = m.group(1)
    if len(s) <= maxLen: return s
    return s[:maxLen] + " " + log_grey("...") + " " + s[-12:]

#------------------------------------------------------------------------------
# testing

s_tests = []
s_active_test = None
s_tests_verbose = False
s_n_tests_failed = 0

# test_assert checks if two strings are equal, and prints a message if they're not
def test(name, a, b: str = None):
    ctx = caller()
    sa = log_strip(str(a))
    if not b:
        print(f"{log_grey(ctx)} {name}")
        print(f"{a}")
        return
    sb = log_strip(b)
    if sb.startswith("\n"): sb = sb[1:]
    if sa == sb:
        if s_tests_verbose: print(f"{log_grey(ctx)} {log_green("passed")} {name}")
    else:
        global s_n_tests_failed
        s_n_tests_failed += 1
        print(f"{log_grey(ctx)} {log_red("failed")} {name}")
        print("expected:")
        print(log_grey(sb)) # or print(log_disclose(sb)) if you want to see CRs and spaces
        print("got:")
        print(sa) # or print(log_disclose(sa)) if you want to see CRs and spaces

# decorator that wraps a function, adding it to s_tests
def this_is_a_test(fn):
    global s_tests
    s_tests.append(fn)
    return fn

# decorator that you use on the current active test (that you're working on)
def this_is_the_test(fn):
    global s_active_test
    s_active_test = fn

# run all tests
def test_run_all():
    global s_tests_verbose
    global s_n_tests_failed
    s_n_tests_failed = 0
    if s_tests_verbose:
        print("test ------------------------------------")
    global s_tests
    global s_active_test
    if not s_active_test:
        for test_fn in s_tests:
            test_fn()
        if s_n_tests_failed == 0:
            print(log_green("all tests passed."))
    if s_active_test:
        global s_log
        s_log = True
        s_tests_verbose = True
        s_active_test()
    

#------------------------------------------------------------------------------
# call-stack introspection

s_cwd = os.getcwd() + "/src/"

# returns 'file:line:' of the function that called our caller
def caller() -> str:
    frame = inspect.currentframe()
    frame = inspect.getouterframes(frame)[2]    # [1] would be the call to caller(), so...
    file = frame.filename.replace(s_cwd, '')    # dunno if we totally need this but 
    return f"{file}:{frame.lineno}:"

# extracts filename from a 'caller' string
def caller_file(caller: str) -> str:
    parts = caller.split(":")
    return parts[0]

# extracts line number from a 'caller' string
def caller_line(caller: str) -> int:
    parts = caller.split(":")
    return int(parts[1])

# get the actual line of source code
def caller_source(caller: str) -> str:
    parts = caller.split(":")
    file = parts[0]
    line = int(parts[1])
    return read_file(s_cwd + file).split('\n')[line-1]

# caller-show prints a complete call stack- all functions and parameters
def caller_show(maxLevels: int = None, verbose: bool = True) -> str:
    out = ""
    level = 1
    while maxLevels == None or level <= maxLevels:
        cs = caller_show_level(level, verbose)
        if cs == None: break
        out += cs + "\n"
        level += 1
    return out.rstrip()

# shows a single level of the callstack (0 is the caller of this function), including function-params
def caller_show_level(level: int, verbose: bool = True) -> str:
    level = level + 1
    # get stack frame at (level) levels above us
    frame = inspect.currentframe()
    outers = inspect.getouterframes(frame)
    if level >= len(outers): return None
    frame = outers[level]
    if frame == None: return None
    # get function name
    name = frame.function
    if name == "<module>": return None
    # get file and line number
    file = frame.filename.replace(s_cwd, '')
    line = frame.lineno
    loc = f"{file}:{line}: "
    out = log_grey(loc) + name
    if verbose:
        # get all the parameters
        args = inspect.getargvalues(frame.frame)
        for arg_name in args.args:
            arg_value = args.locals[arg_name]
            arg_type = type(arg_value).__name__
            out += f"\n{arg_name}: {arg_type} = "
            out += log_short(arg_value, 48)
    return out.rstrip()

# finds the argument value from a function higher up in the call-stack
def caller_get_arg(level: int, arg_name: str) -> Any:
    level = level + 1
    frame = inspect.currentframe()
    outers = inspect.getouterframes(frame)
    if level >= len(outers): return None
    frame = outers[level]
    if frame == None: return None
    args = inspect.getargvalues(frame.frame)
    return args.locals[arg_name] if arg_name in args.locals else None

#------------------------------------------------------------------------------
# exception handling

# prints a short exception message including a clickable line number
def exception_message(e: Exception) -> str:
    # Extract the traceback details from the exception
    tb = traceback.extract_tb(e.__traceback__)
    # Get the last frame (where the exception was raised)
    i_frame = len(tb) - 1
    last_frame = tb[i_frame]
    return log_grey(f"{last_frame.filename.replace(s_cwd, '')}:{last_frame.lineno}: ") + log_red("!!! " + str(e))

# Get the function signature with parameter names, types, and values.
def get_function_signature(func, frame):
    if inspect.ismethod(func):
        func = func.__func__
    sig = inspect.signature(func) # Get function's parameter specifications
    # Get type hints (if any)
    try:
        type_hints = get_type_hints(func)
    except:
        type_hints = {}
    # Get local variables from the frame
    local_vars = frame.f_locals
    # Build the signature string
    params = []
    for name, param in sig.parameters.items():
        param_type = type_hints.get(name, inspect.Parameter.empty)
        param_value = local_vars.get(name, inspect.Parameter.empty)
        param_str = name
        if param_value is not inspect.Parameter.empty:
            actual_type = type(param_value).__name__
            ps = f"{param_value!r}"
            ps = log_short(ps)
            if param_type is not inspect.Parameter.empty:
                if actual_type != param_type.__name__:
                    actual_type = log_red(actual_type)
                param_str += f": {actual_type}"
            param_str += f" = {ps}"
        params.append(param_str)  
    return ", ".join(params)

# exception handler: called when an exception is raised
def exception_handler(exc_type, exc_value, exc_traceback):
    msg = log_red(f"exception: {str(exc_value)!r}") + "\n"
    frames = []
    tb = exc_traceback
    while tb is not None:
        frame = tb.tb_frame
        filename = frame.f_code.co_filename.replace(s_cwd, '')
        function_name = frame.f_code.co_name
        lineno = tb.tb_lineno
        
        # Get the function object
        func = frame.f_globals.get(function_name)
        if func is None:
            func = frame.f_locals.get(function_name)
            if func is None and 'self' in frame.f_locals:
                # It might be a method called on self
                instance = frame.f_locals['self']
                func = getattr(instance.__class__, function_name, None)
                if func:
                    func = func.__get__(instance, instance.__class__)  # Bind the method
        
        
        if func is not None:
            # Get the function signature
            try:
                signature = get_function_signature(func, frame)
            except:
                signature = ""
        else:
            signature = ""
        
        frames.append((filename, lineno, function_name, signature))
        tb = tb.tb_next
    
    # Print frame information in reverse order
    for filename, lineno, function_name, signature in reversed(frames):
        if function_name.startswith("wrapper"): continue
        if function_name.startswith("<lambda>"): continue
        if function_name.startswith("<module>"): continue
        loc = log_grey(f"{filename}:{lineno}: ")
        msg += (f"{loc}{function_name} ({signature})") + "\n"
    print(msg)

# installs our exception handler
def exception_install_handler():
    sys.excepthook = exception_handler

def my_test_wrapper_func(n: str):
    return my_test_func(n)

def my_test_func(n: str):
   return(caller_show())

@this_is_a_test
def test_caller():
    test("caller", my_test_wrapper_func("yo"), """
util.py:...: my_test_func
n: str = yo
util.py:...: my_test_wrapper_func
n: str = yo
util.py:...: test_caller
util.py:...: test_run_all
zeta.py:...: main
    """)

def my_test_raiser(p: str):
    raise Exception("oops " + p)

def my_test_bad_func(p: str):
    my_test_raiser(p)

# exception readout can't be tested
# test exception: uncomment the following line to see a test exception trace readout
#@this_is_a_test
def test_exception():
    my_test_bad_func("hey")

#------------------------------------------------------------------------------
# file system

s_file_cache = {}  # maps filename => text

# read a file, cache it
def read_file(path: str) -> str:
    global s_file_cache
    if path in s_file_cache: 
        return s_file_cache[path]
    text = ""
    with open(path, "r") as file:
        text = file.read()
        s_file_cache[path] = text
    return text

# write a file, ensuring folders exist
def write_file(path: str, text: str):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as file:
        file.write(text)
    s_file_cache[path] = text


#------------------------------------------------------------------------------
# startup

log_clear()
exception_install_handler()