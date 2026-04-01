# howto
*quick reference for building and running zero programs*

## build

Compile a zero program from its features.md listing:

    python3 zeta.py website/features.md website/output/

Use `--verbose` for detailed build output:

    python3 zeta.py --verbose website/features.md website/output/

## run

Start a compiled program in the background:

    ./zero.sh website

Start in foreground (for debugging):

    ./zero.sh --fg website

Specify target language (defaults to .py):

    ./zero.sh website.ts

## stop

    ./zero.sh --stop website

## restart

    ./zero.sh --restart website

## test

Run all tests from the command line:

    ./zero.sh --test website

Run tests for a specific feature:

    ./zero.sh --test website not-found

Run tests via HTTP on a running server:

    /@rpc/test ()
    /@rpc/test ("not-found")

## inspect

List all features, variables, and functions:

    /@rpc/

Get a variable:

    /@rpc/port

Set a variable:

    /@rpc/landing-page-enabled = false

Call a function:

    /@rpc/not found ()
    /@rpc/trim ("  hello  ")

## features.md format

Each line: feature name, optional flags and config:

    website port=8084
    not-found
    rpc
    landing-page dynamic

- Names are searched automatically (no path needed)
- `dynamic` wraps the feature's extensions in a runtime enable guard
- `key=value` overrides feature-scoped variables

## feature file structure

Each `.zero.md` file has three sections:

    # name
    *one-line summary*

    ## specification
    What this feature does and why.

    ## interface
    Function signatures with test examples:
        not found () => "not found"
        trim ("  hello  ") => "hello"

    ## definition
        feature name [extends parent]
        ... zero code ...

## extensions

Add behaviour before an existing function:

    before (string body) = handle request (http-request request)
        if (request.path == "/")
            body = landing page ()

Replace a function body:

    replace (string body) = handle request (http-request request)
        body = not found ()

Assigning the result variable ends the function (early return). Later extensions get first crack (before prepends).

## dynamic features

Mark a feature as `dynamic` in features.md to enable runtime toggling:

    landing-page dynamic

This injects a `landing-page-enabled` bool that guards the feature's extensions. Toggle via RPC:

    /@rpc/landing-page-enabled = false
    /@rpc/landing-page-enabled = true

## graceful shutdown

    /@rpc/stop ()
    /@rpc/exit process ()

`stop ()` calls the lifecycle hook (extendable by features). `exit process ()` terminates the server after a short delay.
