# rpc
*RPC endpoint for runtime evaluation*

## specification

Adds a `before` extension to `handle request` that intercepts `/@rpc/` paths and evaluates zero expressions at runtime. Non-RPC paths fall through to the next handler.

## interface

Get a variable by name:

    rpc eval ("port") => "8084"

Set a variable:

    rpc eval ("logo = hi") => "logo = hi"

Call a function:

    rpc eval ("not found ()") => "not found"
    rpc eval ("trim (\"  hello  \")") => "hello"
    rpc eval ("length of (\"test\")") => "4"

## definition

    feature rpc extends website

Intercept `/@rpc/` paths before the default handler. Extract the expression from the URL and evaluate it:

    before (string body) = handle request (Http-Request request)
        if ((request.path) starts with ("/@rpc/"))
            string expr = substring of (request.path) from (6)
            body = rpc eval (expr)
