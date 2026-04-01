# rpc
*RPC endpoint for runtime evaluation*

## specification

Adds a `before` extension to `handle request` that intercepts `/@rpc/` paths and evaluates zero expressions at runtime. Non-RPC paths fall through to the next handler.

## interface

Get a variable by name:

    rpc eval ("port") => "8084"

Set a variable:

    rpc eval ("logo = hi") => "logo = hi"

Call a zero-arg function:

    rpc eval ("not found ()") => "not found"

## definition

    feature rpc extends website

    before (string body) = handle request (http-request request)
        if ((request.path) starts with ("/@rpc/"))
            string expr = substring of (request.path) from (6)
            body = rpc eval (expr)
