# http
*http server*

## specification

HTTP server platform. `serve http` listens on a port and produces a stream of requests. `response$` is a stream of responses paired to requests by the `http_response` type.

## interface

    type http_request
        string path = ""
        string method = ""

    type http_response
        http_request request
        string body = ""

    http_response response$

    on (http_request request$) <- serve http (int port)
