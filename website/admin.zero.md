# admin
*HTTP bridge for runtime feature management*

## specification

Intercepts `/@admin/` paths to get and set feature variables at runtime.

    /@admin/set/landing_page_enabled/false => "landing_page_enabled = false"
    /@admin/get/landing_page_enabled => "true"

## definition

    feature admin extends website

    before (string body) = handle request (http_request request)
        if ((request.path) starts with ("/@admin/"))
            body = handle admin (request)

    on (string body) = handle admin (http_request request)
        string parts$ = split (request.path) by ("/")
        string action = parts$[2]
        if (action == "set")
            string name = parts$[3]
            string value = parts$[4]
            set feature var (name) (value)
            body = name + " = " + value
        else if (action == "get")
            string name = parts$[3]
            body = get feature var (name)
        else
            body = "unknown action: " + action
