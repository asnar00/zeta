# background
*per-user background colour*

## specification

Adds a per-user background colour to the landing page. Uses `after` to post-process the landing page HTML, replacing the default teal with the user's colour.

## definition

    feature background extends landing-page

    user string colour = "#34988b"

    after (string body) = landing page ()
        body = replace ("#34988b") in (body) with (background.colour)
