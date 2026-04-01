# background
*per-user background colour*

## specification

Adds a per-user background colour to the landing page. Uses `after` to post-process the landing page HTML, replacing the default teal with the user's colour.

## definition

    feature background extends landing-page

Each user can have their own background colour, defaulting to teal:

    user string colour = "#34988b"

After the landing page HTML is generated, swap the default teal for this user's colour:

    after (string body) = landing page ()
        body = replace ("#34988b") in (body) with (background.colour)
