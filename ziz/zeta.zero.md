# zeta
*zero translator*

## specification

Translates zero programs to any target language. This is the base feature of the self-hosted translator.

## interface

The `main` task takes command-line arguments and produces output lines:

    main(["input.zero.md", "output.py"]) => "ᕦ(ツ)ᕤ"

## definition

    feature zeta

The zeta logo:

    string logo = "ᕦ(ツ)ᕤ"

The main entry point takes command-line arguments and produces output:

    on (string out$) <- main (string args$)
        out$ <- logo
