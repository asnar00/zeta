# hello
*timed countdown*

## definition

    use terminal.out$

    on (int i$) <- count down from (int n)
        i$ <- n <- (i$ - 1) while (i$ > 0)

    on main (string args$)
        out$ <- count down from (10) at ((1) hz)
