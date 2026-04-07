# hello
*concurrent timed streams*

## definition

    use terminal.out$

    on (int i$) <- count down from (int n)
        i$ <- n <- (i$ - 1) while (i$ > 0)

    on (int i$) <- count up to (int n)
        i$ <- 1 <- (i$ + 1) while (i$ <= n)

    on main (string args$)
        concurrently
            out$ <- count down from (10) at ((1) hz)
        and
            out$ <- count up to (20) at ((5) hz)
