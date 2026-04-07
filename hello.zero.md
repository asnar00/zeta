# hello
*timed countdown*

## definition

    on (int i$) <- count down from (int n)
        i$ <- n <- (i$ - 1) while (i$ > 0)

    on main (string args$)
        int i$ <- count down from (10) at ((1) hz)
        for each (i) in (i$)
            print (i)
