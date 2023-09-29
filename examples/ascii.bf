this program prints out all the printable ascii characters (each separated by a space)

we want to step through all of ascii from 32 to 128 so write 127 into the first cell and leave pointer on cell 2
>++++++++[-<++++++++++++++++>]<->

and then write 32 into the second cell and leave pointer on cell 2
>++++[-<++++++++>]<-

<[

    write our initial sentinel value back to cell 1 but first zero cell 1 as it's also our condition cell (uses cell 3 as temporary)
    [-]>>>++++++++[-<<<++++++++++++++++>>>]<<<->>><<<

    >+. print the current ascii value at cell 2
    >>>>++++++++++++++++++++++++++++++++.[-]<<<< print some whitespace between each character

    condition check writes 1 into cell 1 if cell 1 != 2 else writes 0

    >[-] cell 3
    >[-] cell 4
    <<<[>>>+<<<-]
    >[>>-<+<-]
    >[<+>-]
    >[<<<+>>>[-]]

    <<< move back to cell 1

]
