*! version 1.2.3 3/29/1998 - use new res file
*! version 1.2.2 3/26/1998 - update for new hcplt
*! version 1.2.1 11/12/97 - enter full name of data file and fix rounding
*! version 1.2.0 10/10/97 - multiple screens.
*! version 1.1.1 9/29/97 - fix power labels
*! version 1.1.0 9/26/97 - any power type
*! version 1.0.4 9/18/97 - add note on data name
*! version 1.0.3 9/11/97 - add note on seed

capture program drop hcpltpw
program define hcpltpw
    if "`1'"=="" {
    di "Syntax: hcpltpw [structure][b-balpha: 05|10]/*
    */[Type: HC|2#|B|C|G|W] [salpha-balpha:05|10]"
        exit }
    local struct `1'
*di "struct `struct'"
    local balpha `2'
*di "balpha `balpha'"
    local type  `3'
*di "type `type'"
    local salpha   `4'
*di "salpha `salpha'"

    local hc2num ""
    if "`type'"=="HC" {
        local lb HC Tests:
    }
    if "`type'"=="20" {
        local lb "Screen .`salpha'-HC0:"

        local hc2num 0
    }
    if "`type'"=="21" {
        local lb "Screen .`salpha'-HC1:"

        local hc2num 1
    }
    if "`type'"=="22" {
        local lb "Screen .`salpha'-HC2:"
        local hc2num 2
    }
    if "`type'"=="23" {
        local lb "Screen .`salpha'-HC3:"
        local hc2num 3
    }

    if "`balpha'"=="05" {
        local top ""
    }
    if "`balpha'"=="10" {
        local top ,.25
    }
    if "`salpha'"=="05" {
        local salpha 5
    }
    if "`salpha'"=="10" {
        local salpha 1
    }

* set up variable names
    if "`type'"=="HC" {
        local ty HC`balpha'
     }

    use `struct'res, clear
    global seedis = seedis[1]
    global dsnum = dsnumber[1]
    global dslbl = dslabel[1]
    global R2 = int(1000*R2[1])/1000
    local s 1
    while `s' < 5 {
        label var pOLS05b`s' "OLS test of b`s'"
        label var pOLS10b`s' "OLS test of b`s'"
        local m 0
        while `m' < 4 {
            label var p`m'B105b`s' " "
            label var p`m'B005b`s' " "
            label var p`m'C105b`s' " "
            label var p`m'C005b`s' " "
            label var p`m'G105b`s' " "
            label var p`m'G005b`s' " "
            label var p`m'W105b`s' " "
            label var p`m'W005b`s' " "

            label var p`m'B110b`s' " "
            label var p`m'B010b`s' " "
            label var p`m'C110b`s' " "
            label var p`m'C010b`s' " "
            label var p`m'G110b`s' " "
            label var p`m'G010b`s' " "
            label var p`m'W110b`s' " "
            label var p`m'W010b`s' " "

            label var p`m'HC05b`s' " "
            label var p`m'HC10b`s' " "

            local m = `m' + 1
        }
        local s = `s' + 1
    }

    label var p0B105b1 "Letter = type of Screening"
    label var p0B005b1 "Letter = type of Screening"
    label var p0B110b1 "Letter = type of Screening"
    label var p0B010b1 "Letter = type of Screening"

    label var p1B105b1 "Letter = type of Screening"
    label var p1B005b1 "Letter = type of Screening"
    label var p1B110b1 "Letter = type of Screening"
    label var p1B010b1 "Letter = type of Screening"

    label var p2B105b1 "Letter = type of Screening"
    label var p2B005b1 "Letter = type of Screening"
    label var p2B110b1 "Letter = type of Screening"
    label var p2B010b1 "Letter = type of Screening"

    label var p3B105b1 "Letter = type of Screening"
    label var p3B005b1 "Letter = type of Screening"
    label var p3B110b1 "Letter = type of Screening"
    label var p3B010b1 "Letter = type of Screening"


    label var p0HC05b1 "# = type of HC correction"
    label var p0HC10b1 "# = type of HC correction"

    label def sizel 1 "25" 2 "50" 3 "100" 4 "250" 5 "500" 6 "1000"
    label val n123 sizel
    label var n123 " "
    set textsize 150

    if "`type'"=="20"|"`type'"=="21"|"`type'"=="22"|"`type'"=="23" {

        gen str1 mb="B"
        gen str1 mg="G"
        gen str1 mc="C"
        gen str1 mw="W"
        local hc `hc2num'

    #delimit ;
    graph  p`hc'B`salpha'`balpha'b1 p`hc'C`salpha'`balpha'b1
        p`hc'G`salpha'`balpha'b1 p`hc'W`salpha'`balpha'b1
        pOLS`balpha'b1 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb1,replace)    ;
    graph  p`hc'B`salpha'`balpha'b2 p`hc'C`salpha'`balpha'b2
        p`hc'G`salpha'`balpha'b2 p`hc'W`salpha'`balpha'b2
        pOLS`balpha'b2 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6) gap(4)
        saving(tb2,replace)    ;

    graph  p`hc'B`salpha'`balpha'b3 p`hc'C`salpha'`balpha'b3
        p`hc'G`salpha'`balpha'b3 p`hc'W`salpha'`balpha'b3
        pOLS`balpha'b3 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb3,replace)    ;

    graph  p`hc'B`salpha'`balpha'b4 p`hc'C`salpha'`balpha'b4
        p`hc'G`salpha'`balpha'b4 p`hc'W`salpha'`balpha'b4
        pOLS`balpha'b4 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6) gap(4)
        saving(tb4,replace)    ;
    #delimit cr
    }


    if "`type'"=="HC" {

    #delimit ;
    graph  pOLS`balpha'b1 p0`ty'b1 p1`ty'b1 p2`ty'b1 p3`ty'b1 n123,
        s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb1,replace)    ;

    graph  pOLS`balpha'b2 p0`ty'b2 p1`ty'b2 p2`ty'b2 p3`ty'b2 n123,
        s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6) gap(4)
        saving(tb2,replace)    ;

    graph  pOLS`balpha'b3 p0`ty'b3 p1`ty'b3 p2`ty'b3 p3`ty'b3 n123,
        s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb3,replace)    ;

    graph  pOLS`balpha'b4 p0`ty'b4 p1`ty'b4 p2`ty'b4 p3`ty'b4 n123,
        s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha',1) yscale(0,.15)
        ylab(0,.20,.40,.60,.80,1.0) xlab(1,2,3,4,5,6) gap(4)
        saving(tb4,replace)    ;
    #delimit cr
    }

    set textsize 90
    graph using tb1 tb2 tb3 tb4, /*
*/b2("Unscreened Power (p=`balpha'): `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")


end
