*! version 1.2.2 98.03.26 - add axis limits and clean up

capture program drop hcpltsz
program define hcpltsz
    if "`1'"=="" {
        di "Syntax: hcpltsz [structure][Alpha: 05|10][maxp]/*
        */[Type: HC|2#|B|C|G|W][ScrnAlpha:05|10]"
        exit }
    local struct `1'
*di "struct `struct'"
    local balpha `2'
*di "balpha `balpha'"
    local maxp  `3'
*di "maxp `maxp'"
    local type  `4'
*di "type `type'"
    local salpha  `5'
*di "salpha `salpha'"

* set range of y axis
    local ylbl "0,.05,.10"
    local ymax ".10"
    if "`maxp'"==".15" {
        local ylbl "0,.05,.10,.15"
        local ymax ".15"
    }
    if "`maxp'"==".20" {
        local ylbl "0,.05,.10,.15,.20"
        local ymax ".20"
    }
    if "`maxp'"==".25" {
        local ylbl "0,.05,.10,.15,.20,.25"
        local ymax ".25"
    }

    local hc2num ""
    if "`type'"=="HC" {
        local lb Unscreened Size (p=.`balpha'):
    }
    if "`type'"=="20" {
        local lb ".`salpha' Screen-HC0:"
        local hc2num 0
    }
    if "`type'"=="21" {
        local lb ".`salpha' Screen-HC1:"
        local hc2num 1
    }
    if "`type'"=="22" {
        local lb ".`salpha' Screen-HC2:"
        local hc2num 2
    }
    if "`type'"=="23" {
        local lb ".`salpha' Screen-HC3:"
        local hc2num 3
    }

* rename screen level for naming of variables below
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
    * s==b1, b2, etc.
    while `s' < 5 {
        label var sOLS05b`s' "OLS test of b`s'"
        label var sOLS10b`s' "OLS test of b`s'"
        local m 0
        * m==HC method
        while `m' < 4 {
            label var s`m'B105b`s' " "
            label var s`m'B005b`s' " "
            label var s`m'C105b`s' " "
            label var s`m'C005b`s' " "
            label var s`m'G105b`s' " "
            label var s`m'G005b`s' " "
            label var s`m'W105b`s' " "
            label var s`m'W005b`s' " "

            label var s`m'B110b`s' " "
            label var s`m'B010b`s' " "
            label var s`m'C110b`s' " "
            label var s`m'C010b`s' " "
            label var s`m'G110b`s' " "
            label var s`m'G010b`s' " "
            label var s`m'W110b`s' " "
            label var s`m'W010b`s' " "

            label var s`m'HC05b`s' " "
            label var s`m'HC10b`s' " "

            local m = `m' + 1
        }
        local s = `s' + 1
    }

    label var s0B105b1 "Letter = type of Screening"
    label var s0B005b1 "Letter = type of Screening"
    label var s0B110b1 "Letter = type of Screening"
    label var s0B010b1 "Letter = type of Screening"

    label var s1B105b1 "Letter = type of Screening"
    label var s1B005b1 "Letter = type of Screening"
    label var s1B110b1 "Letter = type of Screening"
    label var s1B010b1 "Letter = type of Screening"

    label var s2B105b1 "Letter = type of Screening"
    label var s2B005b1 "Letter = type of Screening"
    label var s2B110b1 "Letter = type of Screening"
    label var s2B010b1 "Letter = type of Screening"

    label var s3B105b1 "Letter = type of Screening"
    label var s3B005b1 "Letter = type of Screening"
    label var s3B110b1 "Letter = type of Screening"
    label var s3B010b1 "Letter = type of Screening"

    label var s0HC05b1 "# = type of HC correction"
    label var s0HC10b1 "# = type of HC correction"

    label def sizel 1 "25" 2 "50" 3 "100" 4 "250" 5 "500" 6 "1000"
    label val n123 sizel
    label var sampleN "Sample Size"
    label var sampleN " "
    label var n123 " "
    set textsize 150

if "`type'"=="20"|"`type'"=="21"|"`type'"=="22"|"`type'"=="23" {

        gen str1 mb="B"
        gen str1 mg="G"
        gen str1 mc="C"
        gen str1 mw="W"

    local hc `hc2num'

    #delimit ;
    graph  s`hc'B`salpha'`balpha'b1 s`hc'C`salpha'`balpha'b1
        s`hc'G`salpha'`balpha'b1 s`hc'W`salpha'`balpha'b1
        sOLS`balpha'b1 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb1,replace)    ;

    graph  s`hc'B`salpha'`balpha'b2 s`hc'C`salpha'`balpha'b2
        s`hc'G`salpha'`balpha'b2 s`hc'W`salpha'`balpha'b2
        sOLS`balpha'b2 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb2,replace)    ;

    label var n123 "Sample Size";
    label var n123 " ";
    graph  s`hc'B`salpha'`balpha'b3 s`hc'C`salpha'`balpha'b3
        s`hc'G`salpha'`balpha'b3 s`hc'W`salpha'`balpha'b3
        sOLS`balpha'b3 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb3,replace)    ;

    graph  s`hc'B`salpha'`balpha'b4 s`hc'C`salpha'`balpha'b4
        s`hc'G`salpha'`balpha'b4 s`hc'W`salpha'`balpha'b4
        sOLS`balpha'b4 n123, s([mb][mc][mg][mw]S)
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb4,replace)    ;

    #delimit cr

}

if "`type'" == "HC" {
    #delimit ;
    graph  sOLS`balpha'b1 s0`ty'b1 s1`ty'b1 s2`ty'b1 s3`ty'b1 n123,
        s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb1,replace)    ;

    graph  sOLS`balpha'b2 s0`ty'b2 s1`ty'b2 s2`ty'b2 s3`ty'b2 n123, s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6) gap(4)
        saving(tb2,replace)    ;

    graph  sOLS`balpha'b3 s0`ty'b3 s1`ty'b3 s2`ty'b3 s3`ty'b3 n123, s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb3,replace)    ;

    graph  sOLS`balpha'b4 s0`ty'b4 s1`ty'b4 s2`ty'b4 s3`ty'b4 n123, s(S[m0][m1][m2][m3])
        c(lllll) psize(180) yline(.`balpha') yscale(0,`ymax')
        ylab(`ylbl') xlab(1,2,3,4,5,6) gap(4)
        saving(tb4,replace)    ;
    #delimit cr
    }

    global dt = datenum[1]
    set textsize 90

    graph using tb1 tb2 tb3 tb4, /*
     */b2("`lb' `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")

end
