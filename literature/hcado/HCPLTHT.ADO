*! version 1.0.4 3/29/1998 - use new res file
*! version 1.0.3 3/26/1998 - use res2 and use more levels
*! version 1.0.2 11/12/97 - fix rounding problem
*! version 1.0.1 11/12/97 - enter full name of data file.
*! version 1.0.0 10/09/97
* plot results of hetero tests
capture program drop hcpltht
program define hcpltht
    if "`1'"=="" {
        di hcpltht <structure>
        exit
    }

    local outnm `1'
    use `outnm'res, clear

    label def sizel 1 "25" 2 "50" 3 "100" 4 "250" 5 "500" 6 "1000"
    label val n123 sizel
    label var n123 " "
    label var sampleN "Sample Size"
label var sampleN ""
    label var wha05 "White: .05"
    label var wha10 "White: .10"
    label var wha30 "White: .30"
    label var wha40 "White: .40"
    label var gla05 "Glejser: .05"
    label var gla10 "Glejser: .10"
    label var gla30 "Glejser: .30"
    label var gla40 "Glejser: .40"
    label var bpa05 "Breusch-Pagan: .05"
    label var bpa10 "Breusch-Pagan: .10"
    label var bpa30 "Breusch-Pagan: .30"
    label var bpa40 "Breusch-Pagan: .40"
    label var cwa05 "Cook-Weisberg: .05"
    label var cwa10 "Cook-Weisberg: .10"
    label var cwa30 "Cook-Weisberg: .30"
    label var cwa40 "Cook-Weisberg: .40"

    set textsize 150
    #delimit ;
    graph
        wha05 gla05 bpa05 cwa05
        n123, s(OSTd)
        c(llll) psize(180) yline(.05,.5,1)
        yscale(0,.15)
        ylab(0.0,0.05,0.25,.5,.75,1.) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb1,replace)    ;

    graph
        wha10 gla10 bpa10 cwa10
        n123, s(OSTd)
        c(llll) psize(180) yline(.10,.5,1)
        yscale(0,.15)
        ylab(0.0,0.10,0.25,.5,.75,1.) xlab(1,2,3,4,5,6)
        l1("") gap(3)
        saving(tb2,replace)    ;

    graph
        wha30 gla30 bpa30 cwa30
        n123, s(OSTd)
        c(llll) psize(180) yline(.30,.5,1)
        yscale(0,.15)
        ylab(0.0,0.25,.3,.5,.75,1.) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb3,replace)    ;

    graph
        wha40 gla40 bpa40 cwa40
        n123, s(OSTd)
        c(llll) psize(180) yline(.40,.5,1)
        yscale(0,.15)
        ylab(0.0,0.25,.4,.5,.75,1.) xlab(1,2,3,4,5,6)
        l1("") gap(3)
        saving(tb4,replace)    ;

    graph using, saving(null,replace);

    #delimit cr

    global R2 = int(1000*R2[1])/1000
    global seedis = seedis[1]
    global dsnum = dsnumber[1]
    global dslbl = dslabel[1]
    global dt = datenum[1]
    set textsize 90
    graph using tb1 tb2 tb3 tb4, /*
    */b2("Heteroscedasticity Tests: `lb' `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")

end


