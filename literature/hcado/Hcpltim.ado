*! version 1.0.0 9/12/97
capture program drop hcpltim
program define hcpltim
    local outnm "`1'res"
    use `outnm', clear

    label def sizel 1 "25" 2 "50" 3 "100" 4 "250" 5 "500" 6 "1000"
    label val n123 sizel
    label var n123 " "
    label var sampleN "Sample Size"


    label var ct1a05 "Heterodasticity: .05"
        label var ct1a10 ".10 level"
    label var ct2a05 "Skewness: .05 level"
    label var ct2a10 ".10 level"
    label var ct3a05 "Kurtosis: .05 level"
    label var ct3a10 ".10 level"
    label var ctta05 "Total: .05 level"
    label var ctta10 ".10 level"

    set textsize 150
    #delimit ;
    graph  ct1a05 ct1a10 n123, s(TO)
        c(ll) psize(180) yline(.05,.10,.5,1) yscale(0,1)
        ylab(0.0,.25,.50,.75,1.0) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb1,replace)    ;
    graph  ct2a05 ct2a10 n123, s(TO)
        c(ll) psize(180) yline(.05,.10,.5,1) yscale(0,1)
        ylab(0.0,.25,.50,.75,1.0) xlab(1,2,3,4,5,6)
        gap(4)
        saving(tb2,replace)    ;
    label var n123 "Sample Size";
    graph  ct3a05 ct3a10 n123, s(TO)
        c(ll) psize(180) yline(.05,.10,.5,1) yscale(0,1)
        ylab(0.0,.25,.50,.75,1.0) xlab(1,2,3,4,5,6)
        l1("Percent Rejected") gap(3)
        saving(tb3,replace)    ;
    graph  ctta05 ctta10 n123, s(TO)
        c(ll) psize(180) yline(.05,.10,.5,1) yscale(0,1)
        ylab(0.0,.25,.50,.75,1.0) xlab(1,2,3,4,5,6)
        gap(4)
        saving(tb4,replace)    ;

        #delimit cr
    global seedis = seedis[1]
    global dsnum = dsnumber[1]
    global dslbl = dslabel[1]
    global R2 = round(R2[1],.001)
    
    set textsize 90
    graph using tb1 tb2 tb3 tb4, /*
    */ b1("IM Test: `lb' `1'.hc$dsnum.$dslbl" - $seedis - R2=$R2)
end


