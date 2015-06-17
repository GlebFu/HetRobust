*! version 1.0.1 98.03.23 - plot various screen probabilities

capture program drop hcpltsc
program define hcpltsc
    if "`1'"=="" {
di "Syntax: hcpltsc [structure][Type:B|C|G|W][maxp][b1|b2|b3|b4]"
        exit }
    local outnm `1'
    local type  `2'
    local hc2num ""
    if "`type'"=="B" {
        local lb BP Screen:
    }
    if "`type'"=="G" {
        local lb Glejser Screen:
    }
    if "`type'"=="W" {
        local lb BP Screen:
    }
    if "`type'"=="C" {
        local lb CW Screen:
    }
    local maxp `3'
    local bis `4'
    local top ""
    use `outnm'res2, clear
    global seedis = seedis[1]
    global dsnum = dsnumber[1]
    global dslbl = dslabel[1]
    global R2 = int(1000*R2[1])/1000
    global dt = datenum[1]

    local yv "0,.05,.10"
    local ymax ".10"
    if "`maxp'"==".15" {
        local yv "0,.05,.10,.15"
        local ymax ".15"
    }
    if "`maxp'"==".20" {
        local yv "0,.05,.10,.15,.2"
        local ymax ".20"
    }

    local s 1
    * loop over coefficients s==b1, b2, etc.
    while `s' < 5 {
        label var sOLS05b`s' "OLS test"
        label var s3HC05b`s' "HC3 test"
        label var s3`type'005b`s' " "
        label var s3`type'105b`s' " "
        label var s3`type'205b`s' " "
        label var s3`type'305b`s' " "
        label var s3`type'405b`s' " "
        label var s3`type'505b`s' " "
        label var s3`type'105b`s' "#=Sig Level for Het Screen"
        local s = `s' + 1
    }

    label def sizel 1 "25" 2 "50" 3 "100" 4 "250" 5 "500" 6 "1000"
    label val n123 sizel
    label var sampleN "Sample Size"
    label var n123 " "
    set textsize 150
    gen str1 a1="1"
    gen str1 a2="2"
    gen str1 a3="3"
    gen str1 a4="4"
    gen str1 a5="5"
    gen str1 ah="H"
    gen str1 ab=""
    local hc `hc2num'

    * size of text symbols for 1x1 plot
    local psz 100
    * size of text symbols for 2x2 plot
    if "`bis'"=="" { local psz 180 }
    * size of text for 1x1 plot
    set textsize 90
    * size of text for 2x2 plot
    if "`bis'"=="" { set textsize 100 }

    #delimit ;
    if "`bis'"=="" | "`bis'"=="b1" | "`bis'"=="B1" {;
        graph
            sOLS05b1 s3HC05b1
            s3`type'105b1 s3`type'305b1 s3`type'505b1
            n123, s(OT[a1][a3][a5])
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb1,replace)    ;
        label var sOLS05b2 " ";
        label var s3HC05b2 " ";
        label var s3`type'105b2 " ";
    };

    if "`bis'"=="" {;
        graph
            s3`type'105b2 s3`type'305b2 s3`type'505b2
            s3`type'505b2
            sOLS05b2 s3HC05b2
            n123, s([a1][a3][a5][ab]OT)
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb2,replace)    ;
        label var sOLS05b3 " ";
        label var s3HC05b3 " ";
        label var s3`type'105b3 " ";
    };
    if "`bis'"=="b2" | "`bis'"=="B2" {;
        graph
            sOLS05b2 s3HC05b2
            s3`type'105b2 s3`type'305b2 s3`type'505b2
            n123, s(OT[a1][a3][a5])
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb2,replace)    ;
        label var sOLS05b3 " ";
        label var s3HC05b3 " ";
        label var s3`type'105b3 " ";
    };

    if "`bis'"=="" {;
        graph
            s3`type'105b3 s3`type'305b3 s3`type'505b3
            sOLS05b3 s3HC05b3
            n123, s([a1][a3][a5][ab]OT)
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb3,replace)    ;
        label var sOLS05b4 " ";
        label var s3HC05b4 " ";
        label var s3`type'105b4 " ";
    };
    if "`bis'"=="b3" | "`bis'"=="B3" {;
        graph
            sOLS05b3 s3HC05b3
            s3`type'105b3 s3`type'305b3 s3`type'505b3
            n123, s(OT[a1][a3][a5])
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb3,replace)    ;
        label var sOLS05b4 " ";
        label var s3HC05b4 " ";
        label var s3`type'105b4 " ";
    };

    if "`bis'"=="" {;
        graph
            s3`type'105b4 s3`type'305b4 s3`type'505b4
            sOLS05b4 s3HC05b4
            n123,s([a1][a3][a5][ab]OT)
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb4,replace)    ;
    };
    if "`bis'"=="b4" | "`bis'"=="B4" {;
        graph
            sOLS05b4 s3HC05b4
            s3`type'105b4 s3`type'305b4 s3`type'505b4
            n123, s(OT[a1][a3][a5])
            c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
            ylab(`yv') xlab(1,2,3,4,5,6)
            l1("Percent Rejected") gap(3)
            saving(tb4,replace)    ;
    };

    #delimit cr

    if "`bis'"=="" {
        set textsize 90
        graph using tb1 tb2 tb3 tb4, /*
         */b2(".05 Size `lb' `1'.hc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
    }
    set textsize 95
    if "`bis'"=="b1"|"`bis'"=="B1" {
        graph using tb1 , /*
         */b2(".05 Size `lb' `1'.hc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
    }
    if "`bis'"=="b2"|"`bis'"=="B2" {
        graph using tb2 , /*
         */b2(".05 Size `lb' `1'.hc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
    }
    if "`bis'"=="b3"|"`bis'"=="B3" {
        graph using tb3 , /*
         */b2(".05 Size `lb' `1'.hc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
    }
    if "`bis'"=="b4"|"`bis'"=="B4" {
        graph using tb4 , /*
         */b2(".05 Size `lb' `1'.hc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
    }

end;
