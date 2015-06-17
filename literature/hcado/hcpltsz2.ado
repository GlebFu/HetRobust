*! version 1.0.3 3/29/1998 - for new combined res file
*! version 1.0.2 12:30 PM - modify form hcpltsc to be like hcpltpw
*! version 1.0.1 98.03.23 - plot various screen probabilities

capture program drop hcpltsz2
program define hcpltsz2
    if "`1'"=="" {
        #delimit ;
di "=> hcpltsz2 [structure][maxp][Screen test:B|C|G|W|all]
[Screen alpha: 05|10|20|30|40|50|all][b1|b2|b3|b4|all]";
        exit;
        #delimit cr
    }

    local struct `1'
*di "struct `struct'"
    local maxp  `2'
*di "maxp `maxp'"
    local htest  `3'
*di "htest `htest'"
    local salpha  `4'
*di "salpha `salpha'"
    local bis `5'
    if "`bis'"=="" { local bis "all" }

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

* get data and add labels
    use `struct'res, clear
    global seedis = seedis[1]
    global dsnum = dsnumber[1]
    global dslbl = dslabel[1]
    global R2 = int(1000*R2[1])/1000
    global dt = datenum[1]
    label def sizel 1 "25" 2 "50" 3 "100" 4 "250" 5 "500" 6 "1000"
    label val n123 sizel
    label var n123 " "
* add some graph defaults
    * size of text symbols for 1x1 plot
    local psz 120
    * size of text symbols for 2x2 plot
    if "`bis'"=="all" { local psz 180 }
    * size of text for 1x1 plot
    set textsize 100
    * size of text for 2x2 plot
    if "`bis'"=="all" { set textsize 150 }

*****************************************************
* Look at selected screen alphas for given het test

    if "`salpha'"=="all"|"`salpha'"=="ALL"|"`salpha'"=="All" {

        local s 1
        * loop over coefficients s==b1, b2, etc.
        while `s' < 5 {
            label var sOLS05b`s' "OLS test of b`s'"
            label var s3HC05b`s' "HC3 test"
            label var s3`htest'005b`s' " "
            label var s3`htest'105b`s' " "
            label var s3`htest'205b`s' " "
            label var s3`htest'305b`s' " "
            label var s3`htest'405b`s' " "
            label var s3`htest'505b`s' " "
            label var s3`htest'105b`s' "#=.# p-value for screen"
            local s = `s' + 1
        }
        if "`htest'"=="B" {
            local lb Size (p=.05) B-P Screen
        }
        if "`htest'"=="G" {
            local lb Size (p=.05) Glejser Scrn
        }
        if "`htest'"=="W" {
            local lb Size (p=.05) White Scrn
        }
        if "`htest'"=="C" {
            local lb Size (p=.05) C-W Screen
        }

        quietly {
            gen str1 a1="1"
            gen str1 a2="2"
            gen str1 a3="3"
            gen str1 a4="4"
            gen str1 a5="5"
            gen str1 ah="H"
            gen str1 ab=""
        }

        #delimit ;
        if "`bis'"=="all" | "`bis'"=="b1" | "`bis'"=="B1" {;
            graph
                sOLS05b1 s3HC05b1
                s3`htest'105b1 s3`htest'305b1 s3`htest'505b1
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb1,replace)    ;
            label var sOLS05b2 "OLS test of b2";
            label var s3HC05b2 " ";
            label var s3`htest'105b2 " ";
        };

        if "`bis'"=="all" {;
            graph
                sOLS05b2
                s3`htest'105b2 s3`htest'305b2 s3`htest'505b2
                s3`htest'505b2
                s3HC05b2
                n123, s(S[a1][a3][a5][ab]T)
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb2,replace)    ;
            label var s3HC05b3 " ";
            label var s3`htest'105b3 " ";
        };
        if "`bis'"=="b2" | "`bis'"=="B2" {;
            graph
                sOLS05b2 s3HC05b2
                s3`htest'105b2 s3`htest'305b2 s3`htest'505b2
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb2,replace)    ;
            *label var sOLS05b3 " ";
            label var s3HC05b3 " ";
            label var s3`htest'105b3 " ";
        };

        if "`bis'"=="all" {;
            graph
                sOLS05b3
                s3`htest'105b3 s3`htest'305b3 s3`htest'505b3
                s3HC05b3 s3HC05b3
                n123, s(S[a1][a3][a5][ab]T)
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var sOLS05b4 " ";
            label var s3HC05b4 " ";
            label var s3`htest'105b4 " ";
        };
        if "`bis'"=="b3" | "`bis'"=="B3" {;
            graph
                sOLS05b3 s3HC05b3
                s3`htest'105b3 s3`htest'305b3 s3`htest'505b3
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var sOLS05b4 " ";
            label var s3HC05b4 " ";
            label var s3`htest'105b4 " ";
        };

        if "`bis'"=="all" {;
            label var sOLS05b4 "OLS test of b4";
            graph
                sOLS05b4
                s3`htest'105b4 s3`htest'305b4 s3`htest'505b4
                s3HC05b4 s3HC05b4
                n123,s(S[a1][a3][a5][ab]T)
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb4,replace)    ;
        };
        if "`bis'"=="b4" | "`bis'"=="B4" {;
            graph
                sOLS05b4 s3HC05b4
                s3`htest'105b4 s3`htest'305b4 s3`htest'505b4
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb4,replace)    ;
        };
        #delimit cr

        if "`bis'"=="all" {
            set textsize 90
            graph using tb1 tb2 tb3 tb4, /*
             */b2("`lb': `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")

/*b2("`lb' (p=.05): `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")*/
        }
        set textsize 95
        if "`bis'"=="b1"|"`bis'"=="B1" {
            graph using tb1 , /*
             */b2("b1: `lb' (p=.05): `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b2"|"`bis'"=="B2" {
            graph using tb2 , /*
             */b2("b2: `lb' (p=.05): `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b3"|"`bis'"=="B3" {
            graph using tb3 , /*
             */b2("b3: `lb' (p=.05): `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b4"|"`bis'"=="B4" {
            graph using tb4 , /*
             */b2("b4: `lb' (p=.05): `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }

    }

********************************************
* for given screen alpha, look at all tests

    if "`htest'"=="all"|"`htest'"=="ALL"|"`htest'"=="All" {

        local s 1
        * loop over coefficients s==b1, b2, etc.
        while `s' < 5 {
            label var sOLS05b`s' "OLS test of b`s'"
            label var s3HC05b`s' "HC3 test"
            label var s3B`salpha'5b`s' "G=Gleijser; W=White;"
            label var s3C`salpha'5b`s' "B=B-P; C=C-W"
            label var s3G`salpha'5b`s' ""
            label var s3W`salpha'5b`s' ""
            local s = `s' + 1
        }
        quietly {
            gen str1 abp="B"
            gen str1 aw="W"
            gen str1 ac="C"
            gen str1 ag="G"
            gen str1 ah="H"
            gen str1 ab=""
        }
        #delimit ;
        if "`bis'"=="all" | "`bis'"=="b1" | "`bis'"=="B1" {;
            graph
                sOLS05b1 s3HC05b1
                s3B`salpha'5b1 s3C`salpha'5b1
                s3G`salpha'5b1 s3W`salpha'5b1
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb1,replace)    ;
            label var s3HC05b2 " ";
        };

        if "`bis'"=="all" {;
            label var s3HC05b2 " ";
            label var s3B`salpha'5b2 " ";
            label var s3C`salpha'5b2 " ";
            label var s3G`salpha'5b2 " ";
            label var s3W`salpha'5b2 " ";
            graph
                sOLS05b2
                s3B`salpha'5b2 s3C`salpha'5b2
                s3G`salpha'5b2 s3W`salpha'5b2
                 s3HC05b2
                n123, s(S[abp][ac][ag][aw]T)
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb2,replace)    ;
            label var s3HC05b3 " ";
        };
        if "`bis'"=="b2" | "`bis'"=="B2" {;
            graph
                sOLS05b2 s3HC05b2
                s3B`salpha'5b2 s3C`salpha'5b2
                s3G`salpha'5b2 s3W`salpha'5b2
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb2,replace)    ;
            label var sOLS05b3 " ";
            label var s3HC05b3 " ";
        };

        if "`bis'"=="all" {;
            label var s3HC05b2 " ";
            label var s3B`salpha'5b3 " ";
            label var s3C`salpha'5b3 " ";
            label var s3G`salpha'5b3 " ";
            label var s3W`salpha'5b3 " ";

            graph
                sOLS05b3
                s3B`salpha'5b3 s3C`salpha'5b3
                s3G`salpha'5b3 s3W`salpha'5b3
                 s3HC05b3
                n123, s(S[abp][ac][ag][aw]T)
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var s3HC05b4 " ";
        };
        if "`bis'"=="b3" | "`bis'"=="B3" {;
            graph
                sOLS05b3 s3HC05b3
                s3B`salpha'5b3 s3C`salpha'5b3
                s3G`salpha'5b3 s3W`salpha'5b3
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var sOLS05b4 " ";
            label var s3HC05b4 " ";
        };

        if "`bis'"=="all" {;
            label var s3HC05b4 " ";
            label var s3B`salpha'5b4 " ";
            label var s3C`salpha'5b4 " ";
            label var s3G`salpha'5b4 " ";
            label var s3W`salpha'5b4 " ";

            graph
                sOLS05b4
                s3B`salpha'5b4 s3C`salpha'5b4
                s3G`salpha'5b4 s3W`salpha'5b4
                s3HC05b4
                n123,s(S[abp][ac][ag][aw]T)
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb4,replace)    ;
        };
        if "`bis'"=="b4" | "`bis'"=="B4" {;
            graph
                sOLS05b4 s3HC05b4
                s3B`salpha'5b4 s3C`salpha'5b4
                s3G`salpha'5b4 s3W`salpha'5b4
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(.05) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb4,replace)    ;
        };

        #delimit cr
        if "`bis'"=="all" {
            set textsize 90
            graph using tb1 tb2 tb3 tb4, /*
             */b2("Size (p=.05) for .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        set textsize 95
        if "`bis'"=="b1"|"`bis'"=="B1" {
            graph using tb1 , /*
             */b2("Size (p=.05) for b1: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b2"|"`bis'"=="B2" {
            graph using tb2 , /*
             */b2("Size (p=.05) for b2: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b3"|"`bis'"=="B3" {
            graph using tb3 , /*
             */b2("Size (p=.05) for b3: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b4"|"`bis'"=="B4" {
            graph using tb4 , /*
             */b2("Size (p=.05) for b4: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
    }

end;
