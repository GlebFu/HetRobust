*! version 1.0.1 3/29/1998 - use new res file
*! version 1.0.0 3/26/1998 - power for various screen types

capture program drop hcpltpw2
program define hcpltpw2
    if "`1'"=="" {
        #delimit ;
di "=> hcpltpw2 [structure][Screen test:B|C|G|W|all]
[Screen alpha: 05|10|20|30|40|50|all][b1|b2|b3|b4|all]";
        exit;
        #delimit cr
    }

    local struct `1'
    local htest  `2'
    local salpha  `3'
    local bis `4'
    if "`bis'"=="" { local bis "all" }

* set range of y axis
    local ylbl "0,.20,.40,.60,.80,1.0"
    local ymax "1.0"
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
            label var pOLS05b`s' "OLS test of b`s'"
            label var p3HC05b`s' "HC3 test"
            label var p3`htest'005b`s' " "
            label var p3`htest'105b`s' " "
            label var p3`htest'205b`s' " "
            label var p3`htest'305b`s' " "
            label var p3`htest'405b`s' " "
            label var p3`htest'505b`s' " "
            label var p3`htest'105b`s' "#=.# p-value for screen"
            local s = `s' + 1
        }
        if "`htest'"=="B" {
            local lb B-P Screen
        }
        if "`htest'"=="G" {
            local lb Glejser
        }
        if "`htest'"=="W" {
            local lb White Screen
        }
        if "`htest'"=="C" {
            local lb C-W Screen
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
                pOLS05b1 p3HC05b1
                p3`htest'105b1 p3`htest'305b1 p3`htest'505b1
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb1,replace)    ;
            label var pOLS05b2 "OLS test of b2";
            label var p3HC05b2 " ";
            label var p3`htest'105b2 " ";
        };

        if "`bis'"=="all" {;
            graph
                pOLS05b2
                p3`htest'105b2 p3`htest'305b2 p3`htest'505b2
                p3`htest'505b2
                p3HC05b2
                n123, s(S[a1][a3][a5][ab]T)
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb2,replace)    ;
            label var p3HC05b3 " ";
            label var p3`htest'105b3 " ";
        };
        if "`bis'"=="b2" | "`bis'"=="B2" {;
            graph
                pOLS05b2 p3HC05b2
                p3`htest'105b2 p3`htest'305b2 p3`htest'505b2
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb2,replace)    ;
            *label var pOLS05b3 " ";
            label var p3HC05b3 " ";
            label var p3`htest'105b3 " ";
        };

        if "`bis'"=="all" {;
            graph
                pOLS05b3
                p3`htest'105b3 p3`htest'305b3 p3`htest'505b3
                p3HC05b3 p3HC05b3
                n123, s(S[a1][a3][a5][ab]T)
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var pOLS05b4 " ";
            label var p3HC05b4 " ";
            label var p3`htest'105b4 " ";
        };
        if "`bis'"=="b3" | "`bis'"=="B3" {;
            graph
                pOLS05b3 p3HC05b3
                p3`htest'105b3 p3`htest'305b3 p3`htest'505b3
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var pOLS05b4 " ";
            label var p3HC05b4 " ";
            label var p3`htest'105b4 " ";
        };

        if "`bis'"=="all" {;
            label var pOLS05b4 "OLS test of b4";
            graph
                pOLS05b4
                p3`htest'105b4 p3`htest'305b4 p3`htest'505b4
                p3HC05b4 p3HC05b4
                n123,s(S[a1][a3][a5][ab]T)
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb4,replace)    ;
        };
        if "`bis'"=="b4" | "`bis'"=="B4" {;
            graph
                pOLS05b4 p3HC05b4
                p3`htest'105b4 p3`htest'305b4 p3`htest'505b4
                n123, s(ST[a1][a3][a5])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb4,replace)    ;
        };
        #delimit cr

        if "`bis'"=="all" {
            set textsize 90
            graph using tb1 tb2 tb3 tb4, /*
             */b2("Power (p=.05) `lb': `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        set textsize 95
        if "`bis'"=="b1"|"`bis'"=="B1" {
            graph using tb1 , /*
             */b2("b1: Power (p=.05) `lb': `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b2"|"`bis'"=="B2" {
            graph using tb2 , /*
             */b2("b2: Power (p=.05) `lb': `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b3"|"`bis'"=="B3" {
            graph using tb3 , /*
             */b2("b3: Power (p=.05) `lb': `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b4"|"`bis'"=="B4" {
            graph using tb4 , /*
             */b2("b4: Power (p=.05) `lb': `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }

    }

********************************************
* for given screen alpha, look at all tests

    if "`htest'"=="all"|"`htest'"=="ALL"|"`htest'"=="All" {

        local s 1
        * loop over coefficients s==b1, b2, etc.
        while `s' < 5 {
            label var pOLS05b`s' "OLS test of b`s'"
            label var p3HC05b`s' "HC3 test"
            label var p3B`salpha'5b`s' "B=B-P; C=Cook-W"
            label var p3C`salpha'5b`s' "G=Gleijser; W=White"
            label var p3G`salpha'5b`s' ""
            label var p3W`salpha'5b`s' ""
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
                pOLS05b1 p3HC05b1
                p3B`salpha'5b1 p3C`salpha'5b1
                p3G`salpha'5b1 p3W`salpha'5b1
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb1,replace)    ;
            label var p3HC05b2 " ";
        };

        if "`bis'"=="all" {;
            label var p3HC05b2 " ";
            label var p3B`salpha'5b2 " ";
            label var p3C`salpha'5b2 " ";
            label var p3G`salpha'5b2 " ";
            label var p3W`salpha'5b2 " ";
            graph
                pOLS05b2
                p3B`salpha'5b2 p3C`salpha'5b2
                p3G`salpha'5b2 p3W`salpha'5b2
                 p3HC05b2
                n123, s(S[abp][ac][ag][aw]T)
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb2,replace)    ;
            label var p3HC05b3 " ";
        };
        if "`bis'"=="b2" | "`bis'"=="B2" {;
            graph
                pOLS05b2 p3HC05b2
                p3B`salpha'5b2 p3C`salpha'5b2
                p3G`salpha'5b2 p3W`salpha'5b2
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb2,replace)    ;
            label var pOLS05b3 " ";
            label var p3HC05b3 " ";
        };

        if "`bis'"=="all" {;
            label var p3HC05b2 " ";
            label var p3B`salpha'5b3 " ";
            label var p3C`salpha'5b3 " ";
            label var p3G`salpha'5b3 " ";
            label var p3W`salpha'5b3 " ";

            graph
                pOLS05b3
                p3B`salpha'5b3 p3C`salpha'5b3
                p3G`salpha'5b3 p3W`salpha'5b3
                 p3HC05b3
                n123, s(S[abp][ac][ag][aw]T)
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var p3HC05b4 " ";
        };
        if "`bis'"=="b3" | "`bis'"=="B3" {;
            graph
                pOLS05b3 p3HC05b3
                p3B`salpha'5b3 p3C`salpha'5b3
                p3G`salpha'5b3 p3W`salpha'5b3
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb3,replace)    ;
            label var pOLS05b4 " ";
            label var p3HC05b4 " ";
        };

        if "`bis'"=="all" {;
            label var p3HC05b4 " ";
            label var p3B`salpha'5b4 " ";
            label var p3C`salpha'5b4 " ";
            label var p3G`salpha'5b4 " ";
            label var p3W`salpha'5b4 " ";

            graph
                pOLS05b4
                p3B`salpha'5b4 p3C`salpha'5b4
                p3G`salpha'5b4 p3W`salpha'5b4
                p3HC05b4
                n123,s(S[abp][ac][ag][aw]T)
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("") gap(3)
                saving(tb4,replace)    ;
        };
        if "`bis'"=="b4" | "`bis'"=="B4" {;
            graph
                pOLS05b4 p3HC05b4
                p3B`salpha'5b4 p3C`salpha'5b4
                p3G`salpha'5b4 p3W`salpha'5b4
                n123, s(ST[abp][ac][ag][aw])
                c(llllllll) psize(`psz') yline(1.0) yscale(0,`ymax')
                ylab(`ylbl') xlab(1,2,3,4,5,6)
                l1("Percent Rejected") gap(3)
                saving(tb4,replace)    ;
        };

        #delimit cr
        if "`bis'"=="all" {
            set textsize 90
            graph using tb1 tb2 tb3 tb4, /*
             */b2("Power (p=.05) for .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        set textsize 95
        if "`bis'"=="b1"|"`bis'"=="B1" {
            graph using tb1 , /*
             */b2("Power (p=.05) for b1: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b2"|"`bis'"=="B2" {
            graph using tb2 , /*
             */b2("Power (p=.05) for b2: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b3"|"`bis'"=="B3" {
            graph using tb3 , /*
             */b2("Power (p=.05) for b3: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
        if "`bis'"=="b4"|"`bis'"=="B4" {
            graph using tb4 , /*
             */b2("Power (p=.05) for b4: .`salpha' Screen: `1'.jslhc$dsnum.$dslbl.$dt.$seedis: R2=$R2")
        }
    }

end;
