capture program drop hcbase
*! version 1.0.3 9/11/97 - add note on seed
*! version 1.0.2 9/7/97 - to all IM test
*! version 1.0.1 9/1/97 - based on LE's oldgen.do
*
* syntax: hcbase <#ofcases> <dsname> <seed-optional>
*
* Generate base population structure for montecarlo simulations
* on HC standard errors.

program define hcbase
    version 5.0
    drop _all
    local seedis `3'
    while "`seedis'"=="" { local seedis 29038 }
    set seed `seedis'
    quietly set obs `1'
    di "** Seed = `seedis' for population of `1'."
    quietly {
        gen u1= uniform()
        gen x1 = 1+ u1
        gen x2 = 3*u1 + .6*invnorm(uniform())
        * gen cchi1 = invchi(1, u1)
        * gen cchi2 = [invnorm(u1)]^2  (mn close to 1, larger variance
        gen cchi = [invnorm(uniform())]^2
        gen x3 = 2*u1 + .6*cchi
        gen ebase = uniform()
        gen u2 = uniform()
        * x4 is used to include a variable that has no effect on y
        gen x4 = .1*x1 + .9*x3 + 4*u2 - .8*invnorm(uniform())
        gen e = .
        gen y =.
        gen sortvar =.
        gen xb = 1 + x1 + x2 + x3
        gen x11 = x1*x1
        gen x12 = x1*x2
        gen x13 = x1*x3
        gen x14 = x1*x4
        gen x22 = x2*x2
        gen x23 = x2*x3
        gen x24 = x2*x4
        gen x33 = x3*x3
        gen x34 = x3*x4
        gen x44 = x4*x4
        gen res = .
        gen ct = .
        gen ct2 = .
    }
    label var x1 "Uniform + 1."
    label var x2 "Uniform + normal."
    label var x3 "Uniform + chisq1."
    label var x4 "No effect on y."
    label var ebase "Base for e."
    label var e "Error in equaiton."
    label var y "Observed y."
    label var xb "1 + x1 + x2 + x3."
    label var sortvar "Sort index."
    order y xb x1 x2 x3 x4 e sortvar x11 x12 x13 x14 x22 x23 x24 x33 x34 x44
    drop u1 u2 cchi
    di " "

    note: "Seed is: `seedis'."
    save `2', replace
    describe
    sum xb x1 x2 x3 x4 ebase
    corr xb x1 x2 x3 x4
end
