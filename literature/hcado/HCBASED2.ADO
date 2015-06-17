capture program drop hcbased2

*! 11/6/97 - correct xb
*! version 1.0.3 9/11/97 - add note on seed
*! version 1.0.2 9/7/97 - to all IM test
*! version 1.0.1 9/1/97 - based on LE's oldgen.do
*
* syntax: hcbased2  <dsname>
*
* Generate base population structure - X2 dummy -for montecarlo
* simulations on HC standard errors.

program define hcbased2
    version 5.0
    use jslhc1

    gen x2d = 1 if x2 > 1.6
    replace x2d = 0 if x2 <= 1.6
    drop x2
    rename x2d x2

    label var x2 "Dummy."
    replace x12 = x1*x2
    replace x22 = x2*x2
    replace x23 = x3*x2
    replace x24 = x4*x2

    replace xb = 1 + x1 + x2 + x3

    order y xb x1 x2 x3 x4 e sortvar x11 x12 x13 x14 x22 x23 x24 x33 x34 x44
    di " "

    save `1', replace
    describe
    sum xb x1 x2 x3 x4 ebase
    corr xb x1 x2 x3 x4
end


