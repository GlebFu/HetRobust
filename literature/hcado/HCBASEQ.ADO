capture program drop hcbaseq

*! version 1.0.3 9/11/97 - add note on seed
*! version 1.0.2 9/7/97 - to all IM test
*! version 1.0.1 9/1/97 - based on LE's oldgen.do
*
* syntax: hcbaseq  <dsname>
*
* Generate base population structure with equal variances

program define hcbaseq
    version 5.0
    use jslhc1
quietly {
    egen x1sd = std(x1)
    egen x2sd = std(x2)
    egen x3sd = std(x3)
    egen x4sd = std(x4)

    drop x1 x2 x3 x4
    rename x1sd x1
    rename x2sd x2
    rename x3sd x3
    rename x4sd x4

    replace xb = 1 + x1 + x2 + x3
    replace x11 = x1*x1
    replace x12 = x2*x1
    replace x13 = x3*x1
    replace x14 = x4*x1
    replace x22 = x2*x2
    replace x23 = x3*x2
    replace x24 = x4*x2
    replace x33 = x3*x3
    replace x34 = x4*x3
    replace x44 = x4*x4

    order y xb x1 x2 x3 x4 e sortvar x11 x12 x13 x14 x22 x23 x24 x33 x34 x44
}
    di " "
    save `1', replace
    describe
    sum xb x1 x2 x3 x4 ebase
    corr xb x1 x2 x3 x4
end
