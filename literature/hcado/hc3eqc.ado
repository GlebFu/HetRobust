capture program drop hc3eqc
*! version 1.0.2 11/23/97 add constant to x3 to remove sqrt(<0)
*! version 1.0.1 11/14/97 changed name replaces hch3c4.ado
*! version 1.0.0 8/29/97

* Chi-square errors(df=5), extreme hetero on x3, equal variances of x's
* R2=.4

program define hc3eqc
    global factor = 1.25
    egen etmp = std(invchi(5, (1 - ebase)))
    replace e = $factor*2.0*sqrt(x3+1.56)*etmp
    drop etmp
    replace y = xb + e
end


