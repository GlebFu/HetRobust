capture program drop hc2dsm
*! version 1.0.1 11/15/97 changed name, replaces hc2v.ado
*! version 1.0.0 08/29/97

* Small difference in error variances on dummy var X2, R2=.4

program define hc2dsm
    global factor = .75
    replace e = $factor*3*invnorm(ebase) if x2 == 1
    replace e = $factor*2*invnorm(ebase) if x2 == 0
    summarize e
    summarize e if x2 == 1
    summarize e if x2 == 0
    replace y = xb + e
end

