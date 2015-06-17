capture program drop hc2dbg
*! version 1.0.1 11/15/97 changed name, replaces hcb2v.ado
*! version 1.0.0 08/29/97

*  BIG difference in error variances on dummy var, R2=.4

program define hc2dbg
    global factor = .665
    replace e = 1
    summarize e x2 x1 x3 x4
    replace e = $factor*4*invnorm(ebase) if x2 == 1
    replace e = $factor*1*invnorm(ebase) if x2 == 0
    summarize e
    summarize e if x2 == 1
    summarize e if x2 == 0
    replace y = xb + e
end

