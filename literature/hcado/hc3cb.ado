capture program drop hc3cb
*! version 1.0.0 08/29/97

* Chi-square errors(df=5), extreme hetero on x3, R2=4

program define hc3cb
    global factor = .7
    egen etmp = std(invchi(5, (1 - ebase)))
    replace e = $factor*2.0*sqrt(x3+1.56)*etmp
    drop etmp
    replace y = xb + e
end


