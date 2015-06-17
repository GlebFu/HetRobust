capture program drop hc3n4
*! version 1.0.2 7/6/99 modify formula
*! version 1.0.1 03/31/98 add factor
*! version 1.0.0 08/29/97

* normal errors, more extreme hetero on x3, R2 =.4

program define hc3n4
    global factor = .7
    egen etmp = std(invnorm(ebase))
    replace e = $factor*2.0*sqrt(x3+1.56)*etmp
    drop etmp
    replace y = xb + e
end

