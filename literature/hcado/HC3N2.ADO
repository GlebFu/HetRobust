capture program drop hc3n2
*! version 1.0.0 03/31/98 add factor
*! version 1.0.0 08/29/97

* normal errors, more extreme hetero on x3, R2 =.2

program define hc3n2
    global factor = 1.52
    egen etmp = std(sqrt(x3)*invnorm(ebase))
    replace e = $factor*2.6*etmp
    drop etmp
    replace y = xb + e
end

