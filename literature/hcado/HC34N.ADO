capture program drop hc34n
*! version 1.0.1 11/22/97 added constant to x4 to remove neg values
*! version 1.0.0 11/15/97

* Het on x3 & x4, normal errors, R2=.4

program define hc34n
    global factor = .375
    egen etmp = std(invnorm(ebase))
    replace e = $factor*2.0*sqrt(x3)*sqrt(x4 + 2.5)*etmp
    drop etmp
    replace y = xb + e
end

