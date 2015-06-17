capture program drop hc34cb
*! version 1.0.0 7/6/99

* Het on x3 & x4, chi-square errors, R2=.4

program define hc34cb
    global factor = .375
    egen etmp = std(invchi(5, (1 - ebase)))
    replace e = $factor*2.0*sqrt(x3)*sqrt(x4 + 2.5)*etmp
    drop etmp
    replace y = xb + e
end

