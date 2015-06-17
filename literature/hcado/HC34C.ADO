capture program drop hc34c
*! version 1.0.2 11/22/97 added constant to x4 to remove neg values
*! version 1.0.1 11/15/97 changed name - replaces hcsth4.ado
*! version 1.0.0 10/02/97

* Het on x3 & x4, chi-square errors, R2=.4

program define hc34c
    global factor = .375
    egen etmp = std(invchi(5, (1 - ebase)))
    replace e = $factor*2.0*sqrt(x3)*sqrt(x4 + 2.5)*etmp
    drop etmp
    replace y = xb + e
end

