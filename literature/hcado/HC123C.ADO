capture program drop hc123c
*! version 1.0.3 98.03.19 - rename and factor
*! version 1.0.2 11/22/97 added constant to x2 to remove neg values
*! version 1.0.1 11/15/97 changed name, replaces hcsth3.ado
*! version 1.0.0 10/02/97

* Structure - het on x1,x2 & x3, chi-square errors, R2=.4

program define hc123c
    global factor = .37
    egen etmp = std(invchi(5, (1 - ebase)))
    replace e = $factor*2.0*sqrt(x1)*sqrt(x2+2.5)*sqrt(x3)*etmp
    sum e etmp ebase
    drop etmp
    replace y = xb + e
end

