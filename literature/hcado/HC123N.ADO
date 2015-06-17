capture program drop hc123n
*! version 1.0.2 03/19/98 rename and factor
*! version 1.0.1 11/22/97 added constant to x2 to remove neg values
*! version 1.0.0 11/15/97 developed from hc123c.ado, changed errors to normal

* Structure - het on x1,x2 & x3, normal errors, R2=.4

program define hc123n
    global factor = .37
    egen etmp = std(invnorm(ebase))
    replace e = $factor*2.0*sqrt(x1)*sqrt(x2 + 2.5)*sqrt(x3)*etmp
    sum e etmp ebase
    drop etmp
    replace y = xb + e
end

