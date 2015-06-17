capture program drop hc3nz
*! version 1.0.0 04/17/98
* revised to match the method used for the c errors

* normal errors, more extreme hetero on x3, R2 =.8

program define hc3nz
    global factor = .23
    egen etmp = std(invnorm(ebase))
    replace e = $factor*2.6*sqrt(x3+1.56)*etmp
    drop etmp
    replace y = xb + e
end

