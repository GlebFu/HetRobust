capture program drop hc1n4
*! version 1.0.1 11/14/97 changed name - replaces hch1n4.ado
*! version 1.0.0 08/29/97

* normal errors, moderate hetero on x1, R2=.4

program define hc1n4
    global factor = .97
    egen etmp = std(sqrt(x1)*invnorm(ebase))
    replace e = $factor*2.6*etmp
    drop etmp
    replace y = xb + e
end

