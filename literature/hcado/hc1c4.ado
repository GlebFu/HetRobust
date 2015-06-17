capture program drop hc1c4
*! version 1.0.1 11/14/97 changed name - replaces hch1c4.ado
*! version 1.0.0 08/29/97

* Chi-square errors(df=5), moderate hetero on x1, R2=.4

program define hc1c4
    global factor = .97
    egen etmp = std(invchi(5, (1 - ebase)))
    replace e = $factor*2.15*sqrt(x1)*etmp
    drop etmp
    replace y = xb + e
end

