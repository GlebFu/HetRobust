capture program drop hcn7
*! version 1.0.1 3/19/98 - rename and factor
*! version 1.0.0
*
* hcn7 - normal errors, R2=.7

program define hcn7
    global factor = .53
    replace e = $factor*2.5*invnorm(ebase)
    replace y = xb + e
end

