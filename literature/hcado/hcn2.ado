capture program drop hcn2
*! version 1.0.1 3/19/98 - rename and factor
*
* hcn2 - normal errors, R2=.2

program define hcn2
    global factor = 1.62
    replace e = $factor*2.5*invnorm(ebase)
    replace y = xb + e
end

