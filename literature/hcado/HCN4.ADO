capture program drop hcn4
*! version 1.0.1 3/19/98 - rename and factor
*! version 1.0.0 8/29/97
*
* hcn4 - Structure 1: normal errors, R2=.4

program define hcn4
    global factor = 1.00
    replace e = $factor*2.5*invnorm(ebase)
    replace y = xb + e
end

