capture program drop hcneq4
*! version 1.0.1 3/28/98
*! version 1.0.0 8/29/97
* Same as:
* hcneq4 - normal errors, R2=.4 with standardized variance for x1,x2,x3,x4.

program define hcneq4
    global factor = 1.25
    replace e = $factor*2.5*invnorm(ebase)
    replace y = xb + e
end

