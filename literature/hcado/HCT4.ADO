capture program drop hct4
*! version 1.0.1 3/19/98 - rename and factor
*! version 1.0.0 8/29/97
*
* hct4 - t 5df errors, R2=.4

program define hct4
    global factor = 1.00
    gen cfix = 1
    gen utmp = uniform()
    replace cfix = -1 if utmp > .5
    replace e =  cfix*(invt(5, (1 - ebase)))
    egen etmp = std(e)
    replace e = $factor*2.55*etmp
    drop cfix etmp utmp
    replace y = xb + e
end

