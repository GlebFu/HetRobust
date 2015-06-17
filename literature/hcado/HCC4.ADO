capture program drop hcc4
*! version 1.0.1 3/19/98 - renamed, add factor
*! version 1.0.0 8/29/97
*
* hcc4 - Chi-squared errors(df=5), R^2=.4

program define hcc4
    global factor = 1.00
  * chi-sq 5-df error
    egen e2 = std(invchi(5, (1 - ebase)))
  * rescale to get the R2=.4
    replace e = $factor*2.6*e2
    drop e2
    replace y = xb + e
end

