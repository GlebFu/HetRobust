capture program drop hccb
*! version 1.0.0 7/6/99
*
* hcc4 - Chi-squared errors(df=5), R^2=.4

program define hccb
    global factor = 1.00
  * chi-sq 5-df error
    egen e2 = std(invchi(5, (1 - ebase)))
  * rescale to get the R2=.4
    replace e = $factor*2.6*e2
    drop e2
    replace y = xb + e
end

