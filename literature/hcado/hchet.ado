*! version 1.0.1 98.03.26 - Rename to hchet from hcresid
*! version 1.0.0 98.03.24 - Summary of spread of residuals
* hcresid <dsnumber> <structure ado name>
*
* Computes ratio of standard deviations of residuals in
* 5-15 percentile and 85-95 percentile.
*
*   dsnumber - jslhc# used for data
*   name of ado file used to generate : n4, t4, etc.

capture program drop hchet
version 5.0

program define hchet
    if "`1'"=="" {
        di "Syntax: hchet <dsnumber: 1 for jslhc1> <structurename>"
        exit }
    local dtanum `1'
    global dtanum `dtanum'
    macro shift
    local outnm `1'
    macro shift
  * call program to construct the data structure
    di " "
    di "** Structure `outnm'.jslHC`dtanum': Constructing population data."
    use jslhc`dtanum',clear
  * create error structure
    quietly hc`outnm'
    label var x1 "  x1"
    label var x2 "  x2"
    label var x3 "  x3"
    label var x4 "  x4"
    di "** Structure `outnm'.jslHC`dtanum': Base regression."
    fit y x1 x2 x3 x4
    quietly {
        fpredict ehat, res
        *di "x1: 5-15 percentile residuals"
        _pctile x1, p(5,15)
        global lo = _result(1)
        global hi = _result(2)
        sum x1 ehat if x1>$lo & x1<$hi
        global sdlo1 = sqrt(_result(4))
        *di "x1: 85-95 percentile residuals"
        _pctile x1, p(85,95)
        global lo = _result(1)
        global hi = _result(2)
        sum x1 ehat if x1>$lo & x1<$hi
        global sdhi1 = sqrt(_result(4))
        *di "x2: 5-15 percentile residuals"
        _pctile x2, p(5,15)
        global lo = _result(1)
        global hi = _result(2)
        sum x2 ehat if x2>$lo & x2<$hi
        global sdlo2 = sqrt(_result(4))
        *di "x2: 85-95 percentile residuals"
        _pctile x2, p(85,95)
        global lo = _result(1)
        global hi = _result(2)
        sum x2 ehat if x2>$lo & x2<$hi
        global sdhi2 = sqrt(_result(4))
        *di "x3: 5-15 percentile residuals"
        _pctile x3, p(5,15)
        global lo = _result(1)
        global hi = _result(2)
        sum x3 ehat if x3>$lo & x3<$hi
        global sdlo3 = sqrt(_result(4))
        *di "x3: 85-95 percentile residuals"
        _pctile x3, p(85,95)
        global lo = _result(1)
        global hi = _result(2)
        sum x3 ehat if x3>$lo & x3<$hi
        global sdhi3 = sqrt(_result(4))
        *di "x4: 5-15 percentile residuals"
        _pctile x4, p(5,15)
        global lo = _result(1)
        global hi = _result(2)
        sum x4 ehat if x4>$lo & x4<$hi
        global sdlo4 = sqrt(_result(4))
        *di "x4: 85-95 percentile residuals"
        _pctile x4, p(85,95)
        global lo = _result(1)
        global hi = _result(2)
        sum x4 ehat if x4>$lo & x4<$hi
        global sdhi4 = sqrt(_result(4))
    }
    di " "
    tempvar r
di "** Structure `outnm'.jslHC`dtanum': SD ratio 85-95/5-15 of residuals."
    di " "
    scalar `r' = round($sdhi1/$sdlo1,.01)
    di "  X1: "`r'
    scalar `r' = round($sdhi2/$sdlo2,.01)
    di "  X2: "`r'
    scalar `r' = round($sdhi3/$sdlo3,.01)
    di "  X3: "`r'
    scalar `r' = round($sdhi4/$sdlo4,.01)
    di "  X4: "`r'

end
