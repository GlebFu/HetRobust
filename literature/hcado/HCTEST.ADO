* do White Het test
capture program drop hctest
*! 1.1.1 - 07 oc 97 - add BP, GL, etc.
*! 1.1.0 - 07 oc 97 - add BP, GL, etc.
* hctest test for models in monte carlo
* Based on imtest 1.0.0
* hcimtest <nobs>

program define hctest
    version 5.0
    local _endno `1'

    local rhs "x1 x2 x3 x4"
    local rhs2 "x11 x12 x13 x14 x22 x23 x24 x33 x34 x44"


quietly {
    tempvar lhs

* cook-weisberg
    reg res2s2 yhat in 1/`_endno'
    global cw = _result(2)/2
    global cwp = chiprob(1,$cw)

* White hetero test - *** 14 df
    regres res2 `rhs' `rhs2' in 1/`_endno'
    global wh = _result(1)*_result(7) /* #obs * R^2 */
    global whp = chiprob(14,$wh)

* breush pagan
    regres res2s2 `rhs' in 1/`_endno'
    global bp = _result(2)/2  /* ss/2 */
    scalar S_9 = _result(3)          /* mdf  */
    global bpp = chiprob(S_9,$bp)

*Glejser
    regres resabs `rhs' in 1/`_endno'
    global gl = _result(1)*_result(7) /* #obs * R^2 */
    scalar S_9 = _result(3)          /* model df */
    global glp = chiprob(S_9,$gl)
}

/*
di "1w" $wh
di $whp
di "1b" $bp
di $bpp
di "1g" $gl
di $glp
di "1c" $cw
di $cwp
*/

/*
di in gr _col(1) "White:         " S_1 "  " S_2
di in gr _col(1) "Breusch-Pagan: " S_3 "  " S_4
di in gr _col(1) "Glejser:       " S_5 "  " S_6
di in gr _col(1) "Cook-Weisberg: " S_9 "  " S_10
*/
end
