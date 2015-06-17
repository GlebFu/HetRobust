*! version 1.2.4 3/31/1998 - fix crash...
*! version 1.2.3 3/18/98 - include datenum
*! version 1.2.2 3/18/98 - use jslhc# not hc#
*! version 1.2.1 10/9/97 - fix bp and cook weisberg
*! version 1.2.0 10/7/97 - add bp and cook weisberg
*! version 1.2.0 10/1/97 - add glejser
*! version 1.1.0 9/26/97 - clean up and add new info in output
*! version 1.0.7 9/26/97 - make factor explicit; write factor and label
*! version 1.0.6 9/18/97 - specify dsname to use
*! version 1.0.5 9/15/97 - don't crunch results

* hcmonte <dsnumber> <dslabel> <structurename> <sample sizes>
*   dsnumber - jslhc# used for data
*   dslabel  - label for results
*       e.g., jshc1 used for hc4 and hc5 with different R2's
*   structure name: n4, t4, etc.
*   sample sizes: if none given, assumes 25 50 100 250 500 1000

capture program drop hcmonte
capture program drop hcest
capture program drop hcsim

* =====================================================
* hcest - runs regs and posts results

program define hcest
    version 5.0
    local _endno `1'  /* sample size passed by simuls */
    * randomly select variables
    quietly replace sortvar = uniform()
    sort sortvar
* HC0
    version 4.0
    quietly hreg y x1 x2 x3 x4 in 1/`_endno'
    version 5.0
    local s1m1 = _se[_cons]
    local s2m1 = _se[x1]
    local s3m1 = _se[x2]
    local s4m1 = _se[x3]
    local s5m1 = _se[x4]
* HC1
    quietly regress y x1 x2 x3 x4 in 1/`_endno', robust
    scalar S_10 = _result(9)*_result(9)*_result(5)/_result(1)
    local s1m2 = _se[_cons]
    local s2m2 = _se[x1]
    local s3m2 = _se[x2]
    local s4m2 = _se[x3]
    local s5m2 = _se[x4]
    quietly {
        regress y x1 x2 x3 x4 in 1/`_endno'
        predict resid in 1/`_endno', res
        gen res2 = resid^2 in 1/`_endno'
        gen resabs = abs(resid) in 1/`_endno'
        gen res2s2 = res2/(_result(4)/_result(1)) in 1/`_endno'
        predict float yhat in 1/`_endno'
    }
    hctest `_endno'  /* compute IM tests */
    drop resid res2 resabs res2s2 yhat

    global cw = _result(2)/2
    global cwp = chiprob(1,$cw)
    global wh = _result(1)*_result(7) /* #obs * R^2 */
    global whp = chiprob(14,$wh)
    global bp = _result(2)/2  /* ss/2 */
    global bpp = chiprob(S_9,$bp)
    global gl = _result(1)*_result(7) /* #obs * R^2 */
    global glp = chiprob(S_9,$gl)

* HC2
    quietly regress y x1 x2 x3 x4 in 1/`_endno', hc2
    local s1m3 = _se[_cons]
    local s2m3 = _se[x1]
    local s3m3 = _se[x2]
    local s4m3 = _se[x3]
    local s5m3 = _se[x4]
* HC3
    quietly regress y x1 x2 x3 x4 in 1/`_endno', hc3
    local s1m4 = _se[_cons]
    local s2m4 = _se[x1]
    local s3m4 = _se[x2]
    local s4m4 = _se[x3]
    local s5m4 = _se[x4]
* Standard OLS
    quietly regress y x1 x2 x3 x4 in 1/`_endno'    /* runs ols on sample  */
    local s1m5 = _se[_cons]
    local s2m5 = _se[x1]
    local s3m5 = _se[x2]
    local s4m5 = _se[x3]
    local s5m5 = _se[x4]
* Post standard errors and estimated betas

/*
di "w" $wh
di $whp
di "b" $bp
di $bpp
di "g" $gl
di $glp
di "c" $cw
di $cwp
*/
    #delimit ;
    post myres
         _b[_cons]  _b[x1]   _b[x2]   _b[x3]   _b[x4]
         `b0'       `b1'     `b2'     `b3'     `b4'
         `s1m1'     `s2m1'   `s3m1'   `s4m1'   `s5m1'
         `s1m2'     `s2m2'   `s3m2'   `s4m2'   `s5m2'
         `s1m3'     `s2m3'   `s3m3'   `s4m3'   `s5m3'
         `s1m4'     `s2m4'   `s3m4'   `s4m4'   `s5m4'
         `s1m5'     `s2m5'   `s3m5'   `s4m5'   `s5m5'
         $wh $whp $bp $bpp $gl $glp $cw $cwp
         $seedmc $simnum_ $dtanum $dtalbl $factor $r2 `_endno'
         $dt
         ;
    #delimit cr
* return to simuls
end

* =====================================================
* hcsim - controls iterations & puts the data generated into "file + _sz"

program define hcsim
    local outnm `1'
    local sampleN `2'
    local n `sampleN'
    if `n'<100 { local n "0`n'" }
    if `n'<1000 { local n "0`n'" }
    local _outfil `outnm'`n'
    display " "
    display "Sample size:  `sampleN'"
    display "Results file: `_outfil'"

* Statistics code: s#m# contain se(b)
* s#: statistic #
*  1-b0; 2-b1; 3-b2; 4-b3; 5-b4
* m#: method #
*  1-HC0; 6-HC1; 5-HC2; 4-HC3; 5-OLS

    #delimit ;
    postfile myres
        s1   s2   s3   s4   s5
        s1m1 s2m1 s3m1 s4m1 s5m1
        s1m2 s2m2 s3m2 s4m2 s5m2
        s1m3 s2m3 s3m3 s4m3 s5m3
        s1m4 s2m4 s3m4 s4m4 s5m4
        s1m5 s2m5 s3m5 s4m5 s5m5
        wh whpr bp bppr gl glpr cw cwpr
        seedis niter dsnumber dslabel factor R2 sampleN
        datenum
        using "`_outfil'", replace;
    #delimit cr
    local i 1               /* simnum_ = number of iterations */
    di "$S_TIME. Working..."
    while `i' <= $simnum_ {
        hcest `sampleN'
        if `i'==2  { di "$S_TIME. 1 completed. Next notice is 10." }
        if `i'==10  { di "$S_TIME. 10 completed. Next notice is 25." }
        if `i'==25  { di "$S_TIME. 25 completed. Next notice is 50." }
        if `i'==50  { di "$S_TIME. 50 completed. Next notice is 100." }
        if `i'==100 { di "$S_TIME. 100 completed. Next notice is 200." }
        if `i'==200 { di "$S_TIME. 200 completed. Next notice is 300." }
        if `i'==300 { di "$S_TIME. 300 completed. Next notice is 400." }
        if `i'==400 { di "$S_TIME. 400 completed. Next notice is 500." }
        if `i'==500 { di "$S_TIME. 500 completed. Next notice is 600." }
        if `i'==600 { di "$S_TIME. 600 completed. Next notice is 700." }
        if `i'==700 { di "$S_TIME. 700 completed. Next notice is 800." }
        if `i'==800 { di "$S_TIME. 800 completed. Next notice is 900." }
        if `i'==900 { di "$S_TIME. 900 completed. Next notice is 950." }
        if `i'==950 { di "$S_TIME. 950 completed. Next notice is 975." }
        if `i'==975 { di "$S_TIME. 975 completed. Next notice is 990." }
        if `i'==990 { di "$S_TIME. 990 completed. Next notice is completion." }
        local i = `i' + 1
     }
     di "$S_TIME. Iterations complete."
     postclose myres          /* Returns from hcest */
end                           /* Returns to hcmonte */

* =====================================================
version 5.0

program define hcmonte
    if "`1'"=="" {
        di "Syntax: hcmonte <dsnumber> <dslabel> <structurename> <sample sizes>"
        exit }
    if "$factor"=="" {
        di in red "Error: " in w "global factor #" in r " must be set."
        exit }
    if "$simnum_"=="" { global simnum_ = 1000 }
    local dtanum `1'
    global dtanum `dtanum'
    macro shift
    local dtalbl `1'
    global dtalbl `dtalbl'
    macro shift
    local outnm `1'
    macro shift
    local parms `*'
    if "`*'" == "" {
        local parms "25 50 100 250 500 1000" }
    parse "`parms'",p(" ")

local dt = date("$S_DATE","dm19y")
global dt `dt'
  * call program to construct the data structure
    di "** Constructing data to analyze..."
    use jslhc`dtanum',clear
*di "outnm `outnm'"
    quietly hc`outnm'
    drop ebase xb e
  * loop through the sample sizes specified
    di "*  Simulation date: $dt"
    di "*  Population:      hc`dtanum'"
    di "*  Data Label:      hc`dtalbl'"
    di "*  Factor:          $factor"
    di "*  Structure:       hc`outnm'"
    di "*  Sample N's:      `*'"
    di "*  Selection seed:  $seedmc"
    di "*  Iterations:      $simnum_"
    di ""
    di "*  Population regression:"
    regress y x1 x2 x3 x4
    global r2 = _result(7)
    di "** Simulations follow:"
    local i = 1
    while "``i''" ~=""   {
        hcsim `outnm' ``i''
        local i = `i' + 1
    }
end
