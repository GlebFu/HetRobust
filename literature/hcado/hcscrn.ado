*! version 1.0.0 6/11/98 - only do screen at .10

* hcscrn <dsnumber> <structurename> <sample sizes>

capture program drop hcscrn
capture program drop hcwhite

* ==================================================
* hcwhite white for two step
program define hcwhite
    local niter = niter[1]
    local dsnumis = dsnumber[1]
    local seedis = seedis[1]
    local dslblis = dslabel[1]
    local factis = factor[1]
    local sampleN = sampleN[1]
    local datenum = datenum[1]
    local n123 `2'

    di "** White pass for N=`sampleN'"

* Statistics code: s#m# - b/se(b) for these combinations
*
*  s#: statistic # : 1-b0; 2-b1; 3-b2; 4-b3; 5-b4
*
*  m#: method #    : 1-HC0; 2-HC1; 3-HC2; 4-HC3; 5-OLS

* Compute nominal size and power
    local df = `sampleN' - 5
    tempname t05 t10 t
    scalar `t05' = invt(`df',.95)

* $sg is the significance level for the t-test of the beta
*   : it is not the sig level of the screening test

    scalar `t' = `t05'
    local s1 5 
    local sg $sg
    capture drop tse tplus tminus pops1 pops2 pops3 pops4 pops5
    quietly {
        gen tse = .
        gen tplus = .
        gen tminus = .
        gen pops1 = $pops1
        gen pops2 = $pops2
        gen pops3 = $pops3
        gen pops4 = $pops4
        gen pops5 = $pops5
        gen zero = 0
    }

* loop for standard ols and HC test for $sg level

    local m 1
    while `m' < 6 {
        local s 1
        while `s' < 6 {
            quietly replace tse = `t' * s`s'm`m'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
          * nominal size
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s`s'`sg'm`m' = _result(1) / `niter'
          * power
            quietly count if zero < tminus | zero > tplus
            local p`s'`sg'm`m' = _result(1) / `niter'
            local s = `s' + 1
        }
        local m = `m' + 1
    }

* HET test results at levels .05 and .10
    quietly count if whpr < .05
    local wh05 = _result(1) / `niter'
    quietly count if whpr < .10
    local wh10 = _result(1) / `niter'

* construct two step process with the $sg sig level of t-test of betas
* at levels .05 for OLS, HC0, HC1, HC2 and HC3.

    local m 1
    while `m' < 6 {
        local s 1
        * loop over beta's
        while `s' < 6 {
            tempvar s2w
            quietly {
            * start with ols results (m5)
            gen `s2w' = s`s'm5
            * screen at .05
            replace `s2w' = s`s'm`m' if whpr < .05 /* hc if hc test <.05 */
            }
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local sW`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local pW`m'b`s' = _result(1) / `niter'
            drop `s2w'

            * loop over statistics
            local s = `s' + 1
        }
        local m = `m' + 1
    }
    
    local m0 = 0
    local m1 = 1
    local m2 = 2
    local m3 = 3
    local mols = .
    local seedis = seedis[1]
    local niter = niter[1]
    local ss `s1'
    local r2 $r2
    
* Post results
    #delimit ;
    post myres `dsnumis' `dslblis' `factis' `niter' `seedis' `n123'
        `sampleN' `r2'
        `s2`sg'm1' `s3`sg'm1' `s4`sg'm1' `s5`sg'm1'
        `s2`sg'm2' `s3`sg'm2' `s4`sg'm2' `s5`sg'm2'
        `s2`sg'm3' `s3`sg'm3' `s4`sg'm3' `s5`sg'm3'
        `s2`sg'm4' `s3`sg'm4' `s4`sg'm4' `s5`sg'm4'
        `s2`sg'm5' `s3`sg'm5' `s4`sg'm5' `s5`sg'm5'
        `p2`sg'm1' `p3`sg'm1' `p4`sg'm1' `p5`sg'm1'
        `p2`sg'm2' `p3`sg'm2' `p4`sg'm2' `p5`sg'm2'
        `p2`sg'm3' `p3`sg'm3' `p4`sg'm3' `p5`sg'm3'
        `p2`sg'm4' `p3`sg'm4' `p4`sg'm4' `p5`sg'm4'
        `p2`sg'm5' `p3`sg'm5' `p4`sg'm5' `p5`sg'm5'
        `m0' `m1' `m2' `m3' `mols'
        `wh05' `wh10' 
        `sW1b2' `sW1b3' `sW1b4' `sW1b5'
        `sW2b2' `sW2b3' `sW2b4' `sW2b5'        
        `sW3b2' `sW3b3' `sW3b4' `sW3b5'        
        `sW4b2' `sW4b3' `sW4b4' `sW4b5'        
        `sW5b2' `sW5b3' `sW5b4' `sW5b5'        
        `pW1b2' `pW1b3' `pW1b4' `pW1b5'
        `pW2b2' `pW2b3' `pW2b4' `pW2b5'        
        `pW3b2' `pW3b3' `pW3b4' `pW3b5'        
        `pW4b2' `pW4b3' `pW4b4' `pW4b5'        
        `pW5b2' `pW5b3' `pW5b4' `pW5b5'        
        ;
        #delimit cr

/*

    #delimit ;
    post myres `dsnumis' `dslblis' `factis' `niter' `seedis' `n123'
        `sampleN' `r2'
        `s2`sg'm1' `s3`sg'm1' `s4`sg'm1' `s5`sg'm1'
        `s2`sg'm2' `s3`sg'm2' `s4`sg'm2' `s5`sg'm2'
        `s2`sg'm3' `s3`sg'm3' `s4`sg'm3' `s5`sg'm3'
        `s2`sg'm4' `s3`sg'm4' `s4`sg'm4' `s5`sg'm4'
        `s2`sg'm5' `s3`sg'm5' `s4`sg'm5' `s5`sg'm5'
        `p2`sg'm1' `p3`sg'm1' `p4`sg'm1' `p5`sg'm1'
        `p2`sg'm2' `p3`sg'm2' `p4`sg'm2' `p5`sg'm2'
        `p2`sg'm3' `p3`sg'm3' `p4`sg'm3' `p5`sg'm3'
        `p2`sg'm4' `p3`sg'm4' `p4`sg'm4' `p5`sg'm4'
        `p2`sg'm5' `p3`sg'm5' `p4`sg'm5' `p5`sg'm5'
        `m0' `m1' `m2' `m3' `mols'
        `wh05' `wh10' 
        `sW1b2' `sW1b3' `sW1b4' `sW1b5'
        `sW2b2' `sW2b3' `sW2b4' `sW2b5'        
        `sW3b2' `sW3b3' `sW3b4' `sW3b5'        
        `sW4b2' `sW4b3' `sW4b4' `sW4b5'        
        `sW5b2' `sW5b3' `sW5b4' `sW5b5'        
        `pW1b2' `pW1b3' `pW1b4' `pW1b5'
        `pW2b2' `pW2b3' `pW2b4' `pW2b5'        
        `pW3b2' `pW3b3' `pW3b4' `pW3b5'        
        `pW4b2' `pW4b3' `pW4b4' `pW4b5'        
        `pW5b2' `pW5b3' `pW5b4' `pW5b5'        
        ;
        #delimit cr

*/

        
end /* return to hcscrn */

*============================================
*============================================
program define hcscrn
pause off
    parse "`*'",p(" ")
    if "`1'"=="" {
        di "=> hcscrn <dsnum> <structurename> <sample sizes>"
        exit }
    * sg is sig level for first pass with hccr1
    local sg "05"
    global sg "`sg'"
    local dtanum `1'
    global dtanum `dtanum'
    macro shift
    local strucnm `1'
    global strucnm `strucnm'
    macro shift
    local parms `*'
    if "`*'" == "" {
        local parms "25 50 100 250 500 1000" }
    global parmis `parms'
    parse "`parms'",p(" ")
    local sampleN `1'
    local n `sampleN'
    if `n'<100 { local n "0`n'" }
    if `n'<1000 { local n "0`n'" }

* get one of the results files to get some parameters
    local infile `strucnm'`n'
    use "`infile'", clear
    local niter = niter[1]
    local dsnumis = dsnumber[1]
    local dslblis = dslabel[1]
    local seedis = seedis[1]
    local factis = factor[1]
    global dt = datenum[1]
    global factor = `factis'
    local r2 = round(R2[1],.01)
    global r2 `r2'
    di " "
di "** Constructing data to analyze..."
    ts
    use jslhc`dtanum',clear
    * construct errors
    quietly hc`strucnm'
    drop ebase xb e
    global structis "`strucnm'.jslhc`dtanum'.`dslblis'"
    di "** Population Data Used for Simulations"
    di "*    Structure:       $structis"
    di "*    Simulation date: $dt"
    di "*    Seed:            `seedis'"
    di "*    Factor:          $factor"
    di "*    R2:              `r2'"
    di "*    Sample N's:      `*'"
    local filenm hc`dtanum'
    local resfile "`strucnm'scr"
    global resfile `resfile'
    summarize y x1 x2 x3 x4
    regress y x1 x2 x3 x4
    global r2 = _result(7)
    global pops1 = _b[_cons]
    global pops2 = _b[x1]
    global pops3 = _b[x2]
    global pops4 = _b[x3]
    global pops5 = _b[x4]
    
    #delimit ;
    postfile myres
        dsnumber dslabel factor niter seedis n123 sampleN R2
        s0HCb1 s0HCb2 s0HCb3 s0HCb4
        s1HCb1 s1HCb2 s1HCb3 s1HCb4
        s2HCb1 s2HCb2 s2HCb3 s2HCb4
        s3HCb1 s3HCb2 s3HCb3 s3HCb4
        sOLSb1 sOLSb2 sOLSb3 sOLSb4
        p0HCb1 p0HCb2 p0HCb3 p0HCb4
        p1HCb1 p1HCb2 p1HCb3 p1HCb4
        p2HCb1 p2HCb2 p2HCb3 p2HCb4
        p3HCb1 p3HCb2 p3HCb3 p3HCb4
        pOLSb1 pOLSb2 pOLSb3 pOLSb4
        m0 m1 m2 m3 mols
        wh05 wh10 
        sW0HCb1 sW0HCb2 sW0HCb3 sW0HCb4
        sW1HCb1 sW1HCb2 sW1HCb3 sW1HCb4
        sW2HCb1 sW2HCb2 sW2HCb3 sW2HCb4
        sW3HCb1 sW3HCb2 sW3HCb3 sW3HCb4
        sWOLSb1 sWOLSb2 sWOLSb3 sWOLSb4
        pW0HCb1 pW0HCb2 pW0HCb3 pW0HCb4
        pW1HCb1 pW1HCb2 pW1HCb3 pW1HCb4
        pW2HCb1 pW2HCb2 pW2HCb3 pW2HCb4
        pW3HCb1 pW3HCb2 pW3HCb3 pW3HCb4
        pWOLSb1 pWOLSb2 pWOLSb3 pWOLSb4
        using "`resfile'", replace;
    #delimit cr
    
    
/*
    
    #delimit ;
    postfile myres
        dsnumber dslabel factor niter seedis n123 sampleN R2
        s0HCb1 s0HCb2 s0HCb3 s0HCb4
        s1HCb1 s1HCb2 s1HCb3 s1HCb4
        s2HCb1 s2HCb2 s2HCb3 s2HCb4
        s3HCb1 s3HCb2 s3HCb3 s3HCb4
        sOLSb1 sOLSb2 sOLSb3 sOLSb4
        p0HCb1 p0HCb2 p0HCb3 p0HCb4
        p1HCb1 p1HCb2 p1HCb3 p1HCb4
        p2HCb1 p2HCb2 p2HCb3 p2HCb4
        p3HCb1 p3HCb2 p3HCb3 p3HCb4
        pOLSb1 pOLSb2 pOLSb3 pOLSb4
        m0 m1 m2 m3 mols
        wh05 wh10 
        sW0HCb1 sW0HCb2 sW0HCb3 sW0HCb4
        sW1HCb1 sW1HCb2 sW1HCb3 sW1HCb4
        sW2HCb1 sW2HCb2 sW2HCb3 sW2HCb4
        sW3HCb1 sW3HCb2 sW3HCb3 sW3HCb4
        sWOLSb1 sWOLSb2 sWOLSb3 sWOLSb4
        pW0HCb1 pW0HCb2 pW0HCb3 pW0HCb4
        pW1HCb1 pW1HCb2 pW1HCb3 pW1HCb4
        pW2HCb1 pW2HCb2 pW2HCb3 pW2HCb4
        pW3HCb1 pW3HCb2 pW3HCb3 pW3HCb4
        pWOLSb1 pWOLSb2 pWOLSb3 pWOLSb4
        using "`resfile'", replace;
    #delimit cr

*/
    
    
    local i = 1

* Loop through sample sizes to crunch simulation results

    while "`1'" ~= ""   {
        local sampleN = `1'
        local n `sampleN'
        if `n'<100 { local n "0`n'" }
        if `n'<1000 { local n "0`n'" }
        local infile `strucnm'`n'
        use "`infile'", clear
        display " "
        display "** Description of Simulation Results in `infile'"
        display "*  Sig Level/Sample N: $sg/`sampleN'"
        summarize
        * crunch results for given sample size
        hcwhite `sampleN' `i'
        macro shift
        local i = `i' + 1
    }
    postclose myres

    use `resfile',clear
    * datenum from simulation results file.
    local dt = $dt
    gen datenum = `dt'
    label var datenum "Numeric date of simulations."
    quietly compress
    save `resfile',replace
end


