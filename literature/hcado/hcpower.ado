*! version 1.0.0 6/11/98 - compute power curves
* hcpower <dsnumber> <structurename> <sample sizes>
* only for b3.

capture program drop hcpower
capture program drop hcpwork

* ===================================
program define hcpwork
    local niter = niter[1]
    local dsnumis = dsnumber[1]
    local seedis = seedis[1]
    local dslblis = dslabel[1]
    local factis = factor[1]
    local sampleN = sampleN[1]
    local n123 `2'

* Statistics code: s#m# - b/se(b) for these combinations
*
*  s#: statistic # : 1-b0; 2-b1; 3-b2; 4-b3; 5-b4
*
*  m#: method #    : 1-HC0; 2-HC1; 3-HC2; 4-HC3; 5-OLS

* Compute nominal size and power
    local df = `sampleN' - 5
    tempname t05 t
    scalar `t' = invt(`df',.95)
    local s1 5
    local sg $sg
    capture drop tse tplus tminus pops1 pops2 pops3 pops4 pops5
    quietly {
        gen sd = .
        gen tse = .
        gen tplus = .
        gen tminus = .
        gen pops4 = $pops4
        gen bis = 0
    }

    local seedis = seedis[1]
    local niter = niter[1]
    local ss `s1'
    local r2 $r2

* loop for standard ols and HC test for $sg level
local bval 0
while `bval' < 61 {

    local m 1
    while `m' < 6 {

        * tse: t value times standard error for b3 using method m`m'
        quietly replace tse = `t' * s4m`m'
        * upper and lower levels of .05 confidence interval around true
        * s4 is the sample estimate for b3.
        quietly replace tplus = s4 + tse
        quietly replace tminus = s4 - tse
        * nominal size
        quietly count if pops4 < tminus | pops4 > tplus
        * size distortion
        local sd`m' = (_result(1) / `niter') - .05
        * power
        * bis: test that estimate == bis
        * if bis is outside confidence interval, it is significant.
        quietly replace bis = `bval'/10  + ($pops4 - 1)
* this foolishness is because the obvious way to do it crashed?!
quietly replace tplus = s4 + tse + 2
quietly replace tminus = s4 - tse + 2

        quietly count if bis < tminus | bis > tplus
        local pm`m' = _result(1) / `niter'
        * size adjusted power
        quietly replace sd = `sd`m''
        local pam`m' = (_result(1) / `niter') - sd
        local m = `m' + 1
    }

    local btst = bis[1]

    #delimit ;
       post myres `dsnumis'
      `dslblis' `factis' `niter'  `seedis' `n123'
      `sampleN' `r2' `btst'
      `pm1' `pm2' `pm3' `pm4' `pm5'
      `pam1' `pam2' `pam3' `pam4' `pam5'            
       ;
      #delimit cr
      
*`sd1' `sd2' `sd3' `sd4' `sd5'                  

local bval = `bval' + 1        
}        

end /* return to hcresult */

*============================================
*============================================
program define hcpower
pause off
    parse "`*'",p(" ")
    if "`1'"=="" {
        di "=> hcpower <dsnum> <structurename> <sample sizes>"
        exit }
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
    local resfile "`strucnm'r1`sg'"
    global resfile `resfile'
    summarize y x1 x2 x3 x4
    regress y x1 x2 x3 x4
    global r2 = _result(7)
    global pops1 = _b[_cons]
    global pops2 = _b[x1]
    global pops3 = _b[x2]
    global pops4 = _b[x3]
    global pops5 = _b[x4]
/*
di " "
di "* Results statistics have the format:"
di " c1:   [s|p] - size or power"
di " c2-4: [0HC|1HC|2HC|3HC|OLS] - method of computing covariance matrix."
di " c2:   [0|1|2|3] - HC method after screening."
di " c3:   [W|G|B|C] - type of screening"
di " c4:   [1|5] - screen at .05 or .10 level"
di " c5-6: [05|10] - sig level of test"
di " c7-8: [b1|b2|b3|b4] - test statistic"
*/


* p... is power; pa... is size adjusted power; sd... is size distortion.
    #delimit ;
    postfile myres
        dsnumber dslabel factor niter seedis n123 sampleN R2
        btstval
        pHC0  pHC1  pHC2  pHC3  pOLS
        paHC0 paHC1 paHC2 paHC3 paOLS                
        using "`resfile'", replace;
      ;
      #delimit cr


*sdHC0 sdHC1 sdHC2 sdHC3 sdOLS                

        
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
        hcpwork `sampleN' `i'
        macro shift
        local i = `i' + 1
    }
    postclose myres

    use $resfile,clear
    * datenum from simulation results file.
    local dt = $dt
    gen datenum = `dt'
    label var datenum "Numeric date of simulations."
    local resfile "`strucnm'pwr"
    quietly compress
    replace btstval = btstval - 2
    save `strucnm'pwr,replace
end


