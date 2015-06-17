*! version 2.0.0 3/27/1998 - merge hcresult with hcres2

* hcresult <dsnumber> <structurename> <sample sizes>
* : hcresult 05 2 n4 25 50 100
* : hcresult 10 2 n4   <-- will assume 25 50 100 250 500 1000

capture program drop hcresult
capture program drop hcres2
capture program drop hccr1
capture program drop hccr2a
capture program drop hccr2b

* ==================================================
* hccr2a white and glejser computations for two step
* test at various screening pvalues.
program define hccr2a
    local niter = niter[1]
    local dsnumis = dsnumber[1]
    local seedis = seedis[1]
    local dslblis = dslabel[1]
    local factis = factor[1]
    local sampleN = sampleN[1]
    local datenum = datenum[1]
    local n123 `2'

    di "** Glejser and White pass for N=`sampleN'"
/*
    display " "
    display "** Results for:"
    display "*  Sample Size:     `sampleN'"
    display "*  Selection Seed:  `seedis'"
    display "*  Simulation date: `datenum'"
    display "*  # of Iterations: `niter'"
    display "*  Sig Level:       $sg"
    summarize s1,meanonly
    local means1 = _result(3)
    summarize s2,meanonly
    local means2 = _result(3)
    summarize s3,meanonly
    local means3 = _result(3)
    summarize s4,meanonly
    local means4 = _result(3)
    summarize s5,meanonly
    local means5 = _result(3)
    display "*  Means of sample estimates of:"
    display "   b0 = `means1'"
    display "   b1 = `means2'"
    display "   b2 = `means3'"
    display "   b3 = `means4'"
    display "   b4 = `means5'"
*/

* Statistics code: s#m# - b/se(b) for these combinations
*
*  s#: statistic # : 1-b0; 2-b1; 3-b2; 4-b3; 5-b4
*
*  m#: method #    : 1-HC0; 2-HC1; 3-HC2; 4-HC3; 5-OLS

* Compute nominal size and power
    local df = `sampleN' - 5
    tempname t05 t10 t
    scalar `t05' = invt(`df',.95)
    scalar `t10' = invt(`df',.90)

* $sg is the significance level for the t-test of the beta
*   : it is not the sig level of the screening test

    if "$sg"=="05" { scalar `t' = `t05' }
    if "$sg"=="05" { local s1 5 }
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

* HET test results at levels .05, .10-.60
    quietly count if whpr < .05
    local wh05 = _result(1) / `niter'
    quietly count if whpr < .10
    local wh10 = _result(1) / `niter'
    quietly count if whpr < .20
    local wh20 = _result(1) / `niter'
    quietly count if whpr < .30
    local wh30 = _result(1) / `niter'
    quietly count if whpr < .40
    local wh40 = _result(1) / `niter'
    quietly count if whpr < .50
    local wh50 = _result(1) / `niter'
    quietly count if whpr < .60
    local wh60 = _result(1) / `niter'
    quietly count if glpr < .05
    local gl05 = _result(1) / `niter'
    quietly count if glpr < .10
    local gl10 = _result(1) / `niter'
    quietly count if glpr < .20
    local gl20 = _result(1) / `niter'
    quietly count if glpr < .30
    local gl30 = _result(1) / `niter'
    quietly count if glpr < .40
    local gl40 = _result(1) / `niter'
    quietly count if glpr < .50
    local gl50 = _result(1) / `niter'
    quietly count if glpr < .60
    local gl60 = _result(1) / `niter'

* construct two step process with the $sg sig level of t-test of betas
* at levels .05, .1, .2, .3, .4, .5 for HC3 only.

        local m 4   /* only do HC3 */
        local s 1
        * loop over beta's
        while `s' < 6 {
            tempvar s2g s2w s2c s2b
* .05 screen
            quietly {
            * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
            * screen at .05
            replace `s2g' = s`s'm`m' if glpr < .05 /* hc if overall <.05 */
            replace `s2w' = s`s'm`m' if whpr < .05 /* hc if hc test <.05 */
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s0`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p0`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s0`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p0`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
* .10 screen
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
          * .10 screen at $sg level
            replace `s2g' = s`s'm`m' if glpr < .10 /* hc if overall <.10 */
            replace `s2w' = s`s'm`m' if whpr < .10 /* hc if hc test <.10 */
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
* .20 screen
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
          * .20 screen at $sg level
            replace `s2g' = s`s'm`m' if glpr < .20 /* hc if overall <.20 */
            replace `s2w' = s`s'm`m' if whpr < .20 /* hc if hc test <.20 */
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s2`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p2`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s2`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p2`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
* .30 screen
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
            replace `s2g' = s`s'm`m' if glpr < .30 /* hc if overall <.30 */
            replace `s2w' = s`s'm`m' if whpr < .30 /* hc if hc test <.30 */
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s3`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p3`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s3`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p3`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
* .40 screen
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
          * .40 screen at $sg level
            replace `s2g' = s`s'm`m' if glpr < .40 /* hc if overall <.40 */
            replace `s2w' = s`s'm`m' if whpr < .40 /* hc if hc test <.40 */
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s4`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p4`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s4`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p4`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
* .50 screen
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
          * .50 screen at $sg level
            replace `s2g' = s`s'm`m' if glpr < .50 /* hc if overall <.50 */
            replace `s2w' = s`s'm`m' if whpr < .50 /* hc if hc test <.50 */
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
/*
* .60 screen
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
          * .60 screen at $sg level
            replace `s2g' = s`s'm`m' if glpr < .60
            replace `s2w' = s`s'm`m' if whpr < .60
            }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s6`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p6`s1'G`m'b`s' = _result(1) / `niter'
          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s6`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p6`s1'W`m'b`s' = _result(1) / `niter'
            drop `s2g' `s2w'
*/
* End of screen sets

            * loop over statistics
            local s = `s' + 1
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
    post myres `dsnumis'
        `dslblis' `factis' `niter'  `seedis' `n123'
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
        `gl05' `gl10' `gl20' `gl30' `gl40' `gl50' `gl60'
        `wh05' `wh10' `wh20' `wh30' `wh40' `wh50' `wh60'
        `s0`ss'G4b2' `s0`ss'G4b3' `s0`ss'G4b4' `s0`ss'G4b5'
        `p0`ss'G4b2' `p0`ss'G4b3' `p0`ss'G4b4' `p0`ss'G4b5'
        `s1`ss'G4b2' `s1`ss'G4b3' `s1`ss'G4b4' `s1`ss'G4b5'
        `p1`ss'G4b2' `p1`ss'G4b3' `p1`ss'G4b4' `p1`ss'G4b5'
        `s2`ss'G4b2' `s2`ss'G4b3' `s2`ss'G4b4' `s2`ss'G4b5'
        `p2`ss'G4b2' `p2`ss'G4b3' `p2`ss'G4b4' `p2`ss'G4b5'
        `s3`ss'G4b2' `s3`ss'G4b3' `s3`ss'G4b4' `s3`ss'G4b5'
        `p3`ss'G4b2' `p3`ss'G4b3' `p3`ss'G4b4' `p3`ss'G4b5'
        `s4`ss'G4b2' `s4`ss'G4b3' `s4`ss'G4b4' `s4`ss'G4b5'
        `p4`ss'G4b2' `p4`ss'G4b3' `p4`ss'G4b4' `p4`ss'G4b5'
        `s5`ss'G4b2' `s5`ss'G4b3' `s5`ss'G4b4' `s5`ss'G4b5'
        `p5`ss'G4b2' `p5`ss'G4b3' `p5`ss'G4b4' `p5`ss'G4b5'
/*        `s6`ss'G4b2' `s6`ss'G4b3' `s6`ss'G4b4' `s6`ss'G4b5'
        `p6`ss'G4b2' `p6`ss'G4b3' `p6`ss'G4b4' `p6`ss'G4b5' */
        `s0`ss'W4b2' `s0`ss'W4b3' `s0`ss'W4b4' `s0`ss'W4b5'
        `p0`ss'W4b2' `p0`ss'W4b3' `p0`ss'W4b4' `p0`ss'W4b5'
        `s1`ss'W4b2' `s1`ss'W4b3' `s1`ss'W4b4' `s1`ss'W4b5'
        `p1`ss'W4b2' `p1`ss'W4b3' `p1`ss'W4b4' `p1`ss'W4b5'
        `s2`ss'W4b2' `s2`ss'W4b3' `s2`ss'W4b4' `s2`ss'W4b5'
        `p2`ss'W4b2' `p2`ss'W4b3' `p2`ss'W4b4' `p2`ss'W4b5'
        `s3`ss'W4b2' `s3`ss'W4b3' `s3`ss'W4b4' `s3`ss'W4b5'
        `p3`ss'W4b2' `p3`ss'W4b3' `p3`ss'W4b4' `p3`ss'W4b5'
        `s4`ss'W4b2' `s4`ss'W4b3' `s4`ss'W4b4' `s4`ss'W4b5'
        `p4`ss'W4b2' `p4`ss'W4b3' `p4`ss'W4b4' `p4`ss'W4b5'
        `s5`ss'W4b2' `s5`ss'W4b3' `s5`ss'W4b4' `s5`ss'W4b5'
        `p5`ss'W4b2' `p5`ss'W4b3' `p5`ss'W4b4' `p5`ss'W4b5'
/*        `s6`ss'W4b2' `s6`ss'W4b3' `s6`ss'W4b4' `s6`ss'W4b5'
        `p6`ss'W4b2' `p6`ss'W4b3' `p6`ss'W4b4' `p6`ss'W4b5' */
        ;
        #delimit cr
end /* return to hcresult */


* ==================================================
* hccr2a BP and CW computations for two step
* test at various screening pvalues.
program define hccr2b
    local niter = niter[1]
    local dsnumis = dsnumber[1]
    local seedis = seedis[1]
    local dslblis = dslabel[1]
    local factis = factor[1]
    local sampleN = sampleN[1]
    local n123 `2'
    di "** BP and CW pass for N=`sampleN'"
/*
    summarize s1,meanonly
    local means1 = _result(3)
    summarize s2,meanonly
    local means2 = _result(3)
    summarize s3,meanonly
    local means3 = _result(3)
    summarize s4,meanonly
    local means4 = _result(3)
    summarize s5,meanonly
    local means5 = _result(3)
*/
* Compute nominal size and power
    local df = `sampleN' - 5
    tempname t05 t10 t
    scalar `t05' = invt(`df',.95)
    scalar `t10' = invt(`df',.90)
* $sg is the significance level for the t-test of the beta
*   : it is not the sig level of the screening test
    if "$sg"=="05" { scalar `t' = `t05' }
    if "$sg"=="05" { local s1 5 }
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

* HET test results
    quietly count if cwpr < .05
    local cw05 = _result(1) / `niter'
    quietly count if cwpr < .10
    local cw10 = _result(1) / `niter'
    quietly count if cwpr < .20
    local cw20 = _result(1) / `niter'
    quietly count if cwpr < .30
    local cw30 = _result(1) / `niter'
    quietly count if cwpr < .40
    local cw40 = _result(1) / `niter'
    quietly count if cwpr < .50
    local cw50 = _result(1) / `niter'
    quietly count if cwpr < .60
    local cw60 = _result(1) / `niter'
    quietly count if bppr < .05
    local bp05 = _result(1) / `niter'
    quietly count if bppr < .10
    local bp10 = _result(1) / `niter'
    quietly count if bppr < .20
    local bp20 = _result(1) / `niter'
    quietly count if bppr < .30
    local bp30 = _result(1) / `niter'
    quietly count if bppr < .40
    local bp40 = _result(1) / `niter'
    quietly count if bppr < .50
    local bp50 = _result(1) / `niter'
    quietly count if bppr < .60
    local bp60 = _result(1) / `niter'

    local m 4   /* only do HC3 */
    local s 1
    while `s' < 6 {
    tempvar s2g s2w s2c s2b

* .05 screen
            quietly {
            * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
            * screen at .05
            replace `s2c' = s`s'm`m' if cwpr < .05
            replace `s2b' = s`s'm`m' if bppr < .05
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s0`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p0`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for breusch pagan
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s0`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p0`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
* .10 screen
            quietly {
          * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
          * .10 screen at $sg level
            replace `s2c' = s`s'm`m' if cwpr < .10 /* hc if overall <.10 */
            replace `s2b' = s`s'm`m' if bppr < .10 /* hc if hc test <.10 */
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
* .20 screen
            quietly {
          * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
          * .20 screen at $sg level
            replace `s2c' = s`s'm`m' if cwpr < .20 /* hc if overall <.20 */
            replace `s2b' = s`s'm`m' if bppr < .20 /* hc if hc test <.20 */
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s2`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p2`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s2`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p2`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
* .30 screen
            quietly {
          * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
            replace `s2c' = s`s'm`m' if cwpr < .30 /* hc if overall <.30 */
            replace `s2b' = s`s'm`m' if bppr < .30 /* hc if hc test <.30 */
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s3`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p3`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s3`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p3`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
* .40 screen
            quietly {
          * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
          * .40 screen at $sg level
            replace `s2c' = s`s'm`m' if cwpr < .40 /* hc if overall <.40 */
            replace `s2b' = s`s'm`m' if bppr < .40 /* hc if hc test <.40 */
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s4`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p4`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s4`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p4`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
* .50 screen
            quietly {
          * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
          * .50 screen at $sg level
            replace `s2c' = s`s'm`m' if cwpr < .50 /* hc if overall <.50 */
            replace `s2b' = s`s'm`m' if bppr < .50 /* hc if hc test <.50 */
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
/*
* .60 screen
            quietly {
          * start with ols results (m5)
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
          * .60 screen at $sg level
            replace `s2c' = s`s'm`m' if cwpr < .60
            replace `s2b' = s`s'm`m' if bppr < .60
            }
          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s6`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p6`s1'C`m'b`s' = _result(1) / `niter'
          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s6`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p6`s1'B`m'b`s' = _result(1) / `niter'
            drop `s2b' `s2c'
*/

* End of screen sets
* End loop over statistics
            local s = `s' + 1
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
    post myres `dsnumis'
        `dslblis' `factis' `niter'  `seedis' `n123'
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
        `bp05' `bp10' `bp20' `bp30' `bp40' `bp50' `bp60'
        `cw05' `cw10' `cw20' `cw30' `cw40' `cw50' `cw60'
        `s0`ss'B4b2' `s0`ss'B4b3' `s0`ss'B4b4' `s0`ss'B4b5'
        `p0`ss'B4b2' `p0`ss'B4b3' `p0`ss'B4b4' `p0`ss'B4b5'
        `s1`ss'B4b2' `s1`ss'B4b3' `s1`ss'B4b4' `s1`ss'B4b5'
        `p1`ss'B4b2' `p1`ss'B4b3' `p1`ss'B4b4' `p1`ss'B4b5'
        `s2`ss'B4b2' `s2`ss'B4b3' `s2`ss'B4b4' `s2`ss'B4b5'
        `p2`ss'B4b2' `p2`ss'B4b3' `p2`ss'B4b4' `p2`ss'B4b5'
        `s3`ss'B4b2' `s3`ss'B4b3' `s3`ss'B4b4' `s3`ss'B4b5'
        `p3`ss'B4b2' `p3`ss'B4b3' `p3`ss'B4b4' `p3`ss'B4b5'
        `s4`ss'B4b2' `s4`ss'B4b3' `s4`ss'B4b4' `s4`ss'B4b5'
        `p4`ss'B4b2' `p4`ss'B4b3' `p4`ss'B4b4' `p4`ss'B4b5'
        `s5`ss'B4b2' `s5`ss'B4b3' `s5`ss'B4b4' `s5`ss'B4b5'
        `p5`ss'B4b2' `p5`ss'B4b3' `p5`ss'B4b4' `p5`ss'B4b5'
/*        `s6`ss'B4b2' `s6`ss'B4b3' `s6`ss'B4b4' `s6`ss'B4b5'
        `p6`ss'B4b2' `p6`ss'B4b3' `p6`ss'B4b4' `p6`ss'B4b5' */
        `s0`ss'C4b2' `s0`ss'C4b3' `s0`ss'C4b4' `s0`ss'C4b5'
        `p0`ss'C4b2' `p0`ss'C4b3' `p0`ss'C4b4' `p0`ss'C4b5'
        `s1`ss'C4b2' `s1`ss'C4b3' `s1`ss'C4b4' `s1`ss'C4b5'
        `p1`ss'C4b2' `p1`ss'C4b3' `p1`ss'C4b4' `p1`ss'C4b5'
        `s2`ss'C4b2' `s2`ss'C4b3' `s2`ss'C4b4' `s2`ss'C4b5'
        `p2`ss'C4b2' `p2`ss'C4b3' `p2`ss'C4b4' `p2`ss'C4b5'
        `s3`ss'C4b2' `s3`ss'C4b3' `s3`ss'C4b4' `s3`ss'C4b5'
        `p3`ss'C4b2' `p3`ss'C4b3' `p3`ss'C4b4' `p3`ss'C4b5'
        `s4`ss'C4b2' `s4`ss'C4b3' `s4`ss'C4b4' `s4`ss'C4b5'
        `p4`ss'C4b2' `p4`ss'C4b3' `p4`ss'C4b4' `p4`ss'C4b5'
        `s5`ss'C4b2' `s5`ss'C4b3' `s5`ss'C4b4' `s5`ss'C4b5'
        `p5`ss'C4b2' `p5`ss'C4b3' `p5`ss'C4b4' `p5`ss'C4b5'
/*        `s6`ss'C4b2' `s6`ss'C4b3' `s6`ss'C4b4' `s6`ss'C4b5'
        `p6`ss'C4b2' `p6`ss'C4b3' `p6`ss'C4b4' `p6`ss'C4b5' */
        ;
        #delimit cr
end /* return to hcresult */

* ===================================
program define hccr1
    local niter = niter[1]
    local dsnumis = dsnumber[1]
    local seedis = seedis[1]
    local dslblis = dslabel[1]
    local factis = factor[1]
    local sampleN = sampleN[1]
    local n123 `2'
/*
    summarize s1,meanonly
    local means1 = _result(3)
    summarize s2,meanonly
    local means2 = _result(3)
    summarize s3,meanonly
    local means3 = _result(3)
    summarize s4,meanonly
    local means4 = _result(3)
    summarize s5,meanonly
    local means5 = _result(3)
    display "*  Means of sample estimates of:"
    display "   b0 = `means1'"
    display "   b1 = `means2'"
    display "   b2 = `means3'"
    display "   b3 = `means4'"
    display "   b4 = `means5'"
*/

* Statistics code: s#m# - b/se(b) for these combinations
*
*  s#: statistic # : 1-b0; 2-b1; 3-b2; 4-b3; 5-b4
*
*  m#: method #    : 1-HC0; 2-HC1; 3-HC2; 4-HC3; 5-OLS

* Compute nominal size and power
    local df = `sampleN' - 5
    tempname t05 t10 t
    scalar `t05' = invt(`df',.95)
    scalar `t10' = invt(`df',.90)
    if "$sg"=="10" { scalar `t' = `t10' }
    if "$sg"=="05" { scalar `t' = `t05' }
    if "$sg"=="10" { local s1 1 }
    if "$sg"=="05" { local s1 5 }
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

* HET test results

    quietly count if whpr < .05
    local wh05 = _result(1) / `niter'
    quietly count if whpr < .10
    local wh10 = _result(1) / `niter'

    quietly count if glpr < .05
    local gl05 = _result(1) / `niter'
    quietly count if glpr < .10
    local gl10 = _result(1) / `niter'

    quietly count if cwpr < .05
    local cw05 = _result(1) / `niter'
    quietly count if cwpr < .10
    local cw10 = _result(1) / `niter'

    quietly count if bppr < .05
    local bp05 = _result(1) / `niter'
    quietly count if bppr < .10
    local bp10 = _result(1) / `niter'

* construct two step process with the $sg sig level
* Note: Some of these are duplicated in later code. They will be
* discarded when files are merged.

    local m 1
    while `m' < 5 {
        local s 1
        while `s' < 6 {
            tempvar s2g s2w s2c s2b
            quietly {
            * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5
            * screen at .05
            replace `s2g' = s`s'm`m' if glpr < .05 /* hc if overall <.05 */
            replace `s2w' = s`s'm`m' if whpr < .05 /* hc if hc test <.05 */
            replace `s2c' = s`s'm`m' if cwpr < .05 /* hc if overall <.05 */
            replace `s2b' = s`s'm`m' if bppr < .05 /* hc if hc test <.05 */
        }
          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'G`m'b`s' = _result(1) / `niter'

          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'W`m'b`s' = _result(1) / `niter'

          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'C`m'b`s' = _result(1) / `niter'

          * size and power for breusch pagan
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s5`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p5`s1'B`m'b`s' = _result(1) / `niter'

            drop `s2g' `s2w' `s2b' `s2c'
            quietly {
          * start with ols results (m5)
            gen `s2g' = s`s'm5
            gen `s2w' = s`s'm5
            gen `s2c' = s`s'm5
            gen `s2b' = s`s'm5

          * .10 screen at $sg level
            replace `s2g' = s`s'm`m' if glpr < .10 /* hc if overall <.10 */
            replace `s2w' = s`s'm`m' if whpr < .10
            replace `s2c' = s`s'm`m' if cwpr < .10
            replace `s2b' = s`s'm`m' if bppr < .10
            }

          * size and power for glejser
            quietly replace tse = `t' * `s2g'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'G`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'G`m'b`s' = _result(1) / `niter'

          * size and power for white
            quietly replace tse = `t' * `s2w'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'W`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'W`m'b`s' = _result(1) / `niter'

          * size and power for cook
            quietly replace tse = `t' * `s2c'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'C`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'C`m'b`s' = _result(1) / `niter'

          * size and power for bp
            quietly replace tse = `t' * `s2b'
            quietly replace tplus = s`s' + tse
            quietly replace tminus = s`s' - tse
            quietly count if pops`s' < tminus | pops`s' > tplus
            local s1`s1'B`m'b`s' = _result(1) / `niter'
            quietly count if zero < tminus | zero > tplus
            local p1`s1'B`m'b`s' = _result(1) / `niter'

            drop `s2g' `s2w' `s2b' `s2c'
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

    #delimit ;
       post myres `dsnumis'
      `dslblis' `factis' `niter'  `seedis' `n123'
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
      `s1`ss'G1b2' `s1`ss'G1b3' `s1`ss'G1b4' `s1`ss'G1b5'
      `s1`ss'W1b2' `s1`ss'W1b3' `s1`ss'W1b4' `s1`ss'W1b5'
      `p1`ss'G1b2' `p1`ss'G1b3' `p1`ss'G1b4' `p1`ss'G1b5'
      `p1`ss'W1b2' `p1`ss'W1b3' `p1`ss'W1b4' `p1`ss'W1b5'
      `s5`ss'G1b2' `s5`ss'G1b3' `s5`ss'G1b4' `s5`ss'G1b5'
      `s5`ss'W1b2' `s5`ss'W1b3' `s5`ss'W1b4' `s5`ss'W1b5'
      `p5`ss'G1b2' `p5`ss'G1b3' `p5`ss'G1b4' `p5`ss'G1b5'
      `p5`ss'W1b2' `p5`ss'W1b3' `p5`ss'W1b4' `p5`ss'W1b5'
      `s1`ss'G2b2' `s1`ss'G2b3' `s1`ss'G2b4' `s1`ss'G2b5'
      `s1`ss'W2b2' `s1`ss'W2b3' `s1`ss'W2b4' `s1`ss'W2b5'
      `p1`ss'G2b2' `p1`ss'G2b3' `p1`ss'G2b4' `p1`ss'G2b5'
      `p1`ss'W2b2' `p1`ss'W2b3' `p1`ss'W2b4' `p1`ss'W2b5'
      `s5`ss'G2b2' `s5`ss'G2b3' `s5`ss'G2b4' `s5`ss'G2b5'
      `s5`ss'W2b2' `s5`ss'W2b3' `s5`ss'W2b4' `s5`ss'W2b5'
      `p5`ss'G2b2' `p5`ss'G2b3' `p5`ss'G2b4' `p5`ss'G2b5'
      `p5`ss'W2b2' `p5`ss'W2b3' `p5`ss'W2b4' `p5`ss'W2b5'
      `s1`ss'G3b2' `s1`ss'G3b3' `s1`ss'G3b4' `s1`ss'G3b5'
      `s1`ss'W3b2' `s1`ss'W3b3' `s1`ss'W3b4' `s1`ss'W3b5'
      `p1`ss'G3b2' `p1`ss'G3b3' `p1`ss'G3b4' `p1`ss'G3b5'
      `p1`ss'W3b2' `p1`ss'W3b3' `p1`ss'W3b4' `p1`ss'W3b5'
      `s5`ss'G3b2' `s5`ss'G3b3' `s5`ss'G3b4' `s5`ss'G3b5'
      `s5`ss'W3b2' `s5`ss'W3b3' `s5`ss'W3b4' `s5`ss'W3b5'
      `p5`ss'G3b2' `p5`ss'G3b3' `p5`ss'G3b4' `p5`ss'G3b5'
      `p5`ss'W3b2' `p5`ss'W3b3' `p5`ss'W3b4' `p5`ss'W3b5'
      `s1`ss'G4b2' `s1`ss'G4b3' `s1`ss'G4b4' `s1`ss'G4b5'
      `s1`ss'W4b2' `s1`ss'W4b3' `s1`ss'W4b4' `s1`ss'W4b5'
      `p1`ss'G4b2' `p1`ss'G4b3' `p1`ss'G4b4' `p1`ss'G4b5'
      `p1`ss'W4b2' `p1`ss'W4b3' `p1`ss'W4b4' `p1`ss'W4b5'
      `s5`ss'G4b2' `s5`ss'G4b3' `s5`ss'G4b4' `s5`ss'G4b5'
      `s5`ss'W4b2' `s5`ss'W4b3' `s5`ss'W4b4' `s5`ss'W4b5'
      `p5`ss'G4b2' `p5`ss'G4b3' `p5`ss'G4b4' `p5`ss'G4b5'
      `p5`ss'W4b2' `p5`ss'W4b3' `p5`ss'W4b4' `p5`ss'W4b5'
      `s1`ss'B1b2' `s1`ss'B1b3' `s1`ss'B1b4' `s1`ss'B1b5'
      `s1`ss'C1b2' `s1`ss'C1b3' `s1`ss'C1b4' `s1`ss'C1b5'
      `p1`ss'B1b2' `p1`ss'B1b3' `p1`ss'B1b4' `p1`ss'B1b5'
      `p1`ss'C1b2' `p1`ss'C1b3' `p1`ss'C1b4' `p1`ss'C1b5'
      `s5`ss'B1b2' `s5`ss'B1b3' `s5`ss'B1b4' `s5`ss'B1b5'
      `s5`ss'C1b2' `s5`ss'C1b3' `s5`ss'C1b4' `s5`ss'C1b5'
      `p5`ss'B1b2' `p5`ss'B1b3' `p5`ss'B1b4' `p5`ss'B1b5'
      `p5`ss'C1b2' `p5`ss'C1b3' `p5`ss'C1b4' `p5`ss'C1b5'
      `s1`ss'B2b2' `s1`ss'B2b3' `s1`ss'B2b4' `s1`ss'B2b5'
      `s1`ss'C2b2' `s1`ss'C2b3' `s1`ss'C2b4' `s1`ss'C2b5'
      `p1`ss'B2b2' `p1`ss'B2b3' `p1`ss'B2b4' `p1`ss'B2b5'
      `p1`ss'C2b2' `p1`ss'C2b3' `p1`ss'C2b4' `p1`ss'C2b5'
      `s5`ss'B2b2' `s5`ss'B2b3' `s5`ss'B2b4' `s5`ss'B2b5'
      `s5`ss'C2b2' `s5`ss'C2b3' `s5`ss'C2b4' `s5`ss'C2b5'
      `p5`ss'B2b2' `p5`ss'B2b3' `p5`ss'B2b4' `p5`ss'B2b5'
      `p5`ss'C2b2' `p5`ss'C2b3' `p5`ss'C2b4' `p5`ss'C2b5'
      `s1`ss'B3b2' `s1`ss'B3b3' `s1`ss'B3b4' `s1`ss'B3b5'
      `s1`ss'C3b2' `s1`ss'C3b3' `s1`ss'C3b4' `s1`ss'C3b5'
      `p1`ss'B3b2' `p1`ss'B3b3' `p1`ss'B3b4' `p1`ss'B3b5'
      `p1`ss'C3b2' `p1`ss'C3b3' `p1`ss'C3b4' `p1`ss'C3b5'
      `s5`ss'B3b2' `s5`ss'B3b3' `s5`ss'B3b4' `s5`ss'B3b5'
      `s5`ss'C3b2' `s5`ss'C3b3' `s5`ss'C3b4' `s5`ss'C3b5'
      `p5`ss'B3b2' `p5`ss'B3b3' `p5`ss'B3b4' `p5`ss'B3b5'
      `p5`ss'C3b2' `p5`ss'C3b3' `p5`ss'C3b4' `p5`ss'C3b5'
      `s1`ss'B4b2' `s1`ss'B4b3' `s1`ss'B4b4' `s1`ss'B4b5'
      `s1`ss'C4b2' `s1`ss'C4b3' `s1`ss'C4b4' `s1`ss'C4b5'
      `p1`ss'B4b2' `p1`ss'B4b3' `p1`ss'B4b4' `p1`ss'B4b5'
      `p1`ss'C4b2' `p1`ss'C4b3' `p1`ss'C4b4' `p1`ss'C4b5'
      `s5`ss'B4b2' `s5`ss'B4b3' `s5`ss'B4b4' `s5`ss'B4b5'
      `s5`ss'C4b2' `s5`ss'C4b3' `s5`ss'C4b4' `s5`ss'C4b5'
      `p5`ss'B4b2' `p5`ss'B4b3' `p5`ss'B4b4' `p5`ss'B4b5'
      `p5`ss'C4b2' `p5`ss'C4b3' `p5`ss'C4b4' `p5`ss'C4b5'
       ;
      #delimit cr

end /* return to hcresult */

*========================================
*========================================
program define hcres2

    local sg "05"
    global sg "`sg'"
    local strucnm $strucnm

    #delimit ;
    postfile myres
        dsnumber dslabel factor niter seedis n123 sampleN R2
        s0HC`sg'b1 s0HC`sg'b2 s0HC`sg'b3 s0HC`sg'b4
        s1HC`sg'b1 s1HC`sg'b2 s1HC`sg'b3 s1HC`sg'b4
        s2HC`sg'b1 s2HC`sg'b2 s2HC`sg'b3 s2HC`sg'b4
        s3HC`sg'b1 s3HC`sg'b2 s3HC`sg'b3 s3HC`sg'b4
        sOLS`sg'b1 sOLS`sg'b2 sOLS`sg'b3 sOLS`sg'b4
        p0HC`sg'b1 p0HC`sg'b2 p0HC`sg'b3 p0HC`sg'b4
        p1HC`sg'b1 p1HC`sg'b2 p1HC`sg'b3 p1HC`sg'b4
        p2HC`sg'b1 p2HC`sg'b2 p2HC`sg'b3 p2HC`sg'b4
        p3HC`sg'b1 p3HC`sg'b2 p3HC`sg'b3 p3HC`sg'b4
        pOLS`sg'b1 pOLS`sg'b2 pOLS`sg'b3 pOLS`sg'b4
        m0 m1 m2 m3 mols
        gla05 gla10 gla20 gla30 gla40 gla50 gla60
        wha05 wha10 wha20 wha30 wha40 wha50 wha60
        s3G0`sg'b1 s3G0`sg'b2 s3G0`sg'b3 s3G0`sg'b4
        p3G0`sg'b1 p3G0`sg'b2 p3G0`sg'b3 p3G0`sg'b4
        s3G1`sg'b1 s3G1`sg'b2 s3G1`sg'b3 s3G1`sg'b4
        p3G1`sg'b1 p3G1`sg'b2 p3G1`sg'b3 p3G1`sg'b4
        s3G2`sg'b1 s3G2`sg'b2 s3G2`sg'b3 s3G2`sg'b4
        p3G2`sg'b1 p3G2`sg'b2 p3G2`sg'b3 p3G2`sg'b4
        s3G3`sg'b1 s3G3`sg'b2 s3G3`sg'b3 s3G3`sg'b4
        p3G3`sg'b1 p3G3`sg'b2 p3G3`sg'b3 p3G3`sg'b4
        s3G4`sg'b1 s3G4`sg'b2 s3G4`sg'b3 s3G4`sg'b4
        p3G4`sg'b1 p3G4`sg'b2 p3G4`sg'b3 p3G4`sg'b4
        s3G5`sg'b1 s3G5`sg'b2 s3G5`sg'b3 s3G5`sg'b4
        p3G5`sg'b1 p3G5`sg'b2 p3G5`sg'b3 p3G5`sg'b4
/*        s3G6`sg'b1 s3G6`sg'b2 s3G6`sg'b3 s3G6`sg'b4
        p3G6`sg'b1 p3G6`sg'b2 p3G6`sg'b3 p3G6`sg'b4 */
        s3W0`sg'b1 s3W0`sg'b2 s3W0`sg'b3 s3W0`sg'b4
        p3W0`sg'b1 p3W0`sg'b2 p3W0`sg'b3 p3W0`sg'b4
        s3W1`sg'b1 s3W1`sg'b2 s3W1`sg'b3 s3W1`sg'b4
        p3W1`sg'b1 p3W1`sg'b2 p3W1`sg'b3 p3W1`sg'b4
        s3W2`sg'b1 s3W2`sg'b2 s3W2`sg'b3 s3W2`sg'b4
        p3W2`sg'b1 p3W2`sg'b2 p3W2`sg'b3 p3W2`sg'b4
        s3W3`sg'b1 s3W3`sg'b2 s3W3`sg'b3 s3W3`sg'b4
        p3W3`sg'b1 p3W3`sg'b2 p3W3`sg'b3 p3W3`sg'b4
        s3W4`sg'b1 s3W4`sg'b2 s3W4`sg'b3 s3W4`sg'b4
        p3W4`sg'b1 p3W4`sg'b2 p3W4`sg'b3 p3W4`sg'b4
        s3W5`sg'b1 s3W5`sg'b2 s3W5`sg'b3 s3W5`sg'b4
        p3W5`sg'b1 p3W5`sg'b2 p3W5`sg'b3 p3W5`sg'b4
/*        s3W6`sg'b1 s3W6`sg'b2 s3W6`sg'b3 s3W6`sg'b4
        p3W6`sg'b1 p3W6`sg'b2 p3W6`sg'b3 p3W6`sg'b4 */
        using "`strucnm'GW", replace;
    #delimit cr

    local parms $parmis
    parse "`parms'",p(" ")
    local sampleN `1'
    local n `sampleN'
    if `n'<100 { local n "0`n'" }
    if `n'<1000 { local n "0`n'" }

    local i = 1
    while "`1'" ~= ""   {
        local sampleN = `1'
        local n `sampleN'
        if `n'<100 { local n "0`n'" }
        if `n'<1000 { local n "0`n'" }
        local infile `strucnm'`n'
        use "`infile'", clear
*        display " "
*        display "** Description of `infile'"
*        summarize
        hccr2a `sampleN' `i'
        macro shift
        local i = `i' + 1
     }
     postclose myres

    local resnm "`strucnm'BC"
    global resnm `resnm'
    #delimit ;
    postfile myres
        dsnumber dslabel factor niter seedis n123 sampleN R2
        s0HC`sg'b1 s0HC`sg'b2 s0HC`sg'b3 s0HC`sg'b4
        s1HC`sg'b1 s1HC`sg'b2 s1HC`sg'b3 s1HC`sg'b4
        s2HC`sg'b1 s2HC`sg'b2 s2HC`sg'b3 s2HC`sg'b4
        s3HC`sg'b1 s3HC`sg'b2 s3HC`sg'b3 s3HC`sg'b4
        sOLS`sg'b1 sOLS`sg'b2 sOLS`sg'b3 sOLS`sg'b4
        p0HC`sg'b1 p0HC`sg'b2 p0HC`sg'b3 p0HC`sg'b4
        p1HC`sg'b1 p1HC`sg'b2 p1HC`sg'b3 p1HC`sg'b4
        p2HC`sg'b1 p2HC`sg'b2 p2HC`sg'b3 p2HC`sg'b4
        p3HC`sg'b1 p3HC`sg'b2 p3HC`sg'b3 p3HC`sg'b4
        pOLS`sg'b1 pOLS`sg'b2 pOLS`sg'b3 pOLS`sg'b4
        m0 m1 m2 m3 mols
        bpa05 bpa10 bpa20 bpa30 bpa40 bpa50 bpa60
        cwa05 cwa10 cwa20 cwa30 cwa40 cwa50 cwa60
        s3B0`sg'b1 s3B0`sg'b2 s3B0`sg'b3 s3B0`sg'b4
        p3B0`sg'b1 p3B0`sg'b2 p3B0`sg'b3 p3B0`sg'b4
        s3B1`sg'b1 s3B1`sg'b2 s3B1`sg'b3 s3B1`sg'b4
        p3B1`sg'b1 p3B1`sg'b2 p3B1`sg'b3 p3B1`sg'b4
        s3B2`sg'b1 s3B2`sg'b2 s3B2`sg'b3 s3B2`sg'b4
        p3B2`sg'b1 p3B2`sg'b2 p3B2`sg'b3 p3B2`sg'b4
        s3B3`sg'b1 s3B3`sg'b2 s3B3`sg'b3 s3B3`sg'b4
        p3B3`sg'b1 p3B3`sg'b2 p3B3`sg'b3 p3B3`sg'b4
        s3B4`sg'b1 s3B4`sg'b2 s3B4`sg'b3 s3B4`sg'b4
        p3B4`sg'b1 p3B4`sg'b2 p3B4`sg'b3 p3B4`sg'b4
        s3B5`sg'b1 s3B5`sg'b2 s3B5`sg'b3 s3B5`sg'b4
        p3B5`sg'b1 p3B5`sg'b2 p3B5`sg'b3 p3B5`sg'b4
/*        s3B6`sg'b1 s3B6`sg'b2 s3B6`sg'b3 s3B6`sg'b4
        p3B6`sg'b1 p3B6`sg'b2 p3B6`sg'b3 p3B6`sg'b4 */
        s3C0`sg'b1 s3C0`sg'b2 s3C0`sg'b3 s3C0`sg'b4
        p3C0`sg'b1 p3C0`sg'b2 p3C0`sg'b3 p3C0`sg'b4
        s3C1`sg'b1 s3C1`sg'b2 s3C1`sg'b3 s3C1`sg'b4
        p3C1`sg'b1 p3C1`sg'b2 p3C1`sg'b3 p3C1`sg'b4
        s3C2`sg'b1 s3C2`sg'b2 s3C2`sg'b3 s3C2`sg'b4
        p3C2`sg'b1 p3C2`sg'b2 p3C2`sg'b3 p3C2`sg'b4
        s3C3`sg'b1 s3C3`sg'b2 s3C3`sg'b3 s3C3`sg'b4
        p3C3`sg'b1 p3C3`sg'b2 p3C3`sg'b3 p3C3`sg'b4
        s3C4`sg'b1 s3C4`sg'b2 s3C4`sg'b3 s3C4`sg'b4
        p3C4`sg'b1 p3C4`sg'b2 p3C4`sg'b3 p3C4`sg'b4
        s3C5`sg'b1 s3C5`sg'b2 s3C5`sg'b3 s3C5`sg'b4
        p3C5`sg'b1 p3C5`sg'b2 p3C5`sg'b3 p3C5`sg'b4
/*        s3C6`sg'b1 s3C6`sg'b2 s3C6`sg'b3 s3C6`sg'b4
        p3C6`sg'b1 p3C6`sg'b2 p3C6`sg'b3 p3C6`sg'b4 */
        using "`strucnm'BC", replace;
    #delimit cr
    parse "`parms'",p(" ")
    display " "
    local i = 1
    while "`1'" ~= ""   {
        local sampleN = `1'
        local n `sampleN'
        if `n'<100 { local n "0`n'" }
        if `n'<1000 { local n "0`n'" }
        local infile `strucnm'`n'
        use "`infile'", clear
*        display "** 2nd pass on file `infile'"
        hccr2b `sampleN' `i'
        macro shift
        local i = `i' + 1
    }
    postclose myres

end /* hcres2 */


*============================================
*============================================
program define hcresult
pause off
    parse "`*'",p(" ")
    if "`1'"=="" {
        di "=> hcresult <dsnum> <structurename> <sample sizes>"
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
    #delimit ;
    postfile myres
        dsnumber dslabel factor niter seedis n123 sampleN R2
        s0HC`sg'b1 s0HC`sg'b2 s0HC`sg'b3 s0HC`sg'b4
        s1HC`sg'b1 s1HC`sg'b2 s1HC`sg'b3 s1HC`sg'b4
        s2HC`sg'b1 s2HC`sg'b2 s2HC`sg'b3 s2HC`sg'b4
        s3HC`sg'b1 s3HC`sg'b2 s3HC`sg'b3 s3HC`sg'b4
        sOLS`sg'b1 sOLS`sg'b2 sOLS`sg'b3 sOLS`sg'b4
        p0HC`sg'b1 p0HC`sg'b2 p0HC`sg'b3 p0HC`sg'b4
        p1HC`sg'b1 p1HC`sg'b2 p1HC`sg'b3 p1HC`sg'b4
        p2HC`sg'b1 p2HC`sg'b2 p2HC`sg'b3 p2HC`sg'b4
        p3HC`sg'b1 p3HC`sg'b2 p3HC`sg'b3 p3HC`sg'b4
        pOLS`sg'b1 pOLS`sg'b2 pOLS`sg'b3 pOLS`sg'b4
        m0 m1 m2 m3 mols
        s0G1`sg'b1 s0G1`sg'b2 s0G1`sg'b3 s0G1`sg'b4
        s0W1`sg'b1 s0W1`sg'b2 s0W1`sg'b3 s0W1`sg'b4
        p0G1`sg'b1 p0G1`sg'b2 p0G1`sg'b3 p0G1`sg'b4
        p0W1`sg'b1 p0W1`sg'b2 p0W1`sg'b3 p0W1`sg'b4
        s0G0`sg'b1 s0G0`sg'b2 s0G0`sg'b3 s0G0`sg'b4
        s0W0`sg'b1 s0W0`sg'b2 s0W0`sg'b3 s0W0`sg'b4
        p0G0`sg'b1 p0G0`sg'b2 p0G0`sg'b3 p0G0`sg'b4
        p0W0`sg'b1 p0W0`sg'b2 p0W0`sg'b3 p0W0`sg'b4
        s1G1`sg'b1 s1G1`sg'b2 s1G1`sg'b3 s1G1`sg'b4
        s1W1`sg'b1 s1W1`sg'b2 s1W1`sg'b3 s1W1`sg'b4
        p1G1`sg'b1 p1G1`sg'b2 p1G1`sg'b3 p1G1`sg'b4
        p1W1`sg'b1 p1W1`sg'b2 p1W1`sg'b3 p1W1`sg'b4
        s1G0`sg'b1 s1G0`sg'b2 s1G0`sg'b3 s1G0`sg'b4
        s1W0`sg'b1 s1W0`sg'b2 s1W0`sg'b3 s1W0`sg'b4
        p1G0`sg'b1 p1G0`sg'b2 p1G0`sg'b3 p1G0`sg'b4
        p1W0`sg'b1 p1W0`sg'b2 p1W0`sg'b3 p1W0`sg'b4
        s2G1`sg'b1 s2G1`sg'b2 s2G1`sg'b3 s2G1`sg'b4
        s2W1`sg'b1 s2W1`sg'b2 s2W1`sg'b3 s2W1`sg'b4
        p2G1`sg'b1 p2G1`sg'b2 p2G1`sg'b3 p2G1`sg'b4
        p2W1`sg'b1 p2W1`sg'b2 p2W1`sg'b3 p2W1`sg'b4
        s2G0`sg'b1 s2G0`sg'b2 s2G0`sg'b3 s2G0`sg'b4
        s2W0`sg'b1 s2W0`sg'b2 s2W0`sg'b3 s2W0`sg'b4
        p2G0`sg'b1 p2G0`sg'b2 p2G0`sg'b3 p2G0`sg'b4
        p2W0`sg'b1 p2W0`sg'b2 p2W0`sg'b3 p2W0`sg'b4
        s3G1`sg'b1 s3G1`sg'b2 s3G1`sg'b3 s3G1`sg'b4
        s3W1`sg'b1 s3W1`sg'b2 s3W1`sg'b3 s3W1`sg'b4
        p3G1`sg'b1 p3G1`sg'b2 p3G1`sg'b3 p3G1`sg'b4
        p3W1`sg'b1 p3W1`sg'b2 p3W1`sg'b3 p3W1`sg'b4
        s3G0`sg'b1 s3G0`sg'b2 s3G0`sg'b3 s3G0`sg'b4
        s3W0`sg'b1 s3W0`sg'b2 s3W0`sg'b3 s3W0`sg'b4
        p3G0`sg'b1 p3G0`sg'b2 p3G0`sg'b3 p3G0`sg'b4
        p3W0`sg'b1 p3W0`sg'b2 p3W0`sg'b3 p3W0`sg'b4
        s0B1`sg'b1 s0B1`sg'b2 s0B1`sg'b3 s0B1`sg'b4
        s0C1`sg'b1 s0C1`sg'b2 s0C1`sg'b3 s0C1`sg'b4
        p0B1`sg'b1 p0B1`sg'b2 p0B1`sg'b3 p0B1`sg'b4
        p0C1`sg'b1 p0C1`sg'b2 p0C1`sg'b3 p0C1`sg'b4
        s0B0`sg'b1 s0B0`sg'b2 s0B0`sg'b3 s0B0`sg'b4
        s0C0`sg'b1 s0C0`sg'b2 s0C0`sg'b3 s0C0`sg'b4
        p0B0`sg'b1 p0B0`sg'b2 p0B0`sg'b3 p0B0`sg'b4
        p0C0`sg'b1 p0C0`sg'b2 p0C0`sg'b3 p0C0`sg'b4
        s1B1`sg'b1 s1B1`sg'b2 s1B1`sg'b3 s1B1`sg'b4
        s1C1`sg'b1 s1C1`sg'b2 s1C1`sg'b3 s1C1`sg'b4
        p1B1`sg'b1 p1B1`sg'b2 p1B1`sg'b3 p1B1`sg'b4
        p1C1`sg'b1 p1C1`sg'b2 p1C1`sg'b3 p1C1`sg'b4
        s1B0`sg'b1 s1B0`sg'b2 s1B0`sg'b3 s1B0`sg'b4
        s1C0`sg'b1 s1C0`sg'b2 s1C0`sg'b3 s1C0`sg'b4
        p1B0`sg'b1 p1B0`sg'b2 p1B0`sg'b3 p1B0`sg'b4
        p1C0`sg'b1 p1C0`sg'b2 p1C0`sg'b3 p1C0`sg'b4
        s2B1`sg'b1 s2B1`sg'b2 s2B1`sg'b3 s2B1`sg'b4
        s2C1`sg'b1 s2C1`sg'b2 s2C1`sg'b3 s2C1`sg'b4
        p2B1`sg'b1 p2B1`sg'b2 p2B1`sg'b3 p2B1`sg'b4
        p2C1`sg'b1 p2C1`sg'b2 p2C1`sg'b3 p2C1`sg'b4
        s2B0`sg'b1 s2B0`sg'b2 s2B0`sg'b3 s2B0`sg'b4
        s2C0`sg'b1 s2C0`sg'b2 s2C0`sg'b3 s2C0`sg'b4
        p2B0`sg'b1 p2B0`sg'b2 p2B0`sg'b3 p2B0`sg'b4
        p2C0`sg'b1 p2C0`sg'b2 p2C0`sg'b3 p2C0`sg'b4
        s3B1`sg'b1 s3B1`sg'b2 s3B1`sg'b3 s3B1`sg'b4
        s3C1`sg'b1 s3C1`sg'b2 s3C1`sg'b3 s3C1`sg'b4
        p3B1`sg'b1 p3B1`sg'b2 p3B1`sg'b3 p3B1`sg'b4
        p3C1`sg'b1 p3C1`sg'b2 p3C1`sg'b3 p3C1`sg'b4
        s3B0`sg'b1 s3B0`sg'b2 s3B0`sg'b3 s3B0`sg'b4
        s3C0`sg'b1 s3C0`sg'b2 s3C0`sg'b3 s3C0`sg'b4
        p3B0`sg'b1 p3B0`sg'b2 p3B0`sg'b3 p3B0`sg'b4
        p3C0`sg'b1 p3C0`sg'b2 p3C0`sg'b3 p3C0`sg'b4
        using "`resfile'", replace;
    #delimit cr
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
        hccr1 `sampleN' `i'
        macro shift
        local i = `i' + 1
    }
    postclose myres

* get ready to do this again with sig level .10
    local sg "10"
    global sg "`sg'"
    local resfile "`strucnm'r1`sg'"
    global resfile `resfile'
    #delimit ;

    postfile myres
        dsnumber dslabel factor niter  seedis n123 sampleN R2
        s0HC`sg'b1 s0HC`sg'b2 s0HC`sg'b3 s0HC`sg'b4
        s1HC`sg'b1 s1HC`sg'b2 s1HC`sg'b3 s1HC`sg'b4
        s2HC`sg'b1 s2HC`sg'b2 s2HC`sg'b3 s2HC`sg'b4
        s3HC`sg'b1 s3HC`sg'b2 s3HC`sg'b3 s3HC`sg'b4
        sOLS`sg'b1 sOLS`sg'b2 sOLS`sg'b3 sOLS`sg'b4
        p0HC`sg'b1 p0HC`sg'b2 p0HC`sg'b3 p0HC`sg'b4
        p1HC`sg'b1 p1HC`sg'b2 p1HC`sg'b3 p1HC`sg'b4
        p2HC`sg'b1 p2HC`sg'b2 p2HC`sg'b3 p2HC`sg'b4
        p3HC`sg'b1 p3HC`sg'b2 p3HC`sg'b3 p3HC`sg'b4
        pOLS`sg'b1 pOLS`sg'b2 pOLS`sg'b3 pOLS`sg'b4
        m0 m1 m2 m3 mols
        s0G1`sg'b1 s0G1`sg'b2 s0G1`sg'b3 s0G1`sg'b4
        s0W1`sg'b1 s0W1`sg'b2 s0W1`sg'b3 s0W1`sg'b4
        p0G1`sg'b1 p0G1`sg'b2 p0G1`sg'b3 p0G1`sg'b4
        p0W1`sg'b1 p0W1`sg'b2 p0W1`sg'b3 p0W1`sg'b4
        s0G0`sg'b1 s0G0`sg'b2 s0G0`sg'b3 s0G0`sg'b4
        s0W0`sg'b1 s0W0`sg'b2 s0W0`sg'b3 s0W0`sg'b4
        p0G0`sg'b1 p0G0`sg'b2 p0G0`sg'b3 p0G0`sg'b4
        p0W0`sg'b1 p0W0`sg'b2 p0W0`sg'b3 p0W0`sg'b4
        s1G1`sg'b1 s1G1`sg'b2 s1G1`sg'b3 s1G1`sg'b4
        s1W1`sg'b1 s1W1`sg'b2 s1W1`sg'b3 s1W1`sg'b4
        p1G1`sg'b1 p1G1`sg'b2 p1G1`sg'b3 p1G1`sg'b4
        p1W1`sg'b1 p1W1`sg'b2 p1W1`sg'b3 p1W1`sg'b4
        s1G0`sg'b1 s1G0`sg'b2 s1G0`sg'b3 s1G0`sg'b4
        s1W0`sg'b1 s1W0`sg'b2 s1W0`sg'b3 s1W0`sg'b4
        p1G0`sg'b1 p1G0`sg'b2 p1G0`sg'b3 p1G0`sg'b4
        p1W0`sg'b1 p1W0`sg'b2 p1W0`sg'b3 p1W0`sg'b4
        s2G1`sg'b1 s2G1`sg'b2 s2G1`sg'b3 s2G1`sg'b4
        s2W1`sg'b1 s2W1`sg'b2 s2W1`sg'b3 s2W1`sg'b4
        p2G1`sg'b1 p2G1`sg'b2 p2G1`sg'b3 p2G1`sg'b4
        p2W1`sg'b1 p2W1`sg'b2 p2W1`sg'b3 p2W1`sg'b4
        s2G0`sg'b1 s2G0`sg'b2 s2G0`sg'b3 s2G0`sg'b4
        s2W0`sg'b1 s2W0`sg'b2 s2W0`sg'b3 s2W0`sg'b4
        p2G0`sg'b1 p2G0`sg'b2 p2G0`sg'b3 p2G0`sg'b4
        p2W0`sg'b1 p2W0`sg'b2 p2W0`sg'b3 p2W0`sg'b4
        s3G1`sg'b1 s3G1`sg'b2 s3G1`sg'b3 s3G1`sg'b4
        s3W1`sg'b1 s3W1`sg'b2 s3W1`sg'b3 s3W1`sg'b4
        p3G1`sg'b1 p3G1`sg'b2 p3G1`sg'b3 p3G1`sg'b4
        p3W1`sg'b1 p3W1`sg'b2 p3W1`sg'b3 p3W1`sg'b4
        s3G0`sg'b1 s3G0`sg'b2 s3G0`sg'b3 s3G0`sg'b4
        s3W0`sg'b1 s3W0`sg'b2 s3W0`sg'b3 s3W0`sg'b4
        p3G0`sg'b1 p3G0`sg'b2 p3G0`sg'b3 p3G0`sg'b4
        p3W0`sg'b1 p3W0`sg'b2 p3W0`sg'b3 p3W0`sg'b4
        s0B1`sg'b1 s0B1`sg'b2 s0B1`sg'b3 s0B1`sg'b4
        s0C1`sg'b1 s0C1`sg'b2 s0C1`sg'b3 s0C1`sg'b4
        p0B1`sg'b1 p0B1`sg'b2 p0B1`sg'b3 p0B1`sg'b4
        p0C1`sg'b1 p0C1`sg'b2 p0C1`sg'b3 p0C1`sg'b4
        s0B0`sg'b1 s0B0`sg'b2 s0B0`sg'b3 s0B0`sg'b4
        s0C0`sg'b1 s0C0`sg'b2 s0C0`sg'b3 s0C0`sg'b4
        p0B0`sg'b1 p0B0`sg'b2 p0B0`sg'b3 p0B0`sg'b4
        p0C0`sg'b1 p0C0`sg'b2 p0C0`sg'b3 p0C0`sg'b4
        s1B1`sg'b1 s1B1`sg'b2 s1B1`sg'b3 s1B1`sg'b4
        s1C1`sg'b1 s1C1`sg'b2 s1C1`sg'b3 s1C1`sg'b4
        p1B1`sg'b1 p1B1`sg'b2 p1B1`sg'b3 p1B1`sg'b4
        p1C1`sg'b1 p1C1`sg'b2 p1C1`sg'b3 p1C1`sg'b4
        s1B0`sg'b1 s1B0`sg'b2 s1B0`sg'b3 s1B0`sg'b4
        s1C0`sg'b1 s1C0`sg'b2 s1C0`sg'b3 s1C0`sg'b4
        p1B0`sg'b1 p1B0`sg'b2 p1B0`sg'b3 p1B0`sg'b4
        p1C0`sg'b1 p1C0`sg'b2 p1C0`sg'b3 p1C0`sg'b4
        s2B1`sg'b1 s2B1`sg'b2 s2B1`sg'b3 s2B1`sg'b4
        s2C1`sg'b1 s2C1`sg'b2 s2C1`sg'b3 s2C1`sg'b4
        p2B1`sg'b1 p2B1`sg'b2 p2B1`sg'b3 p2B1`sg'b4
        p2C1`sg'b1 p2C1`sg'b2 p2C1`sg'b3 p2C1`sg'b4
        s2B0`sg'b1 s2B0`sg'b2 s2B0`sg'b3 s2B0`sg'b4
        s2C0`sg'b1 s2C0`sg'b2 s2C0`sg'b3 s2C0`sg'b4
        p2B0`sg'b1 p2B0`sg'b2 p2B0`sg'b3 p2B0`sg'b4
        p2C0`sg'b1 p2C0`sg'b2 p2C0`sg'b3 p2C0`sg'b4
        s3B1`sg'b1 s3B1`sg'b2 s3B1`sg'b3 s3B1`sg'b4
        s3C1`sg'b1 s3C1`sg'b2 s3C1`sg'b3 s3C1`sg'b4
        p3B1`sg'b1 p3B1`sg'b2 p3B1`sg'b3 p3B1`sg'b4
        p3C1`sg'b1 p3C1`sg'b2 p3C1`sg'b3 p3C1`sg'b4
        s3B0`sg'b1 s3B0`sg'b2 s3B0`sg'b3 s3B0`sg'b4
        s3C0`sg'b1 s3C0`sg'b2 s3C0`sg'b3 s3C0`sg'b4
        p3B0`sg'b1 p3B0`sg'b2 p3B0`sg'b3 p3B0`sg'b4
        p3C0`sg'b1 p3C0`sg'b2 p3C0`sg'b3 p3C0`sg'b4
        using "`resfile'", replace;

    #delimit cr
    parse "`parms'",p(" ")
    local i = 1
    while "`1'" ~= ""   {
        local sampleN = `1'
        local n `sampleN'
        if `n'<100 { local n "0`n'" }
        if `n'<1000 { local n "0`n'" }
        local infile `strucnm'`n'
        use "`infile'", clear
        display " "
        display "** Description of Simulation Results in `infile'"
        summarize
        hccr1 `sampleN' `i'
        macro shift
        local i = `i' + 1
    }
    postclose myres
    use $resfile,clear
    drop dsnumber dslabel factor niter seedis n123 sampleN R2 /*
      */ m0 m1 m2 m3 mols
    local resfile "`strucnm'r105"
    merge using `resfile'

    * datenum from simulation results file.
    local dt = $dt
    gen datenum = `dt'
    label var datenum "Numeric date of simulations."
    drop _merge
    local resfile "`strucnm'r1"
* save temporary file: `strucnm'r1
    save `resfile',replace
    erase `strucnm'r105.dta
    erase `strucnm'r110.dta

hcres2

    * get prior result file from passes at 05 and 10
    use `strucnm'r1,clear
    * merge in GW results
    merge using `strucnm'GW
    drop _merge
    * merge in BC resuls
    merge using `strucnm'BC
    drop _merge
    quietly compress
    save `strucnm'res,replace
    erase `strucnm'BC.dta
    erase `strucnm'GW.dta
    erase `strucnm'r1.dta
end


