*! version 1.0.0 6/2/1998 - short summary for spreadsheet
* hcsum3 <structure ado name> [B|C|G|W|all|none] # [0|1|2]
*
* Create summaries of deviation from the nominal size.
* Summary has mean absolute deviation across all betas.
*
capture program drop hcsum3
version 5.0

program define hcsum3

* infile: error structure
      local infile `1'
* scrn: type of hc test for screen [B G W C all none]
      local scrn `2'
      if "`scrn'"=="" { local scrn "all" }
* xdist:
*   last digit: 0=n; 1=c; 2=t; 3=eq&n; 4=dbg; 5=dsm; 6=eqc
*   prior digit is # of variables associated with the hetero
        local xdist =`3'/10000
/*  Commands to process all data:
hcsum3 c4    none    1
hcsum3 n4    none    0
hcsum3 neq4  none    3
hcsum3 t4    none    2
hcsum3 123c  none 1231
hcsum3 123n  none 1230
hcsum3 1c4   none   11
hcsum3 1n4   none   10
hcsum3 2dbg  none   24
hcsum3 2dsm  none   25
hcsum3 34c   none  341
hcsum3 34n   none  340
hcsum3 3c4   none   31
hcsum3 3eqc  none   36
hcsum3 3nn   none   30
*/

* get data
    use `infile'res,clear
    gen x = 1
    tempvar out minz tmp

* set up output matrix; decide on number of stats to process.
        local nstats 29
        if "`scrn'"~="all" {
            local nstats 11
        }
        if "`scrn'"=="none" {
            local nstats 5
        }

    mat `out' = J(`nstats',6,0)
    mat colnames `out' = N= b1 b2 b3 b4 Dist
    if "`scrn'"=="all" {
        mat rownames `out' = sOLS05 s0HC05 s1HC05 s2HC05 s3HC05 /*
        */                   s3B005 s3B105 s3B205 s3B305 s3B405 /*
        */                   s3B505 s3C005 s3C105 s3C205 s3C305 /*
        */                   s3C405 s3C505 s3G005 s3G105 s3G205 /*
        */                   s3G305 s3G405 s3G505 s3W005 s3W105 /*
        */                   s3W205 s3W305 s3W405 s3W505
    }
    if "`scrn'"~="all" & "`scrn'"~="none" {
        mat rownames `out' = sOLS05 s0HC05 s1HC05 s2HC05 s3HC05 /*
        */ s3`scrn'005 s3`scrn'105 s3`scrn'205 s3`scrn'305 /*
        */ s3`scrn'405 s3`scrn'505
    }
    if "`scrn'"=="none" {
        mat rownames `out' = sOLS05 s0HC05 s1HC05 s2HC05 s3HC05
    }

* loop over sample size
    local n 0
    while `n' < 6 {

        quietly {

            * nis is actual n; nix is 999 for 1000
            if `n'==2 {
                local nis 100
                local nix 9100
                }
            if `n'==3 {
                local nis 250
                local nix 9250
                }
            if `n'==4 {
                local nis 500
                local nix 9500
                }
            if `n'==0 {
                local nis 25
                local nix 9025
                }
            if `n'==1 {
                local nis 50
                local nix 9050
                }
            if `n'==5 {
                local nis 1000
                local nix 1000
                }

            local minis 1
            local size 1

            * loop over b's
            local bi 1
            while `bi' <= 4 {
                * row of out for current data
                local outrow 1
* OLS
                * col 2: n for this set of results
                mat `out'[`outrow',1] = `nix'
                * col 7: distribution
                mat `out'[`outrow',6] = `xdist'
                * x is deviation from nominal level
                replace x = (sOLS05b`bi'-.05)
                * compute mean for current n (not n<= some value)
                sum x if sampleN==`nis',meanonly
                local vb = _result(3)
                * col `bloc': data for `bi'
                local bloc = `bi'+1
                mat `out'[`outrow',`bloc'] = `vb'
                local size `vb'
* HC0
                local outrow 2
                mat `out'[`outrow',6] = `xdist'
                replace x = (s0HC05b`bi'-.05)
                sum x if sampleN==`nis',meanonly
                local vb = _result(3)
                mat `out'[`outrow',1] = `nix'
                mat `out'[`outrow',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `outrow'
                    local size `vb'
                    }
* HC1
                local outrow 3
                mat `out'[`outrow',6] = `xdist'
                replace x = (s1HC05b`bi'-.05)
                sum x if sampleN==`nis',meanonly
                local vb = _result(3)
                mat `out'[`outrow',1] = `nix'
                mat `out'[`outrow',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `outrow'
                    local size `vb'
                    }
* HC2
                local outrow 4
                mat `out'[`outrow',6] = `xdist'
                replace x = (s2HC05b`bi'-.05)
                sum x if sampleN==`nis',meanonly
                local vb = _result(3)
                mat `out'[`outrow',1] = `nix'
                mat `out'[`outrow',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `outrow'
                    local size `vb'
                    }
* HC3
                local outrow 5
                mat `out'[`outrow',6] = `xdist'
                replace x = (s3HC05b`bi'-.05)
                sum x if sampleN==`nis',meanonly
                local vb = _result(3)
                mat `out'[`outrow',1] = `nix'
                mat `out'[`outrow',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `outrow'
                    local size `vb'
                    }

if "`scrn'"~="none" {
                if "`scrn'"~="all" {
* 3`scrn'05
                    local outrow 6
                    mat `out'[`outrow',6] = `xdist'
                    replace x = (s3`scrn'005b`bi'-.05)
                    sum x if sampleN==`nis',meanonly
                    local vb = _result(3)
                    mat `out'[`outrow',1] = `nix'
                    mat `out'[`outrow',`bloc'] = `vb'
                    if `vb'<`size' {
                        local minis `outrow'
                        local size `vb'
                        }
* 3`scrn'10
                    local outrow 7
                    mat `out'[`outrow',6] = `xdist'
                    replace x = (s3`scrn'105b`bi'-.05)
                    sum x if sampleN==`nis',meanonly
                    local vb = _result(3)
                    mat `out'[`outrow',1] = `nix'
                    mat `out'[`outrow',`bloc'] = `vb'
                    if `vb'<`size' {
                        local minis `outrow'
                        local size `vb'
                        }
* 3`scrn'20
                    local outrow 8
                    mat `out'[`outrow',6] = `xdist'
                    replace x = (s3`scrn'205b`bi'-.05)
                    sum x if sampleN==`nis',meanonly
                    local vb = _result(3)
                    mat `out'[`outrow',1] = `nix'
                    mat `out'[`outrow',`bloc'] = `vb'
                    if `vb'<`size' {
                        local minis `outrow'
                        local size `vb'
                        }
* 3`scrn'30
                    local outrow 9
                    mat `out'[`outrow',6] = `xdist'
                    replace x = (s3`scrn'305b`bi'-.05)
                    sum x if sampleN==`nis',meanonly
                    local vb = _result(3)
                    mat `out'[`outrow',1] = `nix'
                    mat `out'[`outrow',`bloc'] = `vb'
                    if `vb'<`size' {
                        local minis `outrow'
                        local size `vb'
                        }
* 3`scrn'40
                    local outrow 10
                    mat `out'[`outrow',6] = `xdist'
                    replace x = (s3`scrn'405b`bi'-.05)
                    sum x if sampleN==`nis',meanonly
                    local vb = _result(3)
                    mat `out'[`outrow',1] = `nix'
                    mat `out'[`outrow',`bloc'] = `vb'
                    if `vb'<`size' {
                        local minis `outrow'
                        local size `vb'
                        }
* 3`scrn'50
                    local outrow 11
                    mat `out'[`outrow',6] = `xdist'
                    replace x = (s3`scrn'505b`bi'-.05)
                    sum x if sampleN==`nis',meanonly
                    local vb = _result(3)
                    mat `out'[`outrow',1] = `nix'
                    mat `out'[`outrow',`bloc'] = `vb'
                    if `vb'<`size' {
                        local minis `outrow'
                        local size `vb'
                        }
                } /* end: if "`scrn'"~="all" */
} /* end: if "`scrn'"~="none" */

/* mark smallest with negative sign
                mat `out'[`minis',`bloc'] = -`out'[`minis',`bloc']
*/
                * go to next beta
                local bi = `bi'+1
                local minis 1
                local size 0
            }   /* end: while `bi' <= 4 */
        }       /* end: quietly */

/*
        * compute average across all betas
        local stattyp 1
        while `stattyp' <= `nstats' {
            mat `out'[`stattyp',1] = 0
            local bcol 3
            while `bcol' <= 6 {
                mat `out'[`stattyp',1] = `out'[`stattyp',1] /*
                  */ + abs(`out'[`stattyp',`bcol']/4)
                local bcol = `bcol' + 1
            }

            local stattyp = `stattyp' + 1
        }
*/
/*
        * find minimum average of all betas across stat types
        local minis 1
        scalar `minz' = 999
        local stattyp 1
        while `stattyp' <= `nstats' {
            if `out'[`stattyp',1] < `minz' {
                scalar `minz' = `out'[`stattyp',1]
                local minis `stattyp'
            }
            local stattyp = `stattyp' + 1
        }
        mat `out'[`minis',1] = -`out'[`minis',1]
*/
        mat list `out',noheader f(%10.4f)
        local n = `n'+1
    } /* end: loop over sample size */

end
