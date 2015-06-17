*! version 1.1.2 5/7/1998 - fix 30 30 30 label bug
*! version 1.1.2 3/30/1998 - rearrange output
*! version 1.1.1 3/27/1998 - select which tests to use
*! version 1.1.0 3/27/1998 - use new, single result file
*! version 1.0.1 98.03.25 - Add sum of summary
*! version 1.0.0 98.03.23 - Summary of spread of residuals
* hcsum <structure ado name>
*
* Computes summaries of key statistics

capture program drop hcsum
version 5.0

program define hcsum
    if "`1'"=="" {
        di "==> hcsum <structurename> [B|C|G|W|all]"
        exit }
    local infile `1'
    local hc `2'
    if "`hc'"=="" { local hc "all" }
    * call program to construct the data structure
    di "** HCSUM for `infile'res: `hc'."
    use `infile'res,clear
    di " * Table of mean absolute deviation from nominal size."
    di " * Negative marks smallest value."

    gen x = 1
    tempvar out minz tmp
    local nstats 29
    if "`hc'"~="all" {
        local nstats 11
    }

    mat `out' = J(`nstats',6,0)
    mat colnames `out' = All Through b1 b2 b3 b4

    if "`hc'"=="all" {

        mat rownames `out' = sOLS05 s0HC05 s1HC05 s2HC05 s3HC05 /*
        */                   s3B005 s3B105 s3B205 s3B305 s3B405 /*
        */                   s3B505 s3C005 s3C105 s3C205 s3C305 /*
        */                   s3C405 s3C505 s3G005 s3G105 s3G205 /*
        */                   s3G305 s3G405 s3G505 s3W005 s3W105 /*
        */                   s3W205 s3W305 s3W405 s3W505
    }
    if "`hc'"~="all" {
        mat rownames `out' = sOLS05 s0HC05 s1HC05 s2HC05 s3HC05 /*
        */                   s3`hc'005 s3`hc'105 s3`hc'205 s3`hc'305 /*
        */                   s3`hc'405 s3`hc'505
    }

    local nloc 2

    local n 1
    while `n' < 5 {
        quietly {
            if `n'==1 { local nis 50 }
            if `n'==2 { local nis 100 }
            if `n'==3 { local nis 250 }
            if `n'==4 { local nis 1000 }
            local bi 1
            local minis 1
            local size 1
            while `bi' <= 4 {

                local sn 1
                replace x = abs(sOLS05b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                local bloc = `bi'+2
                mat `out'[`sn',`bloc'] = `vb'
                local size `vb'

                local sn 2
                replace x = abs(s0HC05b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 3
                replace x = abs(s1HC05b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 4
                replace x = abs(s2HC05b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 5
                replace x = abs(s3HC05b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

if "`hc'"=="all" {

                local sn 6
                replace x = abs(s3B005b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 7
                replace x = abs(s3B105b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 8
                replace x = abs(s3B205b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 9
                replace x = abs(s3B305b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 10
                replace x = abs(s3B405b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 11
                replace x = abs(s3B505b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 12
                replace x = abs(s3C005b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 13
                replace x = abs(s3C105b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 14
                replace x = abs(s3C205b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 15
                replace x = abs(s3C305b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 16
                replace x = abs(s3C405b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 17
                replace x = abs(s3C505b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 18
                replace x = abs(s3G005b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 19
                replace x = abs(s3G105b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }
                local sn 20
                replace x = abs(s3G205b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }
                local sn 21
                replace x = abs(s3G305b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }
                local sn 22
                replace x = abs(s3G405b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 23
                replace x = abs(s3G505b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 24
                replace x = abs(s3W005b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 25
                replace x = abs(s3W105b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 26
                replace x = abs(s3W205b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 27
                replace x = abs(s3W305b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 28
                replace x = abs(s3W405b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 29
                replace x = abs(s3W505b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }
} /* if for all */


if "`hc'"~="all" {

                local sn 6
                replace x = abs(s3`hc'005b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 7
                replace x = abs(s3`hc'105b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 8
                replace x = abs(s3`hc'205b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 9
                replace x = abs(s3`hc'305b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 10
                replace x = abs(s3`hc'405b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

                local sn 11
                replace x = abs(s3`hc'505b`bi'-.05)
                sum x if sampleN<= `nis',meanonly
                local vb = _result(3)
                mat `out'[`sn',`nloc'] = `nis'
                mat `out'[`sn',`bloc'] = `vb'
                if `vb'<`size' {
                    local minis `sn'
                    local size `vb'
                    }

} /* if for not all */

                * mark smallest with negative sign
                mat `out'[`minis',`bloc'] = -`out'[`minis',`bloc']

                local bi = `bi'+1
                local minis 1
                local size 0

            } /* bi's */
        } /* quietly */

local alli 1

        * compute average across all betas
        local stattyp 1
        while `stattyp' <= `nstats' {
            local bcol 3
            while `bcol' <= 6 {
                mat `out'[`stattyp',`alli'] = `out'[`stattyp',`alli'] /*
                  */ + abs(`out'[`stattyp',`bcol']/4)
                local bcol = `bcol' + 1
            }
            mat `out'[`stattyp',`alli'] = `out'[`stattyp',`alli']
            local stattyp = `stattyp' + 1
        }

        * find minimum average of all betas across stat types
        local minis 1
        scalar `minz' = 999
        local stattyp 1
        while `stattyp' <= `nstats' {
            if `out'[`stattyp',`alli'] < `minz' {
                scalar `minz' = `out'[`stattyp',`alli']
                local minis `stattyp'
            }
            local stattyp = `stattyp' + 1
        }
        mat `out'[`minis',`alli'] = -`out'[`minis',`alli']

        mat list `out',noheader f(%8.3f)
        local n = `n'+1
    }    /* n = ... */

end
