*! version 1.0.0 98.03.26 - hcplt

capture program drop hcplt
program define hcplt
*hcplt c4 05 10 maxp
    if "`1'"=="" {
di "=> hcplt [structure][balpha: 05|10][scralpha: 05|10][[maxp][P-to print]"
        exit}
    local struct `1'
    local balph `2'
    local scralph `3'
    local maxp `4'
    local printit `5'
    if "`maxp'"=="" {
        local maxp ".15"
    }
    if "`maxp'"==".10" { local maxp10 ".15" }
    if "`maxp'"==".15" { local maxp10 ".20" }
    if "`maxp'"==".20" { local maxp10 ".25" }
    if "`maxp'"==".25" { local maxp10 ".30" }
    pause on

* Het tests
    hcpltht `struct'
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(12211111) nologo leftmargin(100)
    }
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }

* 05 Size of HC tests w/o screening
    hcpltsz `struct' 05 `maxp' HC `scralph'
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }

* 10 Size of HC tests w/o screening
    if "`balph'"=="10" {
        hcpltsz `struct' 10 `maxp10' HC `scralph'
        if "`printit'"=="" {
            pause q for next plot; BREAK to quit.
        }
        if "`printit'"=="P" {
            gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
        }
    }

* 05 Power of HC test
    hcpltpw `struct' 05 HC
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
* 10 power of HC test
    if "`balph'"=="10" {
        hcpltpw `struct' 10 HC
        if "`printit'"=="P" {
            gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
        }
        if "`printit'"=="" {
            pause q for next plot; BREAK to quit.
        }
    }

********************************************************
* Different het tests at .1, .3 and .5 screening values
/*
    hcpltsz2 `struct' `maxp' all 10
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
*/

    hcpltsz2 `struct' `maxp' all 30
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }


/*
    hcpltsz2 `struct' `maxp' all 50
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
*/

************************************
* Vary screen size, fix hc test
    hcpltsz2 `struct' `maxp' B all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }

    hcpltsz2 `struct' `maxp' C all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }

    hcpltsz2 `struct' `maxp' G all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }

    hcpltsz2 `struct' `maxp' W all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }

********************************************************
* Different het tests at .1, .3 and .5 screening values
/*
    hcpltpw2 `struct' all 10
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
*/
    hcpltpw2 `struct' all 30
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
/*
    hcpltpw2 `struct' all 50
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
*/


************************************
* Vary screen size, fix hc test
    hcpltpw2 `struct' B all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    hcpltpw2 `struct' C all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    hcpltpw2 `struct' G all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    hcpltpw2 `struct' W all
    if "`printit'"=="" {
        pause q for next plot; BREAK to quit.
    }
    if "`printit'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    pause off


end
