*! version 1.1.1 11/12/97 - Print fewer graphs.
*! version 1.1.0 10/10/97 - new 2step printing.
*! version 1.0.1 9/26/97 - print option and only show some 2step
*! version 1.0.0 9/26/97 - plot all of a given type

capture program drop hcpltall
program define hcpltall
    if "`1'"=="" {
        di "Syntax: hcpltall [structure] [P-to print|nothing to view]"
        exit}
    local type `1'
    local pr `2'
    pause on
    hcpltht `type'
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(12211111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }

    hcpltsz `type' HC 05
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    hcpltsz `type' HC 10
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltsz `type' 20 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
/*
    hcpltsz `type' 20 05 10
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltsz `type' 20 10 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltsz `type' 20 10 10
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
*/
    hcpltsz `type' 21 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltsz `type' 22 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltsz `type' 23 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }

    hcpltpw `type' HC 05
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    hcpltpw `type' HC 10
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltpw `type' 20 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
/*
    hcpltpw `type' 20 05 10
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltpw `type' 20 10 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltpw `type' 20 10 10
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
*/
    hcpltpw `type' 21 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltpw `type' 22 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }
    hcpltpw `type' 23 05 05
    if "`pr'"=="P" {
        gphprint, copies(1) thickness(1411111) nologo leftmargin(100)
    }
    if "`pr'"=="" {
        pause q to see next plot; BREAK to quit
    }

    pause off

end
