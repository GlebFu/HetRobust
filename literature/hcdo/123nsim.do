capture log close
log using 123n, replace

*  123n4.do: 3/19/98
*  data structure: het function of x1,x2 & x3,
*    normal errors, R2=.4
* Base data: jslHC1

ts
version 5.0
clear
set more off

    global simnum_ = 1000
    global factor = .37
    global plevel = .05
    global seedmc = 11020760
    use jslhc1
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 1 102 123n 25 50 100 250 500 1000
    di "Iterations complete.
    ts
    set more off
    hcresult 1 123n 25 50 100 250 500 1000

log close


