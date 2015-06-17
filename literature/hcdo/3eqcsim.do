capture log close
log using eq3c4, replace
*! version 1.0.1  11/23/97 - eliminate problem with sqrt(neg)
*! version 1.0.0  11/15/97
*  eq3c4.do
*  Het on x3, Chi-square errors, R2=.4
*  Base data: HC3 (standardized x1,x2,x3,x4)
ts
version 5.0
clear
set more off

    global simnum_ = 1000
    global factor = 1.25
    global plevel = .05
    global seedmc = 11020760
    use jslhc3
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 3 113 3eqc 25 50 100 250 500 1000
    di "Iterations complete.
    ts
    set more off
    hcresult 3 3eqc 25 50 100 250 500 1000

log close


