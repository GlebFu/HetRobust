capture log close
log using 2dsmsim, replace

* 2dsm.do
* data structure: small het function of dummy x2.
* normal errors, R2=.4
* Base data: jslHC4

ts
version 5.0
clear
set more off

    global simnum_ = 1000
    global plevel = .05
    global seedmc = 11020760
    use jslhc4
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 4 113 2dsm 25 50 100 250 500 1000
    di "Iterations complete.
    ts
    set more off

log close


