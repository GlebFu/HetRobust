capture log close
log using 3c4sim, replace

*  3c4.do
*  data structure: het on x3 with chi-sq errors,
*  errors normal, R2=.40.
*  base data: jslHC1

ts
version 5.0
clear
set more off

    global factor = 1.00
    global simnum_ = 1000
    global plevel = .05
    global seedmc = 11020760
    use jslhc1
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 1 114 3c4 25 50 100 250 500 1000
    di "Iterations complete.
    ts

log close


