capture log close
log using 3nzsim, replace

*  3nn.do
*  data structure: het on x3,
*  errors normal, R2=.80.
*  base data: jslHC1

ts
version 5.0
clear
set more off

    global simnum_ = 1000
    global factor = .97
    global plevel = .05
    global seedmc = 11020760
    use jslhc1
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 1 109 3nz 25 50 100 250 500 1000
    di "Iterations complete.
    ts

log close


