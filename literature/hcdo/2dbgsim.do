capture log close
log using 2dbgsim, replace
*  2dbg.do
*  data structure: big het function of dummy x2.
*  normal errors, R2=.4
*  Base Data: jslHC4

ts
version 5.0
clear
set more off
    global simnum_ = 1000
    global plevel = .05
    global factor = .665
    global seedmc = 11020760
    use jslhc4,clear
    set seed $seedmc
    ts
    hcmonte 4 112 2dbg 25 50 100 250 500 1000
    di "Iterations complete.
    ts

log close


