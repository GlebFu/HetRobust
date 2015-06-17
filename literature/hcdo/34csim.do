capture log close
log using 34c4, replace

* 34c4.do: 3/19/98
* data structure: het function of x3 & x4
* chi square errors, R2=.4
* Base data: jslHC1

ts
version 5.0
clear
set more off

    global simnum_ = 1000
    global factor = .375
    global plevel = .05
    global seedmc = 11020760
    use jslhc1
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 1 103 34c 25 50 100 250 500 1000 
    di "Iterations complete.
    ts
    set more off
    hcresult 1 34c 25 50 100 250 500 1000 

log close


