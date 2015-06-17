capture log close
log using t4, replace
* t4.do - data structure: t errors, R2=.40.
* Replication by jsl: 98.03.18
ts
version 5.0
clear
set more off
    global simnum_ = 1000
    global factor = 1.62
    global factor = 1.00
    global plevel = .05
    global seedmc = 11020760
    use jslhc1
    set seed $seedmc
    di "Iterations beginning."
    ts
    hcmonte 1 5 t4 25 50 100 250 500 1000
    di "Iterations complete.
    ts
    hcresult 1 t4 25 50 100 250 500 1000

log close
