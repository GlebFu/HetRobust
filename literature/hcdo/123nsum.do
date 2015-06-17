capture log close
log using 123nsum, replace
ts
set more off
    hcsum 123n B
    hcsum 123n C
    hcsum 123n G
    hcsum 123n W
    hcsum 123n all
set more on
log close
