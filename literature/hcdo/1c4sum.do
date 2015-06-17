capture log close
log using 1c4sum, replace
ts
set more off
    hcsum 1c4 B
    hcsum 1c4 C
    hcsum 1c4 G
    hcsum 1c4 W
    hcsum 1c4 all
set more on
log close
