capture log close
log using 123csum, replace
ts
set more off
    hcsum 123c B
    hcsum 123c C
    hcsum 123c G
    hcsum 123c W
    hcsum 123c all
set more on
log close
