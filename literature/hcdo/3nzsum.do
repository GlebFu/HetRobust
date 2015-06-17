capture log close
log using 3nzsum, replace
ts
set more off
hcsum 3nz B
hcsum 3nz C
hcsum 3nz G
hcsum 3nz W
hcsum 3nz all
set more on
log close
