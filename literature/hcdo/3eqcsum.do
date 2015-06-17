capture log close
log using 3eqcsum, replace
ts
set more off
hcsum 3eqc B
hcsum 3eqc C
hcsum 3eqc G
hcsum 3eqc W
hcsum 3eqc all
set more on
log close
