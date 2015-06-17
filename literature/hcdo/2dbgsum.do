capture log close
log using 2dbgsum, replace
ts
set more off
hcsum 2dbg B
hcsum 2dbg C
hcsum 2dbg G
hcsum 2dbg W
hcsum 2dbg all
set more on
log close
