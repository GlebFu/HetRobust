capture log close
log using N4sum, replace
ts
    hcsum n4 B
    hcsum n4 C
    hcsum n4 G
    hcsum n4 W
    hcsum n4 all
log close
