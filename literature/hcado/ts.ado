* ts.ado - time stamp

capture program drop ts
program define ts
    di "** $S_DATE - $S_TIME"
end
