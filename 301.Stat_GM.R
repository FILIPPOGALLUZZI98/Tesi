ols_v <- fixest::feols(data=events_sum, log(1+all_confl)~value|region + year)
