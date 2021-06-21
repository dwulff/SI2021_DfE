# ---- create problems

safes = rnorm(200, 3.35, 2) / 100
risks_m = rnorm(200, 8.92, 3) / 100
risks_s = rnorm(200, 15.89, 6) / 100

d = (risks_m - safes) / risks_s

sel = d > .1 & d < 1 & safes > 0

probs = cbind(risks_m[sel][1:100], risks_s[sel][1:100],safes[sel][1:100], 0)
probs = t(probs)

heads = c('data_risky_percentage=','data_risky_deviation=',
          'data_safe_percentage=','data_safe_deviation=')

# writeLines(paste0(heads,'"',apply(round(probs,3),1,paste0,collapse=','),'"'),
#            file('/Users/dwulff/Dropbox (2.0)/Work/Teaching/Workshops/EADM_SS2018/_sessions/RiskTool/risktool_problems.csv'))
# 

# ---- transform problems

a = readLines('~/Dropbox (2.0)/Work/Teaching/Workshops/EADM_SS2018/_sessions/RiskTool/risktool_problems.csv')

probs_long = do.call(cbind,lapply(a, function(x) as.numeric(unlist(stringr::str_extract_all(x,'[:digit:]+\\.*[:digit:]*')))))

probs_long = tibble::as_tibble(probs_long)

names(probs_long) = c('risky_m','risky_sd','safe_m','safe_sd')

readr::write_csv(probs_long,'~/Dropbox (2.0)/Work/Teaching/Workshops/EADM_SS2018/_sessions/RiskTool/problems_long.csv')
