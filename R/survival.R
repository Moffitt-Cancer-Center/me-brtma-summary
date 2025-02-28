surv_median <- function(sf) {
  survival:::survmean(sf, rmean="none")$matrix
}

