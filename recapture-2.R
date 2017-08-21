rm(list= ls())
load("data/Rdata/re_capture_list.Rdata")

epi10_hpd <- lapply(re_capture_list$epi10, function(x){x$int_hpd})
hpd10 <- lapply(re_capture_list$hpd10, as.data.frame)

checkOverlap <- function(low1, hi1, low2, hi2) {
  hi1 < low2 | low1 > hi2
}

# Check overlap for each of the 10 realizations
check <- lapply(1:10, function(i){
  
  checkOverlap(low1 = epi10_hpd[[i]]$lower, hi1 = epi10_hpd[[i]]$upper,
               low2 = hpd10[[i]]$lower, hi2 = hpd10[[i]]$upper)
})

# Summarize results
x <- do.call(rbind.data.frame, check)
colnames(x) <- rownames(epi10_hpd[[1]])

# Check which parameters were not captured in all 10 iterations
failures <- data.frame(colSums(x))
