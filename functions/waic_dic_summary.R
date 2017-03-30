getPenDic <- function(x) sum(x$deviance) + sum(x$penalty)
modelDic <- function(z) (sapply(z, getPenDic))
summaryDic <- function(y) sapply(y, modelDic)
findMinModel <- function(x) which.min(x)
bestModel <- function(x){
  x$best_model <- apply(x, FUN= findMinModel, MARGIN = 1 )
  return(x)
}


mean_waic_model <- function(x) (sapply(x, function(x) x$waic))
waic_summary <- function(x) sapply(x, mean_waic_model)
getP_waic_model <- function(x) sapply(x, function(x) x$p_waic)
getP_waic <- function(x) sapply(x, getP_waic_model)

