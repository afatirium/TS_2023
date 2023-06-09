compare_ICs_ugarchfit <- function(models_list) { 
  n <- length(models_list)
  
  for(i in 1:n) {
    ICs_ <- data.frame(t(infocriteria(get(models_list[i]))))
    ICs_$model <- models_list[i]
    if(i == 1) ICs <- ICs_ else ICs <- rbind(ICs, ICs_)
  }
  
  mins <- sapply(ICs[, 1:(ncol(ICs) - 1)], function(x) which(x == min(x)))
  
  return(list(ICs = ICs, which.min = mins))	
}
