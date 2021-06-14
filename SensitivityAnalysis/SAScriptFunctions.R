#Helper functions for RHESSys SA

#Function that aggregates elementary effects based on the supplied FUN function.
AggregateEEs = function(EEs, #The elementary effect matrix
                        ColNames, #Matrix or vector containing the names of the columns. One row per item to have FUN applied
                        FUN #The function to use to aggregate the columns (e.g., mean, max)
)
{
  EEmat = matrix(NA, nrow = nrow(ColNames), ncol = nrow(EEs))
  for (i in 1:nrow(EEmat)){
    EEmat[i,] = apply(X = EEs[, colnames(EEs) %in% ColNames[i,]], MARGIN = 1, FUN = FUN)
  }
  return(t(EEmat))
}

#Mean absolute value function
meanabs = function(x){
  mean(abs(x))
}

#Plotting offset function
offset = function(x){
  if ((x>=0.3) & (x < 0.8)){
    x = x + (x - 0.3)*0.13
  }else if (x >= 0.8) {
    x = x + (x - 0.3)*0.08
  }
  return(x)
}

#Squared norm (L2) of a vector
norm_vec <- function(x){
  sqrt(sum(x^2))
}

#Cluster helper function
# function to compute coefficient
ac <- function(x) {
  agnes(t(df), method = x)$ac
}
