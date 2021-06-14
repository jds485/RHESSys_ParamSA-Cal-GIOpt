#Script to generate delta, lCR, and CR values based on pCR values

set.seed(1)

generateCRvalues <- function(pCR,settings, Npop){
  
  # Random vector, add zero to get first position
  #Note that the last element is always equal to Npop*settings$updateInterval, and is never possible to select in the for loop
  # because the nCR will never be equal to the length of the RandomVec. This is okay - all values in cand may be selected.
  RandomVec <- c(0,cumsum(as.numeric(rmultinom(1, size = Npop*settings$updateInterval, prob = pCR))))
  
  # get candidate points
  cand <- sample(Npop*settings$updateInterval)
  CR <- rep(NA, Npop*settings$updateInterval)
  
  ## Now loop over chains to generate CR values
  for(i in 1:settings$nCR){
    #Start and End
    #Start can be greater than the length of cand. But that's okay if it's desired to replace the last element in RandomVec.
    Start <- RandomVec[i]+1
    End <- RandomVec[i+1]
    
    # get candidates
    candx <- cand[Start:End]
    
    # Assign these indices settings$CR
    CR[candx] <- i/settings$nCR
  }
  ## Reshape CR
  CR <- matrix(CR,Npop,settings$updateInterval)
  
  return(CR)
}

#Load previous run data
load('CurrentChain.RData')

#Generate lCR values. With 100 chain steps and a pCR update every 3 chain steps, lCR should sum to 1000.
# assuming on average 100 counts are needed to forget the initially equal pCR probabilites.
# Not correcting for rounding error possibility here because this is already approximate.
lCR = round(700*temp$pCR+100,0)
  
#Generate delta from delta = pCR*lCR
delta = temp$pCR*lCR

#Generate CR values
CRmat = generateCRvalues(pCR = temp$pCR, settings = temp$settings, Npop = length(temp$chain))

#Write values
write.csv(lCR, 'lCR.csv', row.names=FALSE)
write.csv(delta, 'delta.csv', row.names=FALSE)
write.csv(CRmat, 'CR.csv', row.names=FALSE)