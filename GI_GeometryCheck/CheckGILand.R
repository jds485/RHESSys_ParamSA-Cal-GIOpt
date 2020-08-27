a = vector('numeric', length = 100)
for(i in 1:100){
  f = read.csv(paste0('/sfs/lustre/bahamut/scratch/js4yd/GI_RandomSeedEval_Up/RHESSysRuns/Run', i, '/RHESSys_Baisman30m_g74/lulcFrac30m_GI.csv'), stringsAsFactors=FALSE)
  
  a[i] = sum(f$lulc13)
}

if(length(unique(a)) > 1){
  print('Some GI land cover areas are different across GI arrangements')
}
