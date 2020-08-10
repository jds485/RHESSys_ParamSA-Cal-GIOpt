#Script to allocate Green Infrastructure to grid cells

#Fixme: read in an externally generated file?
#Input from Borg is what fraction of each hillslope+location(up, mid, downslope) to change to GI
HillPct = vector('numeric', length = 21)

library(sp)
library(rgdal)

#Read in the grid cell centers
Cells = 

#Read in   
#Read in the original aggregated 30 m res LULC file
lc30 = 
#Read in the original 1 m LULC file 
lc1 = 

#Round up to the nearest whole number

#Check that the sum of each row = total area (900)
  
#Output the lulcFrac30m.csv file with the updated GI changes
write.csv(lc30_e, file = 'lulcFrac30m.csv', row.names = FALSE)