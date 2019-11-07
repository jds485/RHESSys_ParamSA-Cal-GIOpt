#Script used to sample parameters for Morris SA of RHESSys streamflow-specific def parameters

library(sensitivity)

setwd("/scratch/js4yd/MorrisSA/RHESSysRuns/")

#Read in the sample data
Problem = read.csv(file = 'BaismanMorrisSamplingProblemFile_Full.csv')

#R3.5 seed - verified it's the same sample file in 5.0 and 5.3.
set.seed(8154)
MorrisSample = morris(model = function(x){1}, factors = as.character(Problem$NumberedParams), r = 40, design = list(type = "oat", levels = 100, grid.jump = 50), binf = Problem$Lower, bsup = Problem$Upper, scale = FALSE)

#Write the sample file to a csv to be processed for checks on the input values using Python RHESSys model checks
write.csv(MorrisSample$X, file = 'MorrisSamples_BeforeProcessing.csv', row.names = FALSE)

#Sampling diagnostics
#test = morris(model = function(x){1}, factors = as.character(Problem$NumberedParams), r = 40, design = list(type = "oat", levels = 100, grid.jump = 50), binf = 0, bsup = 1, scale = FALSE)
#Check visually for uniform distribution - this works when the scales are between 0 and 1
#png('50pctJumps.png', res = 200, width = 4, height = 4, units = 'in')
#hist(test$X, breaks = 100, xlab = 'X', main = '50% jumps')
#dev.off()

#test1 = morris(model = function(x){1}, factors = as.character(Problem$NumberedParams), r = 40, design = list(type = "oat", levels = 100, grid.jump = 20), binf = 0, bsup = 1, scale = FALSE)
#Check visually for uniform distribution - this works when the scales are between 0 and 1
#png('20pctJumps.png', res = 200, width = 4, height = 4, units = 'in')
#hist(test1$X, breaks = 100, xlab = 'X', main = '20% jumps')
#dev.off()

#test2 = morris(model = function(x){1}, factors = as.character(Problem$NumberedParams), r = 40, design = list(type = "oat", levels = 100, grid.jump = 80), binf = 0, bsup = 1, scale = FALSE)
#Check visually for uniform distribution - this works when the scales are between 0 and 1
#png('80pctJumps.png', res = 200, width = 4, height = 4, units = 'in')
#hist(test2$X, breaks = 100, xlab = 'X', main = '80% jumps')
#dev.off()