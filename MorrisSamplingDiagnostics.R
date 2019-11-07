#Morris Sampling Diagnostics

setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs")

#Read input file
pre = read.csv('MorrisSamples_BeforeProcessing.csv', stringsAsFactors = FALSE, header = TRUE)

#Read processed input file
post = read.csv('MorrisSamples_AfterProcessing.csv', stringsAsFactors = FALSE, header = TRUE)

#Get the rows of the initial trajectory locations. These will be used for comparing the samples generated
StartLocs = seq(1, 10880, 272)

#Plot scatterplot matrix?
plot(pre[StartLocs, 1:5])
hist(pre$h_gw_loss_coeff, breaks = 100)

#Correlation matrix
Cpre = cor(pre[StartLocs, ])
heatmap(x = Cpre, symm = TRUE)
Cpost = cor(post[StartLocs, ])
heatmap(x = Cpost, symm = TRUE)

Cpre_full = cor(pre)
heatmap(x = Cpre_full, symm = TRUE, Rowv = NA, Colv = NA, revC = TRUE, col = rainbow(10))
Cpost_full = cor(post)
heatmap(x = Cpost_full, symm = TRUE, Rowv = NA, Colv = NA, revC = TRUE, col = rainbow(10))

#Histogram of the correlation matrix
hist(Cpre, breaks=100)
hist(Cpost, breaks=100)

png('PrePostProcessingSamplingCorrelations.png', res = 300, units = 'in', width = 8, height = 4)
layout(rbind(c(1,2)))
hist(Cpre_full, breaks=1000, main = 'Before Processing \n for Parameter Constraints', xlab = 'Correlation Values', cex.axis = 1.5, cex.lab = 1.5, xlim = c(-1,1), freq = FALSE)
hist(Cpost_full, breaks=1000, main = 'After Processing \n for Parameter Constraints', xlab = 'Correlation Values', cex.axis = 1.5, cex.lab = 1.5, xlim = c(-1,1), freq = FALSE)
dev.off()