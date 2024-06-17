#Find the parameter set that contributed the MinMax Pareto solution.

library(tidyverse)
library(ggplot2)
library(scico)
library(ggpubr)
library(RColorBrewer)
library(forcats)

a = read.table('./GInolic_IDs.referenceDVO', header = TRUE)
b = read.table('./DVO_c_MOROMinMax.txt', header = TRUE)

c_flood = left_join(x = a, y = b, by = c('N', 'M', 'NumTrees', 'Flooding' = 'Flood'))
c_low = left_join(x = a, y = b, by = c('N', 'M', 'NumTrees', 'LowFlow'))

write.table(c_flood, 'MinMax_Params_Flood.txt', row.names = FALSE)
write.table(c_low, 'MinMax_Params_LowFlow.txt', row.names = FALSE)

flood = read.table('MinMax_Params_Flood.txt', header = TRUE)
low = read.table('MinMax_Params_LowFlow.txt', header = TRUE)

#Remove when all DVs = 0
flood = flood[!apply(X = flood, MARGIN = 1, FUN = function(x){ifelse(all(x[1:6] == 0),TRUE,FALSE)}),]
low = low[!apply(X = low, MARGIN = 1, FUN = function(x){ifelse(all(x[1:6] == 0),TRUE,FALSE)}),]

png('MinMax_Params_ParetoSet_Boxplots.png', res = 600, width = 6, height = 4, units = 'in')
layout(rbind(c(1,2)))
par(mar=c(4.1,4.1,1,1))
boxplot(-flood$Flooding*100 ~ flood$P, xlab = 'Flood Reduction (%)', 
        ylab = 'Parameter Set', varwidth=TRUE, horizontal = TRUE)
boxplot(-low$LowFlow*100 ~ low$P, xlab = 'Low Flow Change (%)', 
        ylab = '', varwidth=TRUE, horizontal = TRUE)
dev.off()


flood$P = as.character(flood$P)
flood$`Parameter Set` = flood$P
flood$`Parameter Set`[flood$`Parameter Set` == '1'] = '1: MAP'
flood = flood[order(flood$P),]
p1 = ggplot(data = flood, aes(-Flooding*100, -LowFlow.x*100)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = `Parameter Set`)) +
  scale_color_manual(aesthetics = 'color', 
                     values = scico(n = 9, palette = 'batlowW', end = 0.9),
                     breaks = c('1: MAP', as.character(seq(2,9,1)))) + 
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  xlab('Flood Reduction (%)') +
  ylab('Low Flow Change (%)') +
  ggtitle('Flood Parameter Sets for Min-Max Optimization')

low$P = as.character(low$P)
low$`Parameter Set` = low$P
low$`Parameter Set`[low$`Parameter Set` == '1'] = '1: MAP'
low = low[order(low$P),]
p2 = ggplot(data = low, aes(-Flooding*100, -LowFlow*100)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = `Parameter Set`)) +
  scale_color_manual(aesthetics = 'color', 
                     values = scico(n = 9, palette = 'batlowW', end = 0.9),
                     breaks = c('1: MAP', as.character(seq(2,9,1)))) + 
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  xlab('Flood Reduction (%)') +
  ylab('') +
  ggtitle('Low Flow Parameter Sets for Min-Max Optimization')


ggsave(filename = 'MinMax_Params_Pareto_batlow.png', plot = ggarrange(p1, p2, nrow = 1, ncol = 2), 
       device = 'png', width = 12, height = 6)

ggsave(filename = 'MinMax_Params_Pareto_batlow.pdf', plot = ggarrange(p1, p2, nrow = 1, ncol = 2), 
       device = 'pdf', width = 12, height = 6)

ggsave(filename = 'Fig7ab.pdf', plot = ggarrange(p1, p2, nrow = 1, ncol = 2), 
       device = 'pdf', width = 12, height = 6)


#Stacked barplots for number of trees
MaxGI = read.csv('MaxGI30m910.csv')

DVOMinMax = read.table('DVO_c_MOROMinMaxSyn.txt', header = TRUE)
DVOMORO = read.table('DVO_c_MOROSyn.txt', header = TRUE)
DVOMAP = read.table('DVO_c_MAPSyn.txt', header = TRUE)
DVOSyn = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVOSyn) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')

allocate_trees = function(DVO, MaxGI){
  DVO$TreesH9d = DVO$H9d*sum(MaxGI$MaxGI[MaxGI$Downslope == 9])/9
  DVO$TreesH9m = DVO$H9m*sum(MaxGI$MaxGI[MaxGI$Midslope == 9])/9
  DVO$TreesH9u = DVO$H9u*sum(MaxGI$MaxGI[MaxGI$Upslope == 9])/9
  DVO$TreesH10d = DVO$H10d*sum(MaxGI$MaxGI[MaxGI$Downslope == 10])/9
  DVO$TreesH10m = DVO$H10m*sum(MaxGI$MaxGI[MaxGI$Midslope == 10])/9
  DVO$TreesH10u = DVO$H10u*sum(MaxGI$MaxGI[MaxGI$Upslope == 10])/9
  
  #Sum down, mid, up
  DVO$Treesd = DVO$TreesH9d + DVO$TreesH10d
  DVO$Treesm = DVO$TreesH9m + DVO$TreesH10m
  DVO$Treesu = DVO$TreesH9u + DVO$TreesH10u
  
  return(DVO)
}

#stacked barplot
DVOMinMax = allocate_trees(DVOMinMax, MaxGI)
DVOMORO = allocate_trees(DVOMORO, MaxGI)
DVOMAP = allocate_trees(DVOMAP, MaxGI)
DVOSyn = allocate_trees(DVOSyn, MaxGI)

stack_barplot = function(DVO, breaks, filename, device, title){
  #get categorical variable for the break
  DVO$breaks = NA
  for(i in 1:length(breaks)){
    if(i == 1){
      DVO$breaks[DVO$NumTrees <= breaks[i]] = as.character(breaks[i])
    }else{
      DVO$breaks[DVO$NumTrees <= breaks[i] & DVO$NumTrees > breaks[i-1]] = as.character(breaks[i])
    }
  }
  
  #Convert the table
  DVO_long = pivot_longer(data = select(DVO, breaks, contains('Tree')) %>%
                 rename(Downslope = Treesd, Midslope = Treesm, Upslope = Treesu), 
               cols = c(Downslope, Midslope, Upslope), 
               names_to = 'Location', values_to = 'TreesLoc')
  
  #Summarize to mean values for each break
  DVO_mean = summarize(group_by(DVO_long, breaks, Location), 
                       Trees = mean(TreesLoc)) %>%
    arrange(as.numeric(breaks))
  
  p1 = ggplot(DVO_mean, aes(x = fct_inorder(breaks), y = Trees, fill = Location)) +
    geom_bar(position = 'stack', stat="identity") +
    xlab('Tree Bins') +
    ylab('Total Trees') +
    theme_classic() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0,18000) +
    scale_color_manual(aesthetics = 'fill',
                       values = c('orange1', 'forestgreen', 'purple3'),
                       breaks = c('Upslope', 'Midslope', 'Downslope'))
  
  p2 = ggplot(DVO_mean, aes(x = as.numeric(breaks), y = Trees, color = Location)) +
    geom_line() +
    xlab('Total Trees') +
    ylab('Trees in Location') +
    theme_classic() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0,7500) +
    scale_color_manual(aesthetics = 'color',
                       values = c('orange1', 'forestgreen', 'purple3'),
                       breaks = c('Upslope', 'Midslope', 'Downslope'))
  
  ggsave(p1, filename = paste0(filename,'.',device), device = device, width = 6, height = 6, units = 'in', dpi = 1200)
  ggsave(p2, filename = paste0(filename,'_line.',device), device = device, width = 6, height = 6, units = 'in', dpi = 1200)
}

breaks = seq(2000,18000,2000)
stack_barplot(DVOMinMax, breaks = breaks, device = 'png', filename = 'Barplot_MinMaxTrees',
              title = 'Min Max')
stack_barplot(DVOMORO, breaks = breaks, device = 'png', filename = 'Barplot_MOROTrees',
              title = 'MORO')
stack_barplot(DVOMAP, breaks = breaks, device = 'png', filename = 'Barplot_MAPTrees',
              title = 'MAP')
stack_barplot(DVOSyn, breaks = breaks, device = 'png', filename = 'Barplot_SynTrees',
              title = 'Synthetic')
#all together
stack_barplot(rbind(rbind(DVOMinMax,DVOMAP,DVOMORO)%>%
                      select(-N,-M,-contains('Syn')),DVOSyn), breaks = breaks, device = 'png', filename = 'Barplot_Trees',
              title = '')

stack_barplot(rbind(rbind(DVOMinMax,DVOMAP,DVOMORO)%>%
                      select(-N,-M,-contains('Syn')),DVOSyn), breaks = breaks, device = 'pdf', filename = 'Barplot_Trees',
              title = '')
