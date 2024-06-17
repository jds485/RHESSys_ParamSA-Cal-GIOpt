import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import matplotlib
import seaborn as sns

MAPset = pd.read_csv('DVO_c_MAPSyn.txt',delimiter='\t')
MOROset = pd.read_csv('DVO_c_MOROSyn.txt',delimiter='\t')
MMOset = pd.read_csv('DVO_c_MOROMinMaxSyn.txt',delimiter='\t')
cols = MOROset.columns.tolist()
OverallSet = pd.read_csv('OverallSynRefSet-MinMax.txt', delimiter='\t', header=None, 
                         names=cols)
formulations = [MAPset, MOROset, MMOset]
names = ['MAP','MORO','MinMax']

OverallSet['Formulation'] = ""
for i, formulation in enumerate(formulations):
    OverallSet = pd.merge(OverallSet, formulation, 
                          on=["FloodingSyn","LowFlowSyn","NumTreesSyn"], 
                          how='left',indicator='Exist')
    OverallSet['Formulation'].iloc[np.where(OverallSet.Exist=='both')[0]] = names[i]
    OverallSet = OverallSet.drop(columns=['Exist'])
    if(i == 1):
        OverallSet = OverallSet.drop(columns=['H9d_y', 'H9m_y', 'H9u_y', 'H10d_y',
        'H10m_y', 'H10u_y', 'Flooding_y', 'LowFlow_y', 'NumTrees_y', 'N_y',
        'M_y', 'H9d', 'H9m', 'H9u', 'H10d', 'H10m', 'H10u', 'Flooding',
        'LowFlow', 'NumTrees', 'N', 'M'])
    
OverallSet = OverallSet.drop(OverallSet.index[-1])

fig = sns.scatterplot(-OverallSet['FloodingSyn']*100, -OverallSet['LowFlowSyn']*100, 
                size=OverallSet['NumTreesSyn'], hue=OverallSet['Formulation'], 
                hue_order = ['MAP', 'MORO', 'MinMax'],
                palette=['m', 'y', 'c'])
fig.set(xlabel = 'Flood Reduction (%)', ylabel = 'Low Flow Increase (%)')
plt.savefig('ReevalCombined.png', dpi = 600)
plt.clf()

#transparent nondominated solutions
concatenated = pd.concat([MAPset.assign(Formulation='MAP', Dominated='T'), 
                          MOROset.assign(Formulation='MORO', Dominated='T'),
                          MMOset.assign(Formulation='MinMax', Dominated='T'),
                          OverallSet.assign(Dominated='F')])

fig = sns.scatterplot(-concatenated['FloodingSyn']*100, -concatenated['LowFlowSyn']*100, 
                size=concatenated['NumTreesSyn'], hue=concatenated['Formulation'], 
                hue_order = ['MAP', 'MORO', 'MinMax'],
                palette=['m', 'y', 'c'], style = concatenated['Dominated'], style_order = ['F', 'T'])
fig.set(xlabel = 'Flood Reduction (%)', ylabel = 'Low Flow Increase (%)')
plt.savefig('ReevalCombined_Dominated.png', dpi = 600)
plt.clf()