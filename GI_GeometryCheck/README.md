# Readme for the GI_Geometry Check Directory
This directory contains code that was used to compute the maximum possible area that green infrastructure (GI) could occupy in each of the RHESSys model grid cells (patches). Constraints like distance to power lines and roads were considered, as well as the area required to plant the selected GI. Random seed tests were used to see if the selected decision variables, proportion to reforest in each hillslope's downslope, midslope, and upslope area, would be useful.

# Files
1. CalcMaxGI.R: Script to calculate how much green infrastructure can be allocated to each of the RHESSys model patches.
2. AllocateGI....R: Scripts to allocate green infrastructure according to the specified decision variables (proportion of hillslope area). 910 in the filename indicates the script is only for hillslopes 9 and 10. All other scripts were run for all hillslopes in Baisman Run.
3. RunArray...##.sh: Scripts used to run GIS2RHESSys and RHESSys to obtain streamflows for 100 random allocations of green infrastructure to ## percent of the possible hillslope areas. These percentages are entered into the text file where the AllocateGI script is called.
4. CheckGILand.R: Script to check that the land cover column totals are the same for each of the 100 replicates.
5. CompareGIStreamflows.R, CompareGIStreamflows_h.R: Scripts to compare the computed streamflows for the basin and hillslopes (\_h).
6. RunCompareStreamflows##.sh, RunCompareStreamflows##\_h.sh: Runs the CompareGIStreamflows.R and CompareGIStreamflows_h.R scripts
7. PlotGIResults.R: Script to plot the results of the random seed study.
8. PlotGIResults_testErrorDist.R: Script to check that the synthetically generated error is working properly.
