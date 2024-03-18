These files are for processing, analyzing and displaying results associated with the manuscript
Global arthropod beta-diversity is spatially and temporally structured by latitude
DOI: in review
Citation: in review

Data for running the scripts and analysis can be found in the supplement of the manuscript 
Link: in reivew

script 00.GenerateCounttables.R
This script generate count tables for each site in the study with the final output being a list of count tables exported as an Rdata file

Script S1_Data_management_GMPT_v4.R 
This script requies the output from the previous script to run and primarily creates a dataframe with all pairwise combinations of sampling events in the study with the final output another Rdata file

Script S2_Permutation_model_fitting_GMTP_v6.R
This script requires the previous Rdata files to run and is the primary data analysis file consisting of a series of statistical test using a permutation framework

Scripts leading with S3 are figure drawing files with the associated figure indicated in the file title corresponding to the figure number in the associated manuscrpt and supplmentary file
Rdata files from the 00, S1 and S2 scripts are needed for different figures

