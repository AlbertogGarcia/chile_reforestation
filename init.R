#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Albert Garcia and Robert Heilmayr
# Project: Chile reforestation
# Date: 6/10/20
# Purpose: Creates a symlink from code directory to a directory storing project data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NOTE: Must be run with administrator permission

# Define the path to your local code directory
code_dir <- 'C:\\Users\\garci\\Documents\\chile_reforestation\\'
# code_dir <- 'D:\\dev\\chile\\chile_reforestation\\'


# Define the path to the local path of your dropbox folder 
data_dir <- 'C:\\Users\\garci\\Dropbox\\chile_reforestation\\'
# data_dir <- 'D:\\cloud\\Dropbox\\collaborations\\chile_reforestation\\'

library(R.utils)
createLink(paste0(code_dir, 'remote\\'), data_dir, overwrite = FALSE)
