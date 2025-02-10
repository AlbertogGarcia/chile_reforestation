#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Albert Garcia and Robert Heilmayr
# Project: Chile reforestation
# Date: 8/1/23
# Purpose: Creates a symlink from code directory to a directory storing project data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(here)
library(R.utils)

# NOTE: Must be run with administrator permission

# Define the path to your local code directory
# code_dir <- 'C:\\Users\\garci\\Documents\\chile_reforestation\\'
code_dir <- here::here()
# code_dir <- 'D:\\dev\\chile\\chile_reforestation\\'


# Define the path to the local path of your dropbox folder 
data_dir <- 'C:\\Users\\AG2964\\Dropbox\\chile_reforestation\\'
# data_dir <- 'D:\\cloud\\Dropbox\\collaborations\\chile_reforestation\\'


createLink(paste0(code_dir, '\\remote\\'), data_dir, overwrite = FALSE)

my_data_dir <- here::here("remote")

# Alternatively, the code is written such that you could also specify the full path:

# my_data_dir <- 'C:/Users/AG2964/Dropbox/chile_reforestation/'
