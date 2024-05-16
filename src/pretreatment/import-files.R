#Import libraries ----
library(parallel)
library(here)
i_am("Cloud SDK.Rproj")
library(arrow)
library(tidyverse)
library(data.table)

#Define custom import function ----
parq_import <- function(fold){
  #get all folders 
  files <- list.files(path = here("rpatentmap-12",fold), 
                      full.names = TRUE)
  #import files in parralel
  no_cores <- detectCores()-2
  cl <- makeCluster(no_cores)
  data_list <- parLapply(cl, files, read_parquet)
  stopCluster(cl)
  #create a binded_df
  binded_df <- rbindlist(data_list)
  #write it to disk as .feather
  write_feather(binded_df, paste0(here("r-exports", fold), ".feather")) 
}

#Run custom import function ----
for(fold in dir("rpatentmap-12")){
  print(paste0("importing ",fold))
  parq_import(fold)
}