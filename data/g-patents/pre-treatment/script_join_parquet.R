library(parallel)
library(here)
i_am("Cloud SDK.Rproj")
library(arrow)
library(tidyverse)
library(data.table)

#Cited patents -----------------------------------------------------------------
no_cores <- detectCores()-2
cl <- makeCluster(no_cores)

files <- list.files(path = here("small_citation"), 
                    full.names = TRUE)
data_list <- parLapply(cl, files, read_parquet)
stopCluster(cl)

all_citations <- bind_rows(data_list)

#Write to feather file
write_feather(all_citations, here("r-exports", "citations.feather"))

#Patents filled in 2011 (B1, B2, US Only) --------------------------------------
no_cores <- detectCores()-2
cl <- makeCluster(no_cores)

files <- list.files(path = here("2011_kindB"), 
                    full.names = TRUE)
data_list <- parLapply(cl, files, read_parquet)
stopCluster(cl)

all_USpatents11 <- bind_rows(data_list)

#Write to feather file
write_feather(all_USpatents11, here("r-exports", "us_patents2011.feather"))


#All patents per assignee ------------------------------------------------------
#If patents are older in other jurisdictions sometime data on the assignee 
#origin is not available, we can retrive some of that with this "backup"

no_cores <- detectCores()-2
cl <- makeCluster(no_cores)

files <- list.files(path = here("backup_country"), 
                    full.names = TRUE)
data_list <- parLapply(cl, files, read_parquet)
stopCluster(cl)

all_patents_assignee <- bind_rows(data_list)

#Write to feather file
write_feather(all_patents_assignee, here("r-exports", "all_patents_assignee.feather"))

#Compute max backup ------------------------------------------------------------
all_patents_assignee <- setDT(read_feather(here("r-exports", "all_patents_assignee.feather")))
frequency_table <- all_patents_assignee[, .N, by = .(assignee_name, assignee_country_backup)]
setorder(frequency_table, assignee_name, -N)
most_frequent_country <- frequency_table[, .SD[1], by = assignee_name]
write_feather(most_frequent_country, here("r-exports", "backup_country.feather"))



