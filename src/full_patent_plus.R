library(here)
i_am("Rpatentmap.Rproj")
library(arrow)
library(tidyverse)
library(data.table)

citations <- setDT(read_feather(here("data","g-patents", "citations.feather")))
patents <- setDT(read_feather(here("data","g-patents", "us_patents2011.feather")))
backup_country <- setDT(read_feather(here("data","g-patents", "backup_country.feather")))


setnames(citations, old = names(citations), new = paste0(names(citations), "_cit"))

patents_plus_full <- patents[citations, on = .(citation_pub_num = citation_pub_num_cit), nomatch = 0] 
patents_plus_full <- left_join(patents_plus_full,
                               backup_country, 
                               by = c("assignee_name_cit" = "assignee_name"))
patents_plus_full[, assignee_country_cit := fifelse(assignee_country_cit=="", assignee_country_backup, assignee_country_cit)]
#patents_plus_full <- patents_plus_full[assignee_country!=""] #if we decided to filter them out


#exports -----------------------------------------------------------------------
write_feather(patents_plus_full, here("data","interim", "patents_plus_full.feather"))
