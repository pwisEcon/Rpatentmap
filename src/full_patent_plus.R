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

network <- patents_plus_full[, .(n = .N), by = .(assignee_name, assignee_country, assignee_name_cit, assignee_country_cit)]
network_noautocit <- patents_plus_full[assignee_name!=assignee_name_cit][, .(n = .N), by = .(assignee_name, assignee_name_cit)]

network_countries <- patents_plus_full[, .(n = .N), by = .(assignee_country, assignee_country_cit)]
network_countries_noautocit <- patents_plus_full[assignee_name!=assignee_name_cit][, .(n = .N), by = .(assignee_country, assignee_country_cit)]


totpatent <- patents_plus_full[, .(tot_patent = uniqueN(publication_number)), by = .(assignee_name)]
totpatent_cit <- patents_plus_full[, .(tot_patent_cit = uniqueN(citation_pub_num)), by = .(assignee_name_cit)]
totpatent_full <- totpatent[totpatent_cit, on = .(assignee_name = assignee_name_cit)]%>%
  rowwise()%>%
  mutate(sum_patent=sum(tot_patent,tot_patent_cit, na.rm = T))

totpatent_cnt <- patents_plus_full[, .(tot_patent = uniqueN(publication_number)), by = .(assignee_country)]
totpatent_cnt_cit <- patents_plus_full[, .(tot_patent_cit = uniqueN(citation_pub_num)), by = .(assignee_country_cit)]
totpatent_cnt_full <- totpatent_cnt[totpatent_cnt_cit, on = .(assignee_country = assignee_country_cit)]%>%
  rowwise()%>%
  mutate(sum_patent=sum(tot_patent,tot_patent_cit, na.rm = T))


#exports -----------------------------------------------------------------------
write_feather(patents_plus_full, here("data","interim", "patents_plus_full.feather"))
write_feather(network, here("data","interim", "network.feather"))
write_feather(network_noautocit, here("data","interim", "network_noautocit.feather"))
write_feather(network_countries, here("data","interim", "network_countries.feather"))
write_feather(network_countries_noautocit, here("data","interim", "network_countries_noautocit.feather"))
write_feather(totpatent_full, here("data","interim", "totpatent_full.feather"))
write_feather(totpatent_cnt_full, here("data","interim", "totpatent_cnt_full.feather"))
