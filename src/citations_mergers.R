library(here)
i_am("Rpatentmap.Rproj")
library(arrow)
library(tidyverse)
library(data.table)

patval <- fread(here("data", "external", "KPSS_2022.csv")) 

citations <- open_dataset(here("data","g-patents", "citations_allbus.feather"), format="arrow")%>%
  filter(country_code=="US"&kind_code%in%c("B1","B2", "A")&year>1970)%>%
  mutate(citation_num = as.integer(sub(".*-(.*)-.*", "\\1", citation_pub_num)))%>%
  left_join(patval[, .(citation_num=patent_num, xi_nominal_cit=xi_nominal, xi_real_cit=xi_real)],
            by="citation_num")%>%
  collect()%>%
  unique()%>%
  setDT()

assignees <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
  filter(country_code=="US"&kind_code%in%c("B1","B2", "A")&year>1970)%>%
  select(publication_number, assignee_name, assignee_country)%>%
  collect()%>%
  unique()%>%
  setDT()

citations <- assignees[,.(citation_pub_num=publication_number, 
                        assignee_name_cit = assignee_name,
                        assignee_country_cit = assignee_country)][citations, 
on = .(citation_pub_num), allow.cartesian=T]

citations <- citations[!is.na(assignee_name_cit)]

names_vect <- c("country_code", "kind_code", "publication_date", "year")

setnames(citations, old = names_vect, 
         new = paste0(names_vect, "_cit"))

patents <- open_dataset(here("data","g-patents", "main_bus.feather"), format="arrow")%>%
  filter(year>2000&year<2024)%>%
  mutate(patent_num = as.integer(sub(".*-(.*)-.*", "\\1", publication_number)))%>%
  left_join(patval[, .(patent_num, xi_nominal, xi_real)],
            by="patent_num")%>%
  collect()%>%
  unique()%>%
  setDT()

patents <- citations[patents, on = .(publication_number)]

patents <- assignees[patents, on = .(publication_number), allow.cartesian = T]

write_feather(patents, here("data","interim","patent_links_panel.feather"))
