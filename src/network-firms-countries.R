library(here)
i_am("Rpatentmap.Rproj")
library(arrow)
library(tidyverse)
library(data.table)
library(igraph)

totpatent_cnt_full <- read_feather(here("data","interim", "totpatent_cnt_full.feather"))
network_countries_noautocit <- read_feather(here("data","interim", "network_countries_noautocit.feather"))

list <- (totpatent_cnt_full%>%filter(sum_patent>100&!is.na(assignee_country)))$assignee_country


network_data <- network_countries_noautocit%>%
  filter(assignee_country%in%list&assignee_country_cit%in%list)%>%
           #assignee_country!=assignee_country_cit)%>%
  rename(to=assignee_country_cit, from=assignee_country, link=n)%>%
  mutate(link=link*2e-6)

vertices_df <- unique(c(network_data$from, network_data$to))
edges_df <- network_data[, c("from", "to", "link")]

g <- graph_from_data_frame(d=edges_df, vertices=vertices_df, directed=T)

total_patents_cnt <- as.data.frame(vertices_df)%>%
  rename(name=vertices_df)%>%
  left_join(totpatent_cnt_full, by=c("name"="assignee_country"))

V(g)$size <- sqrt(total_patents_cnt$sum_patent)/50

plot(g,layout=layout_with_kk(g), vertex.size=V(g)$size, edge.width=E(g)$link, vertex.label.cex=0.8,
     vertex.color="lightblue", edge.arrow.size =0.2)


title("Innovation links* for firms having been granted a patent in the USA in 2011 by origin")




