library(here)
i_am("Rpatentmap.Rproj")
library(arrow)
library(tidyverse)
library(data.table)
library(igraph)

totpatent_full<-read_feather(here("data","interim", "totpatent_full.feather"))
network<-read_feather(here("data","interim", "network.feather"))

#Links between firms
list <- (totpatent_full%>%filter(tot_patent>50))$assignee_name

net <- network_noautocit%>%filter(n>450&assignee_name%in%list)%>%
  rename(from=assignee_name_cit, to=assignee_name, link=n)%>%
  mutate(link=sqrt(link)*55e-3)

vertices <- unique(c(net$from, net$to))
edges <- net[, c("from", "to", "link")]

g <- graph_from_data_frame(d=edges, vertices=vertices, directed=TRUE)

total_patents <- as.data.frame(vertices)%>%
  rename(name=vertices)%>%
  left_join(totpatent_full, by=c("name"="assignee_name"))

V(g)$size <- sqrt(total_patents$sum_patent)*55e-3

plot(g,layout=layout_with_fr(g), vertex.size=V(g)$size, edge.width=E(g)$link, vertex.label.cex=0.4,
     vertex.color="lightblue", edge.arrow.size =0.2)
title("Innovation links for firms that were granted at least 50 patents in the US in 2011", 
      sub = "*sqrt of combined citations between firms, citations from 1945 onward")



#Without direction version
net$pair <- apply(net[, c('from', 'to')], 1, function(x) paste(sort(x), collapse = "-"))

# Aggregate data
aggregated_net <- net %>%
  group_by(pair) %>%
  summarise(link = sum(link)) %>%
  mutate(from = sapply(strsplit(as.character(pair), "-"), `[`, 1),
         to = sapply(strsplit(as.character(pair), "-"), `[`, 2),
         link=log(link)*0.5)

vertices <- unique(c(aggregated_net$from, aggregated_net$to))
edges <- aggregated_net[, c("from", "to", "link")]

g <- graph_from_data_frame(d=edges, vertices=vertices, directed=F)

total_patents <- as.data.frame(vertices)%>%
  rename(name=vertices)%>%
  left_join(totpatent_full, by=c("name"="assignee_name"))

V(g)$size <- log(total_patents$sum_patent)*0.85

plot(g,layout=layout_with_fr(g), vertex.size=V(g)$size, edge.width=E(g)$link, vertex.label.cex=0.4,
     vertex.color="lightblue")
title("Top 250 innovation links* for firms having been granted a patent in the USA in 2011", 
      sub = "*log of combined citations between firms, citations from 1945 onward")