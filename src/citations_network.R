library(here)
i_am("Rpatentmap.Rproj")
library(arrow)
library(tidyverse)
library(data.table)
library(igraph)

#Custom function calls ---------------------------------------------------------
count_patents <- function(data, aggreg_cols){
  on_clause <- setNames(aggreg_cols[2], aggreg_cols[1])
  df1 <- data[, .(tot_patent = uniqueN(publication_number)), by = eval(aggreg_cols[1])]
  df2 <- data[, .(tot_patent_cit = uniqueN(citation_pub_num)), by = eval(aggreg_cols[2])]
  df3 <- df1[df2, on = (on_clause)]%>%
    rowwise()%>%
    mutate(sum_patent=sum(tot_patent,tot_patent_cit, na.rm = T))
  return(df3)
}

compute_network <- function(data, aggreg_cols, autocit=F){
  if(autocit==F){
    df <- data[data[[aggreg_cols[1]]]!=data[[aggreg_cols[2]]]][, .(n = .N), by = aggreg_cols]
  }
  else{
    df <- data[, .(n = .N), by = aggreg_cols]
  }
  return(df)
}

igraph_network <- function(total_p, network, aggreg_cols, 
                           cutoff1, varcutoff, cutoff2, 
                           scale1, scale2, scale3, scale4){
  list <- (total_p%>%filter(!!sym(varcutoff)>cutoff1))[[aggreg_cols[1]]]
  net <- network%>%filter(n>cutoff2&!!sym(aggreg_cols[1])%in%list)%>%
    rename(from=!!sym(aggreg_cols[2]), to=!!sym(aggreg_cols[1]), link=n)%>%
    mutate(link=(link^(scale1))*scale2)
  vertices <- unique(c(net$from, net$to))
  edges <- net[, c("from", "to", "link")]
  g <- graph_from_data_frame(d=edges, vertices=vertices, directed=TRUE)
  total_patents <- as.data.frame(vertices)%>%
    rename(name=vertices)%>%
    left_join(total_p, by=c("name"=aggreg_cols[1]))
  V(g)$size <- ((total_patents$sum_patent)^scale3)*scale4
  return(g)
}

#Load data and filter data -----------------------------------------------------
patents_plus_full <- read_feather(here("data","interim", "patents_plus_full.feather"))
patents_plus_full <- patents_plus_full[publication_date_cit>19999999&str_starts(kind_code_cit, "B")]

#Plot network for firms --------------------------------------------------------

totpatent_full <- count_patents(patents_plus_full, c("assignee_name", "assignee_name_cit"))
network_noautocit <- compute_network(patents_plus_full, c("assignee_name", "assignee_name_cit"))

l <- igraph_network(totpatent_full, network_noautocit, c("assignee_name", "assignee_name_cit"),
                    50,"tot_patent", 125, 
                    0.5, 55e-3, 0.5, 0.9e-1)

pdf(here("results", "innov-2000p-firms.pdf"), width = 22, height = 16)

plot(l,layout=layout_with_fr(l), vertex.size=V(l)$size, edge.width=E(l)$link, vertex.label.cex=0.4,
     vertex.color="lightblue", edge.arrow.size =0.2)

title("Innovation links* to firms that were granted at least 50 patents in the US in 2011", 
      sub = "*Citations between firms, citations from 2000 onward, only granted patents, 
      cited firms do not need to have published in the US in 2011, a link is shown if there is at least 125 citations")

dev.off()

#Plot network for countries  ---------------------------------------------------
totpatent_full <- count_patents(patents_plus_full, c("assignee_country", "assignee_country_cit"))
network_noautocit <- compute_network(patents_plus_full, c("assignee_country", "assignee_country_cit"))

l <- igraph_network(totpatent_full, network_noautocit, c("assignee_country", "assignee_country_cit"),
                    100,"sum_patent", 75, 
                    1, 5e-5, 0.5, 1/50)


pdf(here("results", "innov-2000p-countries.pdf"), width = 22, height = 16)

plot(l,layout=layout_with_kk(l), vertex.size=V(l)$size, edge.width=E(l)$link, vertex.label.cex=0.8,
     vertex.color="lightblue", edge.arrow.size =0.15)

title("Innovation links* to firms that were granted patents in the US in 2011", 
      sub = "*Citations between countries, innovator nationality shown if at least 100 patents in the US, citations from 2000 onward, 
      only granted patents, cited nationality do not need to have published in 2011, a link is shown if there is at least 75 citations")

dev.off()