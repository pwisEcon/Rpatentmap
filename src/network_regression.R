library(tidyverse)
library(arrow)
library(data.table)
library(igraph)
library(here)
#Require installed but not loaded
#lmtest - sandwich

patval <- fread(here("data", "external", "KPSS_2022.csv"))
cit <- read_feather(here("data", "interim", "cit.feather"))
citb2 <- read_feather(here("data", "g-patents", "citb2.feather"))

cpc3 <- open_dataset(here("data","g-patents", "cpc_bus.feather"), format="arrow")%>%
  filter(first==T)%>%
  mutate(cpc3=substr(code, 1,3))%>%
  select(publication_number, cpc3)%>%
  collect()%>%
  unique()%>%
  setDT()

cpc31 <- cpc3[, .SD[1], by=.(publication_number)]

regcit <- cit %>% filter(abs(publication_date-date_fil_cit)<300)%>%
  filter(substr(pub_cit, nchar(pub_cit)-1,nchar(pub_cit))=="B2")%>%
  mutate(filled_post_pub = if_else(date_fil_cit>grant_date, T, F))%>%
  group_by(application_number, filled_post_pub, fil_year=substr(filing_date, 1,4))%>%
  summarise(cnt=n())%>%ungroup()

regcit <- regcit%>%group_by(application_number)%>%filter(n()==2)

regcit2 <- unique(citb2[,.(application_number, publication_number)])[regcit, on="application_number"]
regcit2 <- cpc31[regcit2, on=.(publication_number)]
setDT(regcit2)[, application_number:=as.numeric(sub(".*-(.*)-.*", "\\1", application_number))]


#Compute simple aggregates -----------------------------------------------------
npat_list <- vector("list", length = 3)
ref_years <- c(2010,2015,2019)
for(i in 1:3){
  npat_list[[i]] <- open_dataset(here("data","interim", "patent_links_panel.feather"),
               format = "arrow")%>%
    filter(year==ref_years[i])%>%
    select(publication_number, assignee_name)%>%
    collect()%>%
    unique()%>%
    group_by(assignee_name)%>%
    summarise(cnt=n())%>%ungroup()%>%
    select(assignee_name, cnt)%>%setDT()
}

#Main logic --------------------------------------------------------------------
compute_network <- function(data, aggreg_cols, autocit=F){
  if(autocit==F){
    df <- data[data[[aggreg_cols[1]]]!=data[[aggreg_cols[2]]]][, .(n = .N), by = aggreg_cols]
  }
  else{
    df <- data[data[[aggreg_cols[1]]]==data[[aggreg_cols[2]]]][, .(n = .N), by = aggreg_cols]
  }
  return(df)
}

network_regression <- function(patPub_Y){
  ls_firms <- open_dataset(here("data","interim", "patent_links_panel.feather"),
                           format = "arrow")%>%
    filter(year%in%patPub_Y)%>%
    select(publication_number, assignee_name)%>%
    collect()%>%
    unique()%>%
    group_by(assignee_name)%>%
    summarise(cnt=n())%>%ungroup()%>%
    filter(cnt>quantile(cnt, 0.975))%>%
    select(assignee_name)%>%unique()%>%unlist()
  
  npatY <- open_dataset(here("data","interim", "patent_links_panel.feather"),
                        format = "arrow")%>%
    filter(year%in%patPub_Y)%>%
    select(publication_number, assignee_name)%>%
    collect()%>%
    unique()%>%
    group_by(assignee_name)%>%
    summarise(cnt=n())%>%ungroup()%>%
    select(assignee_name, cnt)%>%setDT()
  
  npat <- open_dataset(here("data","interim", "patent_links_panel.feather"),
                       format = "arrow")%>%
    filter(year_cit%in%patPub_Y&year>year_cit&year<year_cit+10)%>%
    select(citation_pub_num, publication_number, assignee_name_cit)%>%
    collect()%>%
    unique()%>%
    group_by(assignee_name=assignee_name_cit)%>%
    summarise(fward_cit=n())%>%
    ungroup()
  
  xi<-open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(year%in%patPub_Y&kind_code%in%c("B1", "B2")&country_code=="US")%>%
    select(publication_number, assignee_name)%>%
    mutate(patent_num = as.numeric(sub(".*-(.*)-.*", "\\1", publication_number)))%>%
    collect()%>%
    unique()%>%
    inner_join(patval%>%select(patent_num, xi_real), by="patent_num")%>%
    group_by(assignee_name)%>%
    summarise(xi_real=mean(xi_real))
  
  tp <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(year%in%patPub_Y&kind_code%in%c("B1", "B2")&country_code=="US")%>%
    select(publication_number, assignee_name)%>%
    collect()%>%
    unique()%>%
    left_join(cpc3, by="publication_number")%>%
    group_by(assignee_name,cpc3)%>%
    summarise(cnt=n())%>%
    group_by(assignee_name)%>%
    filter(cnt>5&cnt==max(cnt))%>%
    ungroup()%>%
    select(-cnt)%>%
    setDT()
  
  tp <- tp[, .SD[1], by=.(assignee_name)]
  
  mtp <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(year%in%patPub_Y&kind_code%in%c("B1", "B2")&country_code=="US")%>%
    select(publication_number, assignee_name)%>%
    collect()%>%
    unique()%>%
    left_join(cpc3, by="publication_number")%>%
    group_by(assignee_name)%>%
    summarise(cnt=uniqueN(cpc3))%>%
    group_by(assignee_name)%>%ungroup()%>%
    setDT()
  
  simpat <- open_dataset(here("data","g-patents", "simpat_bus.feather"), format="arrow")%>%
    filter(year%in%patPub_Y&publication_number!=publication_number_sim)%>%
    mutate(patent_num = as.integer(sub(".*-(.*)-.*", "\\1", publication_number)),
           patent_num_sim = as.integer(sub(".*-(.*)-.*", "\\1", publication_number_sim)))%>%
    collect()
  
  simpat_assignees <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(country_code=="US"&kind_code%in%c("B1","B2", "A")&year%in%patPub_Y)%>%
    select(publication_number, assignee_name)%>%
    inner_join(simpat, by="publication_number")%>%
    collect()%>%
    unique()%>%
    setDT()
  
  simpat_assignees2 <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(country_code=="US"&kind_code%in%c("B1","B2", "A")&year%in%patPub_Y)%>%
    select(publication_number_sim=publication_number, assignee_name_sim=assignee_name)%>%
    inner_join(simpat_assignees, by="publication_number_sim")%>%
    collect()%>%
    unique()%>%
    setDT()
  
  
  citdY <- patPub_Y[1]:(patPub_Y[3]+5)
  
    
  net_simpat <- compute_network(simpat_assignees2, c("assignee_name", "assignee_name_sim"))
  
  patents_plus_full2 <- open_dataset(here("data","interim", "patent_links_panel.feather"),
                                     format = "arrow")%>%
    filter(year_cit%in%patPub_Y&year%in%citdY&assignee_name%in%ls_firms&assignee_name_cit%in%ls_firms)%>%
    collect()
  
  network_noautocit <- compute_network(patents_plus_full2, c("assignee_name", "assignee_name_cit"), autocit=F)
  
  network_noautocit <- network_noautocit%>%
    left_join(net_simpat[,.(assignee_name, assignee_name_cit=assignee_name_sim, n_sim= n)], 
              by=c("assignee_name", "assignee_name_cit"))%>%
    replace_na(list(n_sim=0))
  
  simshare <- network_noautocit%>%
    group_by(assignee_name)%>%summarise(simshare=mean(n_sim/n))
  
  #summary(lm(n_sim~n, data=network_noautocit))
  #cor(network_noautocit$n_sim, network_noautocit$n)
  
  net <- setDT(network_noautocit)[n>1, .("from"=assignee_name_cit, "to"=assignee_name, "link"=n, n_sim)]
  
  g <- graph_from_data_frame(net, directed = TRUE, vertices = NULL)
  E(g)$weight <- net$link
  g_undirected <- as.undirected(g, mode = "collapse", edge.attr.comb=list(weight="sum"))
  leiden_community <- cluster_leiden(g_undirected, weights = E(g_undirected)$weight, resolution_parameter = 1)
  p=10
  
  ls_regcit3 <- regcit2%>%select(publication_number)%>%unlist()
  
  regcit3 <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(publication_number%in%ls_regcit3&year%in%patPub_Y)%>%
    select(publication_number, assignee_name)%>%
    inner_join(regcit2%>%select(publication_number, cpc3, cnt, filled_post_pub))%>%
    collect()%>%
    unique()%>%
    setDT()
  
  regcit3_cpc <- regcit3[,.(cnt=.N),by=.(assignee_name,cpc1=substr(cpc3,1,1))][,
    max_cnt:=max((cnt)), by=.(assignee_name)
  ][cnt==max_cnt, .SD[1], by=.(assignee_name)][,.(assignee_name, cpc1)]
  
  regcit3 <- regcit3[, .(mcnt=mean(cnt)), by=.(assignee_name, filled_post_pub)][regcit3_cpc,
    on=.(assignee_name)
  ]
  
  ls_ascit3 <- regcit3$assignee_name%>%unique()%>%unlist()
  
  xi<-open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
    filter(assignee_name%in%ls_ascit3&year%in%patPub_Y&kind_code=="B2"&country_code=="US")%>%
    select(publication_number, assignee_name)%>%
    mutate(patent_num = as.numeric(sub(".*-(.*)-.*", "\\1", publication_number)))%>%
    collect()%>%
    unique()%>%
    inner_join(patval%>%select(patent_num, xi_real), by="patent_num")%>%
    group_by(assignee_name)%>%
    summarise(xi_real=mean(xi_real))
  
  regcit3_xi <- setDT(xi)[regcit3, on="assignee_name"]%>%
    remove_missing()
  
  thsd <- regcit3_xi%>%group_by(assignee_name)%>%
    mutate(xi_real=mean(xi_real), 
           fl=(mcnt-lag(mcnt)))%>%
    remove_missing()
  
  #USED TO OUTPUT THE EXAMPLE CLUSTER ------------------------
  #a <- leiden_community[sizes(leiden_community)==30][[1]]
  #E(y)$weight <- net[from%in%a&to%in%a]$link
  
  
  #pdf(here("results", "clustering_example.pdf"), width = 22, height = 16)
  #par(mar=c(5.5, 3, 5.5, 2)) 
  #plot(y,layout=layout_with_fr(y), vertex.label.color = "black", vertex.color="lightblue")
  
  #title("Innovation cluster n°30 - Firms related to power tools producer and sellers", 
  #      sub = "From the 2013:2015+2020 forward citations network",
  #      cex.main=2, cex.sub=1.7)
  #dev.off()
  
  dframe = rbind()
  for(i in names(sizes(leiden_community)[sizes(leiden_community)>p])){
    a <- leiden_community[[i]]
    y <- graph_from_data_frame(net[from%in%a&to%in%a], directed = TRUE, vertices = NULL) 
    E(y)$weight <- net[from%in%a&to%in%a]$link
    #plot(y)
    ls_comA <- a%>%unlist()
    vv <- npat%>%filter(assignee_name%in%ls_comA)
    closeness_centrality <- closeness(y)
    betweenness_centrality <- betweenness(y)
    gtp <- data.frame(cl_centra=closeness_centrality,
                      bt_centra=betweenness_centrality,
                      assignee_name=names(closeness_centrality))
    ttr <- vv%>%
      left_join(gtp, by="assignee_name")%>%
      inner_join(xi, by="assignee_name")%>%
      inner_join(tp, by="assignee_name")%>%
      inner_join(setDT(thsd)[,.(assignee_name, fl, mcnt)], by="assignee_name")%>%
      inner_join(npatY%>%rename(nY=cnt), by="assignee_name")%>%
      inner_join(simshare, by="assignee_name")%>%
      inner_join(mtp[, .(assignee_name, n_cpc=cnt)], by="assignee_name")%>%
      mutate(clust=as.character(i),
             clust_size=sizes(leiden_community)[sizes(leiden_community)>p][[i]])%>%
      left_join(npat_list[[3]]%>%rename(n2019=cnt), by="assignee_name")%>%
      left_join(npat_list[[2]]%>%rename(n2015=cnt), by="assignee_name")%>%
      left_join(npat_list[[1]]%>%rename(n2010=cnt), by="assignee_name")%>%
      replace_na(list(n2019=0, n2015=0, n2010=0))%>%
      group_by(clust)%>%
      mutate(n_cpc_clust=mean(n_cpc), nY_clust=mean(nY), simshare_clust=mean(simshare),
             fl_clust=mean(fl))
    dframe <- rbind(dframe, ttr)
  }
  
  #Regression 1 - patval
  reg1 <- lm(log(xi_real+1)~bt_centra+fl+n_cpc+simshare+nY+as.factor(cpc3), data=dframe)
  #Good for heterosk
  
  #Regression 2 - centrality 
  reg2 <- lm(log(bt_centra+1)~xi_real+fl+nY+as.factor(cpc3)+clust_size,data=dframe)
  #Should add robust
  
  #Regression 3 - explain future citations 
  #Good for heterosk
  if(patPub_Y[1]==2005){
    reg3 <- lm(log(n2015+1)~bt_centra+nY+simshare+n_cpc+xi_real+fl+as.factor(cpc3)+as.factor(clust)+mcnt,data=dframe)
  }else{
    reg3 <- lm(log(n2019+1)~bt_centra+nY+simshare+n_cpc+xi_real+fl+as.factor(cpc3)+as.factor(cpc3)+mcnt,data=dframe)
  }
  dtp <- dframe%>%group_by(clust)%>%
    mutate(ncl=mean(nY*cl_centra), n_cpc=mean(n_cpc*cl_centra),fl=mean(fl*cl_centra*mcnt),
                                           clust_size=mean(clust_size), xi_real=mean(xi_real*cl_centra))
  
  #Regression 4 - Cluster effects? Most important is size;
  #Need to control for heteroske -- 
  if(patPub_Y[1]==2005){
    reg4a <- lm(log(n2015+1)~nY*(ncl+clust_size+n_cpc+fl),data=dtp)
    reg4b <- lm(log(n2015+1)~bt_centra*(ncl+clust_size+n_cpc+nY),data=dtp)
  }else{
    reg4a <- lm(log(n2019+1)~nY*(ncl+clust_size+n_cpc+fl),data=dtp)
    reg4b <- lm(log(n2019+1)~bt_centra*(ncl+clust_size+n_cpc+nY),data=dtp)
  }
  
  date_reg <- list(dframe, reg1, reg2, reg3, reg4a, reg4b)
  
  return(date_reg)
}

#INTEREPRETING RESULTS ---------------------------------------------------------
options(max.print=150)

regs201315 <- network_regression(2013:2015)
sink("network_regs201315.txt")
mean(regs201315[[1]]$fl)
summary(regs201315[[2]])
summary(regs201315[[3]])
print("With robust errors")
lmtest::coeftest(regs201315[[3]], vcov = sandwich::vcovHC(regs201315[[3]], type="HC0"))
summary(regs201315[[4]])
summary(regs201315[[5]])
print("With robust errors")
lmtest::coeftest(regs201315[[5]], vcov = sandwich::vcovHC(regs201315[[5]], type="HC0"))
summary(regs201315[[6]])
print("With robust errors")
lmtest::coeftest(regs201315[[6]], vcov = sandwich::vcovHC(regs201315[[6]], type="HC0"))
sink()

regs200507 <- network_regression(2005:2007)
sink("network_regs200507.txt")
mean(regs200507[[1]]$fl)
summary(regs200507[[2]])
summary(regs200507[[3]])
print("With robust errors")
lmtest::coeftest(regs200507[[3]], vcov = sandwich::vcovHC(regs200507[[3]], type="HC0"))
summary(regs200507[[4]])
summary(regs200507[[5]])
print("With robust errors")
lmtest::coeftest(regs200507[[5]], vcov = sandwich::vcovHC(regs200507[[5]], type="HC0"))
summary(regs200507[[6]])
print("With robust errors")
lmtest::coeftest(regs200507[[6]], vcov = sandwich::vcovHC(regs200507[[6]], type="HC0"))
sink()
