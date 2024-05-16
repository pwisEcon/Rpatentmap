library(here)

cggsave <- function(x){
  ggsave(here("results", paste0(deparse(substitute(x)), ".pdf")), plot = x,
         width = 1500, height = 1000, 
         units = "px", device = cairo_pdf, 
         dpi = 150)
}

cto_date <- function(x){
  return(as.Date(as.character(x), format="%Y%m%d"))
}

dtm <- 365.2425/12

cpc3 <- open_dataset(here("data","g-patents", "cpc_bus.feather"), format="arrow")%>%
  filter(first==T)%>%
  mutate(cpc3=substr(code, 1,3))%>%
  select(publication_number, cpc3)%>%
  collect()%>%
  unique()%>%
  setDT()

citb2 <- open_dataset(here("data","g-patents", "citb2.feather"), format="arrow")%>%
  collect()%>%
  mutate(type="B2", grant_date=publication_date)%>%
  setDT()

cita1 <- open_dataset(here("data","g-patents", "cita1.feather"), format="arrow")%>%
  collect()%>%
  mutate(type="A1")%>%
  left_join(citb2%>%select(application_number, filing_date, grant_date)%>%unique(), by=c("application_number"))%>%
  remove_missing()

cit <- rbind(cita1, citb2)
cit<- setDT(cit)[, .SD[1], by=.(application_number, pub_cit)] #A few Google Patents mistake

write_feather(cit, here("data", "interim", "cit.feather"))

mo = c("10","11","12")
mo2 = c("10","11","12")


cit_timing <- unique(citb2[, .(application_number, pubB2=publication_date, filing_date)])
cit_timing <- unique(cita1[, .(application_number, pubA1=publication_date)])[cit_timing, on=.(application_number), nomatch=NULL]

cit_timing[, 
  c("pubA1", "pubB2", "filing_date"):=list(cto_date(pubA1),cto_date(pubB2),cto_date(filing_date))
]

cit_timing[,
  c("ndA1", "ndB2", "ndA1B2"):=
    list(as.integer(pubA1-filing_date), as.integer(pubB2-filing_date), as.integer(pubB2-pubA1))
]


plot_timings <- cit_timing%>%
  filter(ndA1>0&ndA1<75*dtm&ndB2<180*dtm)%>%
  select(application_number, ndA1, ndB2)%>%
  pivot_longer(-application_number)%>%
  mutate(name=if_else(name=="ndA1", "Time to publication", "Time to grant"),
         name=as.factor(name)%>%relevel("Time to publication"))%>%
  ggplot(aes(x=value/dtm, color=name))+
  geom_density(linewidth=1.1)+
  theme_bw()+
  labs(title="Number of months before granting after publication/filing",
       subtitle="Density plot", x="",y="", 
       color="")+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~name, scales="free")+
  theme(legend.position = "bottom", 
        text = element_text(size=17), 
        legend.margin = margin(-15,0,5,0),
        plot.margin = margin(7,15,7,5))

plot_timings|>cggsave()

cit_timing%>%filter(ndA1B2<150*dtm)%>%
  ggplot(aes(x=ndA1B2/dtm))+geom_density()+
  labs(title="Number of months between publication (A1) and grant (B2)")+
  scale_color_brewer(palette = "Set1")+theme_bw()

#EXPERIMENT LOGIC --------------------------------------------------------------
mo = c("10","11","12") 

reg2df <- function(reg){
  data.frame(coef=reg$coefficients, confint(reg, level=0.94))%>%
    tibble::rownames_to_column()%>%
    filter(str_detect(rowname,":"))%>%
    mutate(rowname=substr(rowname, 8,8))
}

synthetic_did <- function(x, mo){
  ndA1_range <- if(x==1){180:195}else{170:200}
  ndA1B2_rangeT <- if(x==1){685:705}else{640:670}
  ndA1B2_rangeC <- if(x==1){776:796}else{760:790}
  ref <- if(x==1){"4"}else{"5"}
  control_g <- cit_timing%>%
    filter(ndA1%in%ndA1_range&(ndA1B2%in%ndA1B2_rangeC))%>%
    select(application_number)%>%
    unlist()
  treat_g <- cit_timing%>%
    filter(ndA1%in%ndA1_range&(ndA1B2%in%ndA1B2_rangeT))%>%
    select(application_number)%>%
    unlist()
  cit_test <- cit[application_number%in%treat_g|application_number%in%control_g]
  cit_test[, 
     c("date_fil_cit","filing_date", "grant_date"):=
       list(cto_date(date_fil_cit), cto_date(filing_date), cto_date(grant_date))
  ]
  test <- cit_test%>%
    mutate(time = ceiling(as.integer(date_fil_cit-filing_date)/91.25),
           tgroup=if_else(application_number%in%treat_g, T, F), 
           treatment=if_else(time>6, T,F))%>%
    group_by(application_number, tgroup, treatment, time)%>%
    summarise(cnt=n())%>%ungroup()%>%
    filter(time>1&time<9)%>%
    ungroup()%>%
    mutate(quarter=as.factor(time)%>%relevel(ref))
  return(test)
}

#RESULTS -----------------------------------------------------------------------
data1 <- synthetic_did(1)
ptrend1 <- data1%>%group_by(tgroup, time, treatment)%>%summarise(cnt=sum(cnt))%>%
  mutate(time=as.integer(time))%>%
  ggplot(aes(x=time, y=cnt, color=tgroup))+
  labs(title="Parralel trend test", x="N° of quarters", y="cit")+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(legend.position = "bottom", 
        text = element_text(size=19))+
  scale_color_brewer(palette = "Set1")

ptrend1|>cggsave()

did1 <- lm(log(cnt)~quarter*tgroup, data=data1)

sink("did1.txt")
summary(did1)
sink()

plotDD1 <- reg2df(did1)%>%
  ggplot(aes(x=rowname, y=coef))+
  geom_point()+geom_pointrange(aes(ymin=`X3..`, ymax=`X97..`))+
  geom_hline(yintercept = 0)+
  labs(title="DiD results", x="N° of quarters")+
  theme_bw()+
  theme(legend.position = "bottom", 
        text = element_text(size=19))

plotDD1|>cggsave()

#SECOND TEST -------------------------------------------------------------------

data2 <- synthetic_did(2)
ptrend2 <- data2%>%group_by(tgroup, time, treatment)%>%summarise(cnt=sum(cnt))%>%
  mutate(time=as.integer(time))%>%
  ggplot(aes(x=time, y=cnt, color=tgroup))+
  labs(title="Parralel trend test", x="N° of quarters", y="cit")+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(legend.position = "bottom", 
        text = element_text(size=19))+
  scale_color_brewer(palette = "Set1")

ptrend2|>cggsave()

did2 <- lm(log(cnt)~quarter*tgroup, data=data2)

sink("did2.txt")
summary(did2)
sink()

plotDD2 <- reg2df(did2)%>%
  ggplot(aes(x=rowname, y=coef))+
  geom_point()+geom_pointrange(aes(ymin=`X3..`, ymax=`X97..`))+
  geom_hline(yintercept = 0)+
  labs(title="DiD results", x="N° of quarters")+
  theme_bw()+
  theme(legend.position = "bottom", 
        text = element_text(size=19))

plotDD2|>cggsave()
