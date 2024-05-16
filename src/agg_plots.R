library(here)
i_am("Rpatentmap.Rproj")
library(arrow)
library(tidyverse)
library(data.table)
library(scales)

#Ploting script -------------------------------
cggsave <- function(x){
  ggsave(here("results", paste0(deparse(substitute(x)), ".pdf")), plot = x,
         width = 1500, height = 1000, 
         units = "px", device = cairo_pdf, 
         dpi = 150)
}

patents <- open_dataset(here("data","interim","patent_links_panel.feather"), format="arrow")%>%
  select(publication_number,year, citation_pub_num, publication_date_cit, xi_real, xi_nominal)%>%
  collect()%>%
  unique()%>%
  setDT()

patval <- fread(here("data", "external", "KPSS_2022.csv")) 

pub_weight <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
  filter(country_code=="US"&kind_code%in%c("B1", "B2")&year>2000&year<2024)%>%
  mutate(assignee=paste0(assignee_name, assignee_country))%>%
  group_by(publication_number)%>%
  summarise(ptw = 1/n_distinct(assignee))%>%
  collect()%>%
  setDT()

ag_stats <- open_dataset(here("data","g-patents", "assignees_all.feather"), format="arrow")%>%
  filter(country_code=="US"&kind_code%in%c("B1", "B2")&year>2000&year<2024)%>%
  left_join(pub_weight, by="publication_number")%>%
  mutate(patent_num = as.integer(sub(".*-(.*)-.*", "\\1", publication_number)))%>%
  left_join(patval[, .(patent_num, xi_nominal, xi_real)],
            by="patent_num")%>%
  group_by(year, assignee_name, assignee_country)%>%
  summarise(totp_wgh=sum(ptw), totp_unw=n(), xi_real=mean(xi_real, na.rm=T))%>%
  collect()%>%
  setDT()


#Plot1: Number of patents by top 2.5% ---------------------
data1 <- ag_stats%>%
  group_by(year)%>%summarise(
    tot=sum(totp_wgh), 
    `Top 2.5% of patent publishers` = sum(totp_wgh[totp_wgh>quantile(totp_wgh, .975)]), 
    `Rest of firm population` = tot-`Top 2.5% of patent publishers`)%>%
  pivot_longer(-c(year))%>%
  filter(name!="tot")

plot1 <- data1%>%
  ggplot(aes(x=year,y=value, fill=name))+
  geom_bar(stat = "identity")+
  theme_bw()+ 
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ""))+
  scale_fill_brewer(palette = "Set1")+
  labs(x="", 
       title = "Number of published patents per year", 
       subtitle="US published patents only, weighted by number of assignees, 2001-2023", 
       fill="", 
       y="")+  
  theme(legend.position = "bottom", 
        legend.direction = "vertical",
        legend.margin = margin(-35,0,0,0),
        text = element_text(size=19))

plot1|>cggsave()

#Plot1b: Number of patents by top 2.5% by value ---------------------
data1b <- ag_stats%>%mutate(totxi_wgh=totp_wgh*xi_real)%>%
  group_by(year)%>%summarise(
    tot=sum(totxi_wgh, na.rm = T), 
    `Top 2.5% of patent publishers` = sum(totxi_wgh[totp_wgh>quantile(totp_wgh, .99)], na.rm = T), 
    `Rest of firm population` = tot-`Top 2.5% of patent publishers`)%>%
  pivot_longer(-c(year))%>%
  filter(name!="tot")

plot1b <- data1b%>%
  ggplot(aes(x=year,y=value, fill=name))+
  geom_bar(stat = "identity")+
  theme_bw()+ 
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6, big.mark = ""))+
  scale_fill_brewer(palette = "Set1")+
  labs(x="", 
       title = "Value of published patents per year", 
       subtitle="US published patents only, weighted by number of assignees, 2001-2023", 
       fill="", 
       y="")+  
  theme(legend.position = "bottom", 
        legend.direction = "vertical",
        legend.margin = margin(-35,0,0,0),
        text = element_text(size=19))

plot1b|>cggsave()

#Plot2: Distribution of number of patents by firm -------------------------
plot2 <- ag_stats%>%
  ggplot(aes(x=year,y=totp_unw))+
  stat_summary(fun = mean,
               geom = "pointrange",
               fun.min = function(z){quantile(z, 0.5)},
               fun.max = function(z){quantile(z, 0.95)})+
  theme_bw()+
  scale_color_brewer(palette = "Set1")+
  labs(x="", title = "Average number of patents published per firm, per year", subtitle="US published patents only, 2001-2023", color="", y="",
       caption="Lower bound (resp. Higher bound): Median (resp. 95th percentile)")+
  theme(legend.position = "bottom", text = element_text(size=19))

plot2 |> cggsave()

#Plot3: Share of citations with time
data3 <- patents %>% 
  select(publication_number,year, citation_pub_num, publication_date_cit)%>%
  unique()%>%
  group_by(year, year_cit=substr(as.character(publication_date_cit), 1,4))%>%
  filter(year>2003&year<2024&year_cit>year-20)%>%
  summarise(num=n())%>%
  ungroup()%>%
  arrange(year_cit)%>%
  filter(year_cit>1945&year_cit<2024)%>%
  group_by(year)%>%
  mutate(nm_sum = num/sum(num))

plot3 <- data3%>%arrange(year, year_cit)%>%
  filter(num>30)%>%
  group_by(year)%>%
  mutate(nm_sum=cumsum(nm_sum), year_cit=as.numeric(year_cit)-year)%>%
  ungroup()%>%
  ggplot(aes(x=year_cit, y=nm_sum, color=as.character(year%%5)))+
  geom_line(linewidth=1.1)+
  theme_bw()+
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2, big.mark = ""))+
  #scale_x_continuous(breaks = seq(1970, 2024, 10))+
  scale_color_brewer(palette = "Set1")+
  labs(x="", title = "Share of citations from granted patents per year", subtitle="US published patents only, citations from 20Y before grant", color="", y="",
       caption="8.5% of cited US patents by US patents published in 2004 were from 1999")+
  facet_wrap(~as.character(year))+
  geom_hline(yintercept = 0.5, alpha=0.75)+
  geom_vline(xintercept = -7.5, alpha=0.75)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,size=12, vjust=0.5),
        text=element_text(size=17))

plot3|>cggsave()

#Plot4: Share of citations with time
data4 <- patents %>% 
  select(publication_number, year, xi_real)%>%
  unique()%>%
  filter(year>2000)%>%
group_by(year)%>%
  summarise(num=n(),idd=sum(xi_real>=0, na.rm = T), 
            mne = mean(xi_real, na.rm = T),
            medn = median(xi_real, na.rm=T),
            prop=idd/num*100)%>%
  ungroup()%>%
  rename("Share of patents with available valuation (right)"=prop, 
         "Average real value of patent (left)"=mne,
         "Median real value of patent (left)"=medn)%>%
  pivot_longer(-c(year, idd, num))

#paste0(round(     *100,0),"%")
plot4 <- data4%>%filter(year<2023)%>%
  ggplot(aes(x=as.numeric(year), y=value, color=name))+
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis( transform =~.*.01, name="")
  )+
  geom_line(linewidth=1.1)+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(legend.position = "bottom", 
        legend.direction = "vertical",
        legend.margin = margin(-35,0,0,0),
        text=element_text(size=17))+
  labs(x="", title = "Average real patent value per year", 
       subtitle="US published patents only, 2001-2022", color="", y="")

plot4|>cggsave()
