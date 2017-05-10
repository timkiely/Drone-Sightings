





setwd("~/Dropbox (Personal)/Viz_Challenges")

library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(grid)
library(readr)
library(gplots)

# get copies of the data locally

URL1 <- "http://www.faa.gov/uas/media/UAS_Sightings_report_21Aug-31Jan.xlsx"
URL2 <- "http://www.faa.gov/uas/media/UASEventsNov2014-Aug2015.xls"

fil1 <- basename(URL1)
fil2 <- basename(URL2)

if (!file.exists(fil1)) download.file(URL1, fil1)
if (!file.exists(fil2)) download.file(URL2, fil2)

# read it in

xl1 <- read_excel(fil1)
xl2 <- read_excel(fil2)

# munge it a bit so we can play with it by various calendrical options

drones <- setNames(bind_rows(xl2[,1:3],
                             xl1[,c(1,3,4)]), 
                   c("ts", "city", "state"))
drones <- mutate(drones, 
                 year=format(ts, "%Y"), 
                 year_mon=format(ts, "%Y%m"), 
                 ymd=as.Date(ts), 
                 yw=format(ts, "%Y%V"))


# read in geo lat/lon coordinates by city. credit to the fine folks at MIT for open-sourcing that!
URL3<-"http://simplemaps.com/files/cities.csv"
fil3 <- basename(URL3)
if (!file.exists(fil3)) download.file(URL3, fil3)

Cities<-read_csv("cities.csv")

# list of states with their abbreviations:
URL4<-"http://www.fonz.net/blog/wp-content/uploads/2008/04/states.csv"
fil4 <- basename(URL4)
if (!file.exists(fil4)) download.file(URL4, fil4)

States<-read_csv("states.csv")

Cities2<-
  left_join(Cities,States,by=c("state"="Abbreviation")) %>% 
  mutate("City-State"=paste0(city,"-",State)) %>% 
  select(`City-State`,lat,lng,zip)
  
By_City<-
drones %>% 
  group_by(year,city,state) %>% 
  dplyr::summarise(count=n()) %>% 
  mutate("City-State"=paste0(city,"-",state)) %>% 
  left_join(Cities2,by="City-State")

# how many successful maps?
table(is.na(By_City$lat))




## PLOTTING
library(ggplot2)
library(grid)
library(gridExtra)


states <- map_data("state")


## Heatmap of all drone sightings

#draw the base ggplot
ggplot(By_City %>% filter(!is.na(year))) + 
  
  # add the US map
  geom_polygon(data = states 
               ,aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  
  # plot the drone sightings
  geom_point(aes(x=lng, y=lat, size=count),color="darkred")+

  # add density lines
  stat_density2d(aes(x = lng
                     , y = lat)
                 ,n=100
                 ,size=0.5
                 ,bins=10
                 ,color="white") + 
  
  # add a second density layer, this time with color
  stat_density2d(aes(x = lng
                   , y = lat
                   ,fill=..level..
                   ,alpha=..level..)
               ,n=100
               ,size=1
               ,bins = 10
               ,geom = "polygon") + 
  
  # various theme parameters
  scale_fill_gradient(low = "yellow", high = "red") + 
  scale_alpha_continuous(range = c(0.1,0.8))+
  coord_map(xlim=c(-130,-60),ylim=c(23,50))+
  theme_map()+
  theme(legend.position="none"
        ,title=element_text(size=15,face="bold")
        ,panel.background=element_rect(fill="black")
        )+
  labs(title="Where are you most likely to see a drone in the U.S?"
       ,subtitle="Drone sightings in the US; 2014 - 2016"
       ,caption="Data from http://www.faa.gov/uas/law_enforcement/uas_sighting_reports/")





## Draw a GIF!!!
By_month<-
  drones %>% 
  mutate(month=lubridate::month(ts,lab=T)) %>% 
  group_by(year,month,year_mon,city,state) %>% 
  dplyr::summarise(count=n()) %>% 
  mutate("City-State"=paste0(city,"-",state)) %>% 
  left_join(Cities2,by="City-State") %>% 
  ungroup() %>% 
  mutate(MonthLab=paste0(month,"-",year))

monthOrder<-
By_month %>%
  group_by(year_mon,MonthLab) %>% summarise(count=sum(count)) %>% 
  select(-count) %>% 
  filter(!is.na(year_mon)) %>% 
  ungroup() %>% 
  select(-year_mon)

monthOrder<-as.character(monthOrder$MonthLab)

By_month<-By_month %>% mutate(MonthLab=factor(MonthLab,levels=monthOrder))

draw.map<-function(period){
  
  
  plot<-
    ggplot(By_month %>% filter(year_mon==period))+
    
    # add the US map
    geom_polygon(data = states 
                 ,aes(x = long, y = lat, group = group),
                 fill = "grey", color = "black") +
    
    # plot the drone sightings
    geom_point(aes(x=lng, y=lat, size=count),color="darkred")+
    
    # add density lines
    stat_density2d(aes(x = lng
                       , y = lat)
                   ,n=100
                   ,size=0.5
                   ,bins=5
                   ,color="white") + 
    
    # add a second density layer, this time with color
    stat_density2d(aes(x = lng
                       , y = lat
                       ,fill=..level..
                       ,alpha=..level..)
                   ,n=100
                   ,size=1
                   ,bins = 5
                   ,geom = "polygon") + 
    
    # various theme parameters
    scale_fill_gradient(low = "yellow", high = "red") + 
    scale_alpha_continuous(range = c(0.1,0.3))+
    coord_map(xlim=c(-130,-60),ylim=c(23,50))+
    theme_map()+
    theme(legend.position="none"
          ,title=element_text(size=15,face="bold")
          ,panel.background=element_rect(fill="black")
    )
  
  bar_plot<-
  By_month %>%
    mutate(color=ifelse(year_mon==period,"current","other")) %>%
    ggplot(aes(x=MonthLab,y=count,group=color,fill=color))+
    geom_bar(stat="identity")+
    geom_vline(xintercept=period,color="red")+
    scale_fill_fivethirtyeight()+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1,size=20)
          ,legend.position="none")+
    xlab(NULL)+ylab(NULL)
  
  text<-
  textGrob("Drone Sightings in the US"
           ,gp = gpar(fontsize=30,fontface="bold")
           )
  
  captionText<-
    textGrob("Data from http://www.faa.gov/uas/law_enforcement/uas_sighting_reports/"
             ,gp = gpar(fontsize=20,fontface="italic")
    )
    
  
  grid.arrange(text,plot,bar_plot,captionText,ncol=1,heights=c(0.5,3,0.5,0.5))
}

seq<-names(table(By_month$year_mon))

trace.animate <- function() {
  lapply(seq, function(i) {
    draw.map(i)
  })
}


library(animation)
saveGIF(trace.animate(), movie.name="DRONES.gif",ani.width = 1400, ani.height = 1400)








