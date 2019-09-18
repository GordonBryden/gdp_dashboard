library(xml2)
library(dplyr)
library(lubridate)

organisation<-"ASD015"
website<-"https://procxed.scotxed.net/procxedwebservice/ProcXedDataReturn.asmx/GetForthcomingPublications?OrganisationIDList="

df <- data.frame(
  
  publication=character(),
  date = character(),
  location = character(),
  theme = character()
  
)

xml_data <- read_xml(paste0(website,organisation))
xml_series <- xml_find_all(xml_data, ".//Series")
publication<-xml_text(xml_find_first(xml_series, ".//SeriesName"))
  
for (i in 1:length(xml_series)) {
    
    #  print(publication[i])
    
    editions<-xml_find_all(xml_series[[i]], ".//Editions")
    
    location<-xml_text(xml_find_all(editions, ".//UrlAddress"))
    
    theme<-xml_text(xml_find_all(editions, ".//Theme"))
    
    publication_dates<-xml_text(xml_find_all(editions, ".//PublicationFullDate"))
    
    
    for (x in 1:length(editions)){
      
      if (publication_dates[x] != "") {
        
        row = data.frame(publication[i],publication_dates[x],location[x],theme[x],stringsAsFactors=FALSE)
        
        df<-rbind(df,row)
        
      }
    }
}


df2<- df%>%
  rename(publication = publication.i.,
         date=publication_dates.x.,
         theme=theme.x.,
         location=location.x.
  ) %>%
  #mutate(new_date = ymd(date)) %>%
  #mutate(new_date2= if_else(is.na(new_date),
  #                          dmy(paste0("01",date)),
  #                          new_date)) %>%
  mutate(location = paste0("<a href=",location,">link</a>"))%>%
  #arrange(new_date2) %>%
  filter(publication == "Scotland's Gross Domestic Product: First Estimate") %>%
  filter(date <= Sys.Date()) %>%
  arrange(date)


release_date<- paste(
df2$date %>% last() %>% day() %>% ordinal(),
month.name[df2$date %>% last() %>% month()],
df2$date %>% last() %>% year()
)

df3<- df%>%
  rename(publication = publication.i.,
         date=publication_dates.x.,
         theme=theme.x.,
         location=location.x.
  ) %>%
  #mutate(new_date = ymd(date)) %>%
  #mutate(new_date2= if_else(is.na(new_date),
  #                          dmy(paste0("01",date)),
  #                          new_date)) %>%
  mutate(location = paste0("<a href=",location,">link</a>"))%>%
  #arrange(new_date2) %>%
  filter(publication == "Scotland's Gross Domestic Product: First Estimate") %>%
  filter(date > Sys.Date())

next_release_date<-paste(
  df3$date %>% first() %>% day() %>% ordinal(),
  month.name[df3$date %>% first() %>% month()],
  df3$date %>% first() %>% year()
)

rm(df)
