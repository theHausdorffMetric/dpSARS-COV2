library(RCurl)
library(jsonlite)
library(tidyverse)

#
covid_df<-jsonlite::fromJSON(RCurl::getURL("https://coronavirus-tracker-api.herokuapp.com/all"))


myexfn<-function(country_code){
  myn<-which(covid_df$confirmed$locations$country_code==country_code)
  crapdates<-names(covid_df$confirmed$locations$history[myn[1],]) %>% as.Date(format="%m/%d/%y")
  cvalues<-as.numeric(base::colSums(covid_df$confirmed$locations$history[myn,]))
  retdat<-data.frame(Date=crapdates,Cvalues=cvalues) %>% 
    dplyr::arrange(Date)
  colnames(retdat)<-c("Date",country_code)
  return(retdat)
}

if(FALSE){
  purrr::map(.x=list("CH","IT","DE","FR","GR"),.f=myexfn) %>%
    reduce(.f=dplyr::full_join,by="Date") 
  
  jnk<-which(covid_df$confirmed$locations$country_code=="GB")
  covid_df$confirmed$locations$province[jnk]
}


purrr::map(.x=list("CH","IT","DE","FR","GB","ES"),.f=myexfn) %>%
  reduce(.f=dplyr::full_join,by="Date") %>%
  tidyr::gather(Country,Value,-Date) %>% 
  ggplot(aes(x=Date,y=Value,colour=Country)) +
  geom_point() +
  geom_line() + 
  scale_y_log10() +
  labs(x=NULL,y="Confirmed cases (log scale)",
       title="COVID-19 confirmed cases (CSSE John Hopkins)") +
  theme(legend.position="bottom")

purrr::map(.x=list("CH","IT","DE","FR","GB","ES","US"),.f=myexfn) %>%
  reduce(.f=dplyr::full_join,by="Date") %>% tail(10)
  
  
(my.df<-data.frame(t=1:10,y=tail(myexfn("CH"),10)[,2]))


