library(RCurl)
library(jsonlite)
library(tidyverse)
library(gridExtra)

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
  
(
  mydf<-myexfn("CH") %>% 
    dplyr::filter(CH > 100) %>%
    dplyr::mutate(
      Label=format(Date,"%m-%d"),
      t=1:n(),
      dCH=CH-dplyr::lag(CH,1),
      gr=dCH/CH,
      egr=c(NA,base::diff(log(CH)))
    )
)

summary(myfit<-lm(log(CH)~t,data=mydf))

mydf$fit<-exp(myfit$fitted.values)
mydf$dfit<-c(NA,diff(mydf$fit))
mydf
             
mydbl<-log(2)/myfit$coefficients[2]

#library(scales)
#show_col(hue_pal()(4))

P1<-mydf %>%
  ggplot() +
  geom_point(aes(x=t,y=CH),color="#F8766D") + 
  geom_point(aes(x=t,y=fit),color="#00BFC4") +
  geom_line(aes(x=t,y=fit),color="#00BFC4") +
  annotate("text", x=5, y=5000, label=sprintf("Doubling time: %.2f days",mydbl))

P2 <- mydf %>% 
  dplyr::select(Label,dCH,dfit) %>% 
  tidyr::gather(Variable,Value,-Label) %>%
  ggplot(aes(x=Label,y=Value,fill=Variable)) +
  geom_bar(stat="identity",position=position_dodge()) +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))


grid.arrange(P1,P2)


jnk<-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

