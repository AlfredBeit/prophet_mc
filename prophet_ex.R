library("dplyr")
library("parallel")
install.packages("prophet", type="binary")
library("prophet")


rm(list=ls())


dir<-paste0(getwd(),'/parallel/data - data.csv')

df <- read.csv(dir, header=FALSE, stringsAsFactors = FALSE)

df$V2<-(paste0(df$V2,"01"))




agg_df<-df %>%
          mutate(dates= lubridate::ymd(V2)) %>%
          group_by(dates,V1)%>%
          summarise(tot_vol = sum(V5))%>%
          ungroup()%>%
          arrange(V1,dates)

names<-base::unique(df$V1)
series <-  vector("list", length = length(names))
plots <-  vector("list", length = length(names))


names(series)<-names
names(plots)<-names

getModel<-function(s) {

  series[[s]]<- agg_df%>%
                filter(V1==names[s])%>%
                select(ds=dates,y = tot_vol)
  m <- prophet(series[[s]], 
                            daily.seasonality = FALSE, 
                            weekly.seasonality = FALSE,
                            yearly.seasonality = FALSE)
  return(m)
  
}



getForecast<-function(s, forward=5) {
  model<- getModel(s)
  future<-make_future_dataframe(model,periods=forward, freq ="m")
  fc<-predict(model, future)
  return( list(data=tail(fc[c("ds","yhat_lower", "yhat", "yhat_upper")]), 
               plot=dyplot.prophet(model,fc)) )
  }

Forecasts <- parallel::mclapply(1:5,FUN = getForecast)

Forecasts[[1]]$plot
Forecasts[[5]]$plot



