library("dplyr")
library("parallel")
#install.packages("prophet", type="binary")
library("prophet")


rm(list=ls())

dir<-paste0(getwd(),'/parallel/data - data.csv')

df <- read.csv(dir, header=FALSE, stringsAsFactors = FALSE)
names_df<-c("client_id", "yearmon","receiver_id", "sender_id" , "volume")
names(df)<-names_df

df$yearmon<-(paste0(df$yearmon,"01")) # add day to yearmon dates


# tot_vol is the volume of orders summed monthly over each client_id
agg_df<-df %>%
  mutate(dates= lubridate::ymd(yearmon)) %>%
  group_by(dates,client_id)%>%
  summarise(tot_vol = sum(volume))%>%
  ungroup()%>%
  arrange(client_id,dates)

# create a fake dict with each position of the list being ("key", "value") pair
# and ("key", "value") being (unique client_id, list containing the corresponding time-series)

names_dict<-base::unique(df$client_id)
series <-  vector("list", length = length(names_dict))
names(series)<-names_dict



getModel<-function(s) {

  # fill in the "dict"
  series[[s]]<- agg_df %>%
    filter(client_id==names_dict[s]) %>%
    select(ds=dates,y = tot_vol)
  
  # create the fbprohet model object
  m <- prophet(series[[s]], daily.seasonality = FALSE, 
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

#examples of interactive dyplots

Forecasts[[1]]$plot
Forecasts[[5]]$plot


