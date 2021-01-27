library(tidyverse)
library(xts)
library(ggfortify)

gpr_index_red<-gpr_index %>% select(Year,Month,GPRH)
gpr_index_red$Year<-as.integer(gpr_index_red$Year)
gpr_index_red$Year<-as.factor(gpr_index_red$Year)
gpr_index_red$Month<-as.factor(gpr_index_red$Month)
gpr_index_red1<-gpr_index_red %>% filter(Year %in% c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
                                                     "2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))
nifty_it$Date <- as.Date(nifty_it$Date, format = "%d-%b-%Y")
#  Get months
nifty_it$Month <- months(nifty_it$Date)
#  Get years
nifty_it$Year <- format(nifty_it$Date,format="%y")

#  Aggregate 'X2' on months and year and get mean
niftyit_aggr<-aggregate(Close ~ Month + Year,nifty_it,mean)
write.csv(NIFTY_IT_aggr,"niftyitaggr.csv")
write.csv(gpr_index_red1,"gprindex.csv")
nifty_it<-ts(niftyitaggr$Close,star=c(2000,1),freq=12)
gpr_ind<-ts(gpr_index_red1$GPRH,start = c(2000,1),freq=12)
autoplot(com_ts)
com_ts<-cbind(nifty_it,gpr_ind)

theme_set(theme_bw())
nifty_it %>% decompose()%>%autoplot()
gpr_ind %>% decompose()%>%autoplot()
autoplot(com_ts[,c("nifty_it","gpr_ind")]) +
  ylab("") + xlab("Year")
com_ts %>%
  as.data.frame() %>%
  ggplot(aes(x=nifty_it, y=gpr_ind)) +
  ylab("nifty_it") +
  xlab("gpr_index") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

mod<-tslm(nifty_it ~ gpr_ind, data=com_ts)
summary(mod)
