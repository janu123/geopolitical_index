NIFTY_AUTO$Date <- as.Date(NIFTY_AUTO$Date, format = "%d-%b-%Y")
#  Get months
NIFTY_AUTO$Month <- months(NIFTY_AUTO$Date)
#  Get years
NIFTY_AUTO$Year <- format(NIFTY_AUTO$Date,format="%y")
NIFTY_AUTO_aggr<-aggregate(Close ~ Month + Year,NIFTY_AUTO,mean)
write.csv(NIFTY_AUTO_aggr,"niftyautoaggr.csv")
write.csv(gpr_index_red1,"gprindex.csv")
nifty_auto<-ts(niftyautoaggr$Close,star=c(2004,1),freq=12)
gpr_ind<-ts(gpr_index_red1$GPRH,start = c(2004,1),freq=12)
autoplot(com_ts)
com_ts<-cbind(nifty_auto,gpr_ind)

theme_set(theme_bw())
nifty_auto %>% decompose()%>%autoplot()
gpr_ind %>% decompose()%>%autoplot()
autoplot(com_ts[,c("nifty_auto","gpr_ind")]) +
  ylab("") + xlab("Year")
com_ts %>%
  as.data.frame() %>%
  ggplot(aes(x=nifty_auto, y=gpr_ind)) +
  ylab("nifty_auto") +
  xlab("gpr_index") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
mod<-tslm(nifty_auto ~ gpr_ind, data=com_ts)
summary(mod)
checkresiduals(mod)







