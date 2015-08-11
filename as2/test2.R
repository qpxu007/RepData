df<-read.csv("repdata-data-StormData.csv")
str(df)

s<-c("EVTYPE","FATALITIES","INJURIES",
     "PROPDMG","PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
df2 <- df[s]


trans <-function(unit) {
  unit = toupper(unit)
  nfactor = 0.0
  if (unit == "K") {
    nfactor <- 0.001
  } else if (unit == "H") {
    nfactor <- 0.0001
  } else if (unit == "M") {
    nfactor <- 1.0
  } else if (unit == "B") {
    nfactor <- 1000.0
  }
  nfactor
}

df2$PROPDMGEXP1<-sapply(df2$PROPDMGEXP,trans)
df2$PROPDMG1<- df2$PROPDMG * df2$PROPDMGEXP1

df2$CROPDMGEXP1<-sapply(df2$CROPDMGEXP,trans)
df2$CROPDMG1 <- df2$CROPDMG * df2$CROPDMGEXP1