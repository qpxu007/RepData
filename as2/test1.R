trans <-function(unit) {
  unit = toupper(unit)
  nfactor = 0.0
  if ( unit == "K") {
    nfactor = 0.001
  } else if (unit == "M") {
    nfactor = 1.0
  } else if (unit == "B") {
    nfactor = 1000.0
  }
  nfactor
}

s<-c("EVTYPE","FATALITIES","INJURIES",
     "PROPDMG","PROPDMGEXP", "CROPDMG", "CROPDMGEXP", 
     "STATE", "BGN_DATE", "LATITUDE", "LONGITUDE")
dfx <- df[s]

# fix date
dfx$BGN_DATE<-as.Date(as.character(dfx$BGN_DATE), format="%m/%d/%Y %H:%M:%S")
# convert the lattitude and longitude data (assuming there are digital degrees)
dfx$LATITUDE<-dfx$LATITUDE*0.01
dfx$LONGITUDE<-dfx$LONGITUDE*0.01

dfx$PROPDMGEXP<-sapply(dfx$PROPDMGEXP,trans)
dfx$PROPDMG<- dfx$PROPDMG * dfx$PROPDMGEXP

dfx$CROPDMGEXP<-sapply(dfx$CROPDMGEXP,trans)
dfx$CROPDMG <- dfx$PROPDMG * dfx$PROPDMGEXP

dfx$health <- dfx$INJURIES+dfx$FATALITIES
dfx$damage <- dfx$PROPDMG+dfx$CROPDMG
dfx$PROPDMGEXP<- dfx$CROPDMGEXP  <- NULL
dfx <- dfx[dfx$health>0 & dfx$damage>0,]


df4<- aggregate(dfx[c("health","damage", "INJURIES",
                      "FATALITIES", "CROPDMG", "PROPDMG")],
                       by=list(STATE=dfx$STATE), FUN=sum)
