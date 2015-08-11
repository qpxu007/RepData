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

df2$PROPDMGEXP<-sapply(df2$PROPDMGEXP, trans)
df2$PROPDMG<- df2$PROPDMG * df2$PROPDMGEXP

df2$CROPDMGEXP<-sapply(df2$CROPDMGEXP, trans)
df2$CROPDMG <- df2$CROPDMG * df2$CROPDMGEXP

df2$health <- df2$INJURIES+df2$FATALITIES
df2$damage <- df2$PROPDMG+df2$CROPDMG
df2$PROPDMGEXP<- df2$CROPDMGEXP  <- NULL

df3 <- aggregate(df2[c("health","damage", "INJURIES",
                       "FATALITIES", "CROPDMG", "PROPDMG")],
                 by=list(EVTYPE=df2$EVTYPE), FUN=sum)
#df3 <- df3[order(-df3$health, -df3$damage), ]
#df3 <- df3[df3$health > 0 | df3$damage > 0, ]
#colnames(df3)[1] <- "EVTYPE"
summary(df3)


myplot<-function(df, xcol="EVTYPE", ycol="health", n=5, 
                 ylabel="Injuries and Fatalities", 
                 color="red", offset=1.5) {
  # sorted the df according the relevant ycol
  df <- df[order(-df[,ycol]), ]
  # select relevant columns and plot
  y <- df[1:n,ycol]
  x <- df[1:n,xcol]
  seqx <- seq(x)
  
  plot(seqx, log(y), type='b', xaxt='n', 
       ylab=paste("log(",ylabel,")"), xlab="", col=color)
  axis(1,at=seqx, labels=F)
  text(seqx, par("usr")[3] - 0.2, labels = x, srt = 25, pos = 1, 
       xpd = TRUE, offset=offset, cex=1)
}

par(mfrow=c(2,3))
myplot(df3,ycol='health', ylabel="Injuries and Fatalities")
myplot(df3,ycol='FATALITIES', ylabel="Fatalities")
myplot(df3,ycol='INJURIES', ylabel="Injuries")

myplot(df3,ycol='damage', ylabel="Property and Crop Damages (Mil)", color='blue')
myplot(df3,ycol='PROPDMG', ylabel="Property Damages (Mil)", color='blue')
myplot(df3,ycol='CROPDMG', ylabel="Crop Damages (Mil)", color='blue')
