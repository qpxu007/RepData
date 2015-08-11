require(ggplot2)
require(ggthemes)
states_map <- map_data("state")


df4<- aggregate(dfx[c("health","damage", "INJURIES",
                      "FATALITIES", "CROPDMG", "PROPDMG")],
                by=list(region=dfx$region, STATE=dfx$STATE), FUN=sum)

ggplot(df4, aes(map_id = region)) +
  geom_map(aes(fill = health), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) 

ggplot(df4, aes(map_id = region)) +
  geom_map(aes(fill = health), map = states_map, color ="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  ggtitle("Cumulative Injuries and Fatalities by State 1950-2011")



g<-ggplot(df4, aes(map_id = region)) +
  geom_map(aes(fill = cut_number(damage, 5)), map = states_map, color ="white") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Cumulative Property and Crop by State 1950-2011") +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+coord_map()

# add labels
cnames <-aggregate(cbind(long, lat) ~ region, data = states_map, FUN = function(x) mean(range(x)))
states_full_abbrev <-cbind(tolower(state.name),state.abb)
colnames(states_full_abbrev)<-c('region','STATE')
cnames <-merge(cnames, states_full_abbrev, by=c('region'))
cnames[10, c(2:3)] <- c(-114.5, 43.5) # move label for idaho 
cnames[16, 3] <- 30.6 #LA
cnames[20, c(2:3)] <- c(-84.5, 43) # MI
cnames[8, c(2:3)] <- c(-81.5, 28) # FL
g+geom_text(data=cnames, aes(long, lat, label = STATE, map_id =NULL), size=3.5) 


myplot2<-function(df, col="damage", title="Cumulative Property and Crop Damage by State 1950-2011") {
  require(ggplot2)
  require(ggthemes)
  states_map <- map_data("state")

  # add labels
  cnames <-aggregate(cbind(long, lat) ~ region, data = states_map, FUN = function(x) mean(range(x)))
  states_full_abbrev <-cbind(tolower(state.name),state.abb)
  colnames(states_full_abbrev)<-c('region','STATE')
  cnames <-merge(cnames, states_full_abbrev, by=c('region'))
  cnames[10, c(2:3)] <- c(-114.5, 43.5) # move label for idaho 
  cnames[16, 3] <- 30.6 #LA
  cnames[20, c(2:3)] <- c(-84.5, 43) # MI
  cnames[8, c(2:3)] <- c(-81.5, 28) # FL
  
  #make plot
  ncut<- cut_number(df[,col], 5)
  g<-ggplot(df, aes(map_id = region)) +
    geom_map(aes(fill = ncut), map = states_map, color ="white") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    ggtitle(title) +
    theme(legend.position = "bottom",
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          axis.text =  element_blank()) +
    geom_text(data=cnames, aes(long, lat, label = STATE, map_id =NULL), size=3.5) +
    coord_map()
  g
}

plt1<-myplot2(df4,"health", title="Cumulative Injuries and Fatalities by State 1950-2011")
plt2<-myplot2(df4,"FATALITIES", title="Cumulative Fatalities by State 1950-2011")
plt2<-myplot2(df4,"INJURIES", title="Cumulative Injuries by State 1950-2011")
plt4<-myplot2(df4,"damage", title="Cumulative Property and Crop Damage by State 1950-2011")
plt5<-myplot2(df4,"CROPDMG", title="Cumulative Crop Damage by State 1950-2011")
plt6<-myplot2(df4,"PROPDMG", title="Cumulative Property Damage by State 1950-2011")


library(gridExtra)
grid.arrange(plt1, plt4, ncol = 1)
