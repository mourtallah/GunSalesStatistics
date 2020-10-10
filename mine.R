library(dplyr, mask.ok = T)
library(ggplot2)
library(lubridate, mask.ok = T); library(dplyr, mask.ok = T); library(ggplot2, mask.ok = T)
library(reshape2)

data <- read.csv("./GunSales.csv", col.names = c("Date", "Sales"))

## supplant data point from authentic set
data = rbind(data, c("2019/01/15", 1039.19))
data$Sales <- as.numeric(data$Sales)
## round to nearest hundredth 
data$Sales <- round(data$Sales, 2)
# saved copy of X2, stabilize day as X2'
convdate <- ymd(data$Date); day(convdate) <- 15;
# update main_df
cbind(convdate, data) -> data;


# find zeros, x-axis 
#data[data$Sales<100,2] <- 0;

# reading in and binding actual dates taken from Statistica graph 
TotalActualSales <- c(1039.19, NA, 1394.02,1049.4,NA,973.5,NA,1160.55,NA, 1159.28, 1423.87,1237.53,1671.77,1357.65,2538.23,1797.91,NA,2387.52,1960,1780)

## handle<-data.frame(dist.date=unique(data$convdate), realsales=TotalActualSales)

#Saves total associated with each y/m pair
#data %>% group_by(convdate) %>% summarize(MonthlyTotalSales=max(Sales)) %>% arrange(desc(convdate)) %>% as.data.frame() -> Totals



# sort by date and sales 
data %>% arrange(convdate, desc(Sales)) %>% as.data.frame() -> data

## CHECKPOINT ##
head(data) # sorted by convenv, sales


## op1
# indices for transformation
indexfirst <- seq(1,80, 4) 
indexsecond <- seq(1, 80, 4)+1
indexthird <- seq(1,80,4)+2
indexfourth <- seq(1, 80,4)+3

# split groups by type
totalunits <- data[indexfirst,]$Sales
other_guns <- totalunits - data[indexsecond,]$Sales
long_guns <- totalunits - other_guns - data[indexthird,]$Sales
hand_guns <- totalunits - long_guns - other_guns


#bymonthyear <- split.data.frame(data, data$convdate) this will split the df into a list and I don't feel like bothering in a list right now. 


## Checkpoint ## 

data_frame("Date"=names(bymonthyear), totalunits, other_guns, long_guns, hand_guns) %>% as.data.frame() -> op2neat

plot(as.Date(op2neat$Date), op2neat$totalunits, main="Monthly unit sales of firearms in the United States")


apply(op3neat,1, t) -> op4neat 
data.frame(op4neat, cat=c("total","other","long","handgun")) -> op5neat
names(op5neat)<-c(names(bymonthyear),"cat")

data
cbind(data,rep(c('total','handgun','longguns','other'),20))

data<-melt(op5neat, id.vars = c("cat")) 

## CHECKPOINT ##
names(data)[2] <- "date"
data

ggplot(data, aes(fill=cat, y=value, x=date)) + geom_bar(position = "stack", stat = "identity")
ggplot(data, aes(fill=cat, y=value, x=date)) +
  geom_bar(position='dodge', stat="identity") + 
  facet_wrap(~cat)

ggplot(data, aes(y=value, x=date, color=cat)) + geom_line()  + geom_point()

ggplot(data, aes(y=value, x=date, group=cat, color=cat)) + geom_line(linetype="dashed") + geom_point() 

last_year <- data[year(as.Date(data$date))=="2019",]
this_year <- data[year(as.Date(data$date))=="2020",]

ggplot(this_year, aes(y=value, x=date, group=cat, color=cat)) + geom_line(linetype="solid") + geom_point() 

this_year %>% filter(cat=="handgun") %>% ggplot(aes(date, value)) + geom_line() + geom_point()
                                       
                                       