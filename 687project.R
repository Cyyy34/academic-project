#IST 687 Project
#  ________                            ________  
# /  _____/______  ____  __ ________   \_____  \ 
#/   \  __\_  __ \/  _ \|  |  \____ \    ____\  \   
#\    \_\  \  | \(  <_> )  |  /  |_> >   \____   \       
# \______  /__|   \____/|____/|   __/     ____\   \
#        \/                   |__|        \________\   
####################################
#Read the raw data of Satisfaction Survey.
library(readxl)
Satisfaction_Survey_2 <- read_excel("C:/Users/123/Desktop/Satisfaction Survey 2.xlsx")
#Exlude space and "%" sign in variable names
colnames(Satisfaction_Survey_2) <- gsub(" ","",colnames(Satisfaction_Survey_2))
colnames(Satisfaction_Survey_2) <- gsub("%","",colnames(Satisfaction_Survey_2))
options(digits=3)
options(scipen=200)
####################################
#Data clean
#Count rows with NA.
rows <- sum(complete.cases(Satisfaction_Survey_2))
na_rows <- sum(!complete.cases(Satisfaction_Survey_2))
print(paste("Rows with NA =",na_rows))
#Calculate Percent of rows with NA
print(paste("Percent of rows with NA =",100*na_rows/rows,"%"))
#Calculate NA rows that is caused by cancelled flight.
na_rows_cancelled <- nrow(Satisfaction_Survey_2[is.na(Satisfaction_Survey_2$Flighttimeinminutes)&Satisfaction_Survey_2$Flightcancelled=="Yes",])
print(paste("Rows with NA caused by cancelled flight=",na_rows_cancelled))
#Calculate Percent of rows with NA caused by cancelled flight
print(paste("Percent of rows with NA caused by cancelled flight =",100*na_rows_cancelled/na_rows,"%"))
#Set NAs to 0 when the flight is cancelled.
Satisfaction_Survey_2[is.na(Satisfaction_Survey_2$Flighttimeinminutes)&Satisfaction_Survey_2$Flightcancelled=="Yes",26] <- 0
Satisfaction_Survey_2[is.na(Satisfaction_Survey_2$DepartureDelayinMinutes)&Satisfaction_Survey_2$Flightcancelled=="Yes",23] <- 0
Satisfaction_Survey_2[is.na(Satisfaction_Survey_2$ArrivalDelayinMinutes)&Satisfaction_Survey_2$Flightcancelled=="Yes",24] <- 0
#Delete rest of rows with NA
Satisfaction_Survey_2 <- na.omit(Satisfaction_Survey_2)
####################################
#Make histogram and boxplot plots of customer satisfaction.
count <- Satisfaction_Survey_2 %>% 
  group_by(Satisfaction) %>% 
  summarise(count=n())
count
library(ggplot2)
ggplot(count,aes(x=Satisfaction,y=count))+
  geom_col(aes(fill=Satisfaction),color="black")+
  ggtitle("Bar of Customer Satisfaction")
####################################
#Summary descriptive of numeric variables
#Resource:https://stackoverflow.com/questions/20997380/creating-a-summary-statistical-table-from-a-data-frame
library(dplyr)
num <-select_if(Satisfaction_Survey_2,is.numeric)
library(fBasics)
sum <- basicStats(num)[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs"),]
sum <- as.data.frame(t(sum))
sum

####################################

library(dplyr)
library(ggplot2)
Satisfaction_Survey_2_bi <- Satisfaction_Survey_2
Satisfaction_Survey_2_bi$Satisfaction <- ifelse(Satisfaction_Survey_2_bi$Satisfaction<3.38,"Not Satisfied","Satisfied")

count2 <- Satisfaction_Survey_2_bi %>% 
  group_by(Satisfaction) %>% 
  summarise(count=n())

count2[1,2] <- 63421/(63421+66130)
count2[2,2] <- 66130/(66130+63421)

#Pie Chart
#Resource:http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
ggplot(count2, aes(x="", y=count, fill=Satisfaction))+
  geom_bar(width = 1, stat = "identity",col="black")+
  scale_fill_manual(values=c("orange", "steelblue"))+
  coord_polar("y", start=0)+
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = count/3 + c(0, cumsum(count)[-length(count)]), label = percent(1-count)), size=5)

#Resource:https://zhuanlan.zhihu.com/p/27093478
ggplot(Satisfaction_Survey_2_bi, aes(Age)) +
  geom_histogram(aes(fill=Satisfaction), binwidth = 4, col="black", size=1)+
  scale_fill_manual(values=c("orange", "steelblue"))+
  ggtitle("Age Histogram")

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

ggplot(Satisfaction_Survey_2_bi,aes(x=Gender))+
  geom_bar(aes(fill=Satisfaction),width=0.5,col="black")+
  aes(reorder_size(Gender))+
  scale_fill_manual(values=c("orange", "steelblue"))
xlab("Gender")+
  ggtitle("Bar Chart of Gender")


ggplot(Satisfaction_Survey_2_bi,aes(x=Class))+
  geom_bar(aes(fill=Satisfaction),width=0.5,col="black")+
  aes(reorder_size(Class))+
  scale_fill_manual(values=c("orange", "steelblue"))+
  xlab("Class")+
  ggtitle("Bar Chart of Class")

ggplot(Satisfaction_Survey_2_bi,aes(x=AirlineStatus))+
  geom_bar(aes(fill=Satisfaction),width=0.5,col="black")+
  aes(reorder_size(AirlineStatus))+
  scale_fill_manual(values=c("orange", "steelblue"))+
  xlab("Airline Status")+
  ggtitle("Bar Chart of Airline Status")

ggplot(Satisfaction_Survey_2_bi,aes(x=TypeofTravel))+
  geom_bar(aes(fill=Satisfaction),width=0.5,col="black")+
  aes(reorder_size(TypeofTravel))+
  scale_fill_manual(values=c("orange", "steelblue"))+
  xlab("Type of Travel")+
  ggtitle("Bar Chart of Type of Travel")

ggplot(Satisfaction_Survey_2_bi,aes(x=Flightcancelled))+
  geom_bar(aes(fill=Satisfaction),width=0.5,col="black")+
  aes(reorder_size(Flightcancelled))+
  scale_fill_manual(values=c("orange", "steelblue"))+
  xlab("Flight Cancelled")+
  ggtitle("Bar Chart of Flight Cancelled")

ggplot(Satisfaction_Survey_2_bi,aes(x=ArrivalDelaygreater5Mins))+
  geom_bar(aes(fill=Satisfaction),width=0.5,col="black")+
  aes(reorder_size(ArrivalDelaygreater5Mins))+
  scale_fill_manual(values=c("orange", "steelblue"))+
  xlab("ArrivalDelaygreater5Mins")+
  ggtitle("Bar Chart of ArrivalDelaygreater5Mins")
####################################
#Make correlation plot of numeric variables.
#Resource:http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
library(ggcorrplot)
num <-select_if(Satisfaction_Survey_2,is.numeric)
corr <- cor(num)
ggcorrplot(corr)
####################################
#Do the map
#Resource:Introduction to Data Science
library(ggplot2)
library(ggmap)
library(dplyr)
survey_map_origin <- Satisfaction_Survey_2[c(1,19)]
survey_map_origin <- survey_map_origin %>% 
  group_by(OriginState) %>% 
  summarise(average_sat=mean(Satisfaction))
survey_map_origin <-survey_map_origin[-45,]
survey_map_origin <- survey_map_origin[order(survey_map_origin$average_sat),]


ggplot(survey_map_origin,aes(x=reorder(OriginState,-average_sat),y=average_sat))+
  geom_col(fill="orange",color="black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("States")+ylab("Average Satisfaction")+ggtitle("Distribution of Average Satisfaction")+
  theme(plot.title = element_text(hjust = 0.5))

state_data <- data.frame(state.area , state.center , state.name)
names(state_data)[names(state_data)=="state.name"] <- "OriginState"
survey_map_origin <- merge(survey_map_origin , state_data )

us <- map_data("state")
colnames(survey_map_origin)[1] <- 'state'
colnames(survey_map_origin)[4] <- 'lon'
colnames(survey_map_origin)[5] <- 'lat'
survey_map_origin$state <- tolower(survey_map_origin$state)

map.sat <- ggplot(survey_map_origin, aes (map_id = state)) +
  geom_map(map = us , aes(fill = average_sat))+
  expand_limits(x = us$long , y = us$lat)+
  coord_map()+
  ggtitle("Average Satisfaction of Origin States")+
  scale_fill_gradient(low = 'orange', high = 'steelblue')+
  borders("state",colour = "black") 


map.sat 


survey_map_des <- Satisfaction_Survey_2[c(1,21)]
survey_map_des <- survey_map_des %>% 
  group_by(DestinationState) %>% 
  summarise(average_sat=mean(Satisfaction))
survey_map_des <-survey_map_des[-45,]
survey_map_des <- survey_map_des[order(survey_map_des$average_sat),]
tail(survey_map_des)


ggplot(survey_map_des,aes(x=reorder(DestinationState,-average_sat),y=average_sat))+
  geom_col(fill="steelblue",color="black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Destination States")+ylab("Average Satisfaction")+ggtitle("Distribution of Average Satisfaction Over Destination States")+
  theme(plot.title = element_text(hjust = 0.5))

survey_map_des <- merge(survey_map_des , state_data )

us <- map_data("state")
colnames(survey_map_des)[1] <- 'state'
colnames(survey_map_des)[4] <- 'lon'
colnames(survey_map_des)[5] <- 'lat'
survey_map_des$state <- tolower(survey_map_des$state)
survey_map_des <-survey_map_des[-45,]

map.sat <- ggplot(survey_map_des, aes (map_id = state)) +
  geom_map(map = us , aes(fill = average_sat))+
  expand_limits(x = us$long , y = us$lat)+
  coord_map()+
  ggtitle("Average Satisfaction of Destination States")+
  scale_fill_gradient(low = 'red', high = 'steelblue')+
  borders("state",colour = "black") 

map.sat 


####################################
#Do the linear regression model.
Satisfaction_Survey_2$DepartureDelaygreater5mins <- ifelse(Satisfaction_Survey_2$DepartureDelayinMinutes>5,"yes","no")
lm1 <- lm(Satisfaction~AirlineStatus+Age+Gender+PriceSensitivity+NoofFlightsp.a.+TypeofTravel
          +No.ofotherLoyaltyCards+ShoppingAmountatAirport+EatingandDrinkingatAirport+
            +Class+DayofMonth+ScheduledDepartureHour+YearofFirstFlight+DepartureDelaygreater5mins
          +DepartureDelayinMinutes+ArrivalDelayinMinutes+Flightcancelled+Flighttimeinminutes+
            +FlightDistance+ArrivalDelaygreater5Mins,data=Satisfaction_Survey_2)
summary(lm1)

#Linear regression model after excluding variables that are not significant.
lm2 <- lm(Satisfaction~AirlineStatus+Age+Gender+PriceSensitivity+NoofFlightsp.a.+TypeofTravel
          +ShoppingAmountatAirport+EatingandDrinkingatAirport+
            +Class+ScheduledDepartureHour+YearofFirstFlight
          +Flightcancelled+DepartureDelaygreater5mins
          +ArrivalDelaygreater5Mins,data=Satisfaction_Survey_2)
summary(lm2)
####################################
#Associated rules
#Resource:Introduction to Data Science
#Converse data to category factors
category_1 <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  vBuckets
}
survey_category_1 <- sapply(Satisfaction_Survey_2[c(3,6,7,8,10,11,12,14,22,23,24,26,27)],category_1)

category_2 <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3.38] <- "High"
  vBuckets[vec < 3.38] <- "Low"
  vBuckets
} 

category_3 <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 1.275] <- "High"
  vBuckets[vec < 1.275] <- "Low"
  vBuckets
} 

survey_category_2 <- sapply(Satisfaction_Survey_2[1],category_2)
survey_category_3 <- sapply(Satisfaction_Survey_2[5],category_3)
survey_category_4 <- sapply(Satisfaction_Survey_2[c(2,4,9,13,16,17,25,28)],as.factor)
survey_category <- data.frame(cbind(survey_category_1,survey_category_2,survey_category_3,survey_category_4))
library(arules)
library(arulesViz)
#Transact dataset to transactions
survey_categoryX <- as(survey_category,"transactions")
#Plot the item frequency plot
rules<- apriori(survey_categoryX,parameter = list(supp=0.3, conf=0.3),appearance = list(rhs="Satisfaction=Low",default="lhs"))
inspect(rules)
rules.sorted <- sort(rules, decreasing=TRUE, by="lift")
inspect(rules.sorted)
rules.sorted.3 <- head(rules.sorted,3)
#Plot the rule plot
plot(rules.sorted.3,method="graph",main="Top 3 Rulesets of Low Satisfaction")

rules2<- apriori(survey_categoryX,parameter = list(supp=0.3, conf=0.5),appearance = list(rhs="Satisfaction=High",default="lhs"))
rules.sorted2 <- sort(rules2, decreasing=TRUE, by="lift")
inspect(rules.sorted2)
rules.sorted.3 <- head(rules.sorted2,3)
plot(rules.sorted.3,method="graph",main="Top 3 Rulesets of High Satisfaction")

library(dplyr)
sou <- survey_category %>% 
  filter(AirlineName=="Southeast Airlines Co.")
sou <- sou[-c(20,21)]
survey_categoryX <- as(sou,"transactions")


rules<- apriori(survey_categoryX,parameter = list(supp=0.3, conf=0.4),appearance = list(rhs="Satisfaction=Low",default="lhs"))
inspect(rules)
rules.sorted <- sort(rules, decreasing=TRUE, by="lift")
inspect(rules.sorted)
rules.sorted.3 <- head(rules.sorted,3)
#Plot the rule plot
plot(rules.sorted.3,method="graph",main="Top 3 Rulesets of Low Satisfaction for Southeast Airlines")

rules2<- apriori(survey_categoryX,parameter = list(supp=0.3, conf=0.6),appearance = list(rhs="Satisfaction=High",default="lhs"))
rules.sorted2 <- sort(rules2, decreasing=TRUE, by="lift")
inspect(rules.sorted2)
rules.sorted.3 <- head(rules.sorted2,3)
plot(rules.sorted.3,method="graph",main="Top 3 Rulesets of High Satisfaction ")


####################################
#Logistic Regression
#Resource:https://stats.idre.ucla.edu/r/dae/logit-regression/
Satisfaction_Survey_2$DepartureDelaygreater5mins <- ifelse(Satisfaction_Survey_2$DepartureDelayinMinutes>5,"yes","no")
Satisfaction_Survey_2_glm <- Satisfaction_Survey_2
Satisfaction_Survey_2_glm$Satisfaction <- ifelse(Satisfaction_Survey_2_glm$Satisfaction<3.38,1,0)
glm <- glm(Satisfaction~AirlineStatus+Age+Gender+PriceSensitivity+NoofFlightsp.a.+TypeofTravel
           +No.ofotherLoyaltyCards+ShoppingAmountatAirport+EatingandDrinkingatAirport+
             +Class+DayofMonth+ScheduledDepartureHour+YearofFirstFlight+DepartureDelaygreater5mins
           +DepartureDelayinMinutes++ArrivalDelayinMinutes+Flightcancelled+Flighttimeinminutes+
             +FlightDistance+ArrivalDelaygreater5Mins,data=Satisfaction_Survey_2_glm,family = "binomial")
summary(glm)
####################################
#Data mining using SVM
#Resource:Introduction to Data Science
#Divide dataset into train data and test data.
Satisfaction_Survey_2_SVM <- Satisfaction_Survey_2[sample(5000),]
Satisfaction_Survey_2_SVM$Satisfaction <- ifelse(Satisfaction_Survey_2_SVM$Satisfaction<3.38,"Low","High")
randindex <- sample(1:dim(Satisfaction_Survey_2_SVM)[1])
cut_point2_3 <- floor((2*dim(Satisfaction_Survey_2_SVM)[1])/3)
traindata <- Satisfaction_Survey_2_SVM[randindex[1:cut_point2_3],]
testdata <- Satisfaction_Survey_2_SVM[randindex[(cut_point2_3+1):dim(Satisfaction_Survey_2_SVM)[1]],]
#Do the SVM
library(kernlab)
svmoutput <- ksvm(Satisfaction~ArrivalDelaygreater5Mins+AirlineStatus+TypeofTravel+Gender+Flightcancelled+Class,data=traindata,kernal="rbfdot",kapr="automatic",C=5,cross=3,prob.model=T)
svmpred <- predict(svmoutput,testdata,type="votes")
table <- data.frame(testdata$Satisfaction,svmpred[1,])
table(table)
print(paste("Error rate=",(80+288)*100/1667,"%"))
####################################
