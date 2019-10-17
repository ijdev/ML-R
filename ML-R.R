## BIG MART PROBLEM ANALYSIS COMPETION

path <- "C:/Users/Lenovo/Desktop/Data"
setwd(path)
library(ggplot2)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

dim(train)
dim(test)

head(train) 
head(test)

#check the variables and their types in train
str(train)

#check missing values
table(is.na(train))

#check the variables in which these values are missing
colSums(is.na(train))

summary(train)

#Bivariate analysis is done with two variables. sales and visibility
# it's clear there is alot of products are 0 visibilty !
# most prodcut are less than 0.2 visibilty
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + 
  geom_point(size = 1.5, color="navy") + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") + 
  ggtitle("Item Visibility vs Item Outlet Sales")

# bar chart of which outlet ha the highest sales
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()

#Item type vs outlet sales
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + 
  geom_bar( stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + 
  ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

ggplot(train, aes(Outlet_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Outlet Type") + ylab("Item Outlet Sales")+ggtitle("Outlet Type vs Sales")
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")


# compinenig the two frames because they both have the smae problem.
test$Item_Outlet_Sales <-  1 # original test file is missin g the last column.
combi <- rbind(train, test)

dim(combi)

table(is.na(combi))
# Impute missing value by median.
# I'm using median because it is known to be highly robust to outliers.
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

# is.na(combi$Item_Weight)


# solving the 0 visibilty problem of some of the items.
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility) 

# we have four size "",High,Medium, Small.
# so the fist level need to be fixed
levels(combi$Outlet_Size)[1] <- "Other"


# solving the Fat contetn problem:
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat","low fat" = "Low Fat", "reg" = "Regular"))
## THIS WORKS TOO: levels(combi$Item_Fat_Content)[1] = "Low Fat"

## 4:

library(dplyr)
# pass the df and the variable to arrange by
Outlet_Count <- tally(group_by(combi,Outlet_Identifier))

#a <- combi %>% group_by(Outlet_Identifier) %>% tally()
# google count() vs tally()

names(Outlet_Count)[2] = "Outlet_Count"
# join by the oID same as in db.
combi <- full_join(Outlet_Count, combi, by = "Outlet_Identifier") 

item_count <- tally(group_by(combi,ï..Item_Identifier))
names(item_count)[2] = "Item_Count"
combi <- full_join(item_count, combi, by = "ï..Item_Identifier") 



#Outlet_Year <- combi%>%
 # select(Outlet_Establishment_Year) %>% 
  #mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)
# this works too, if there no name with Outlet_Year 
# then this code will create a new column wiht that name
Outlet_Year=combi$Outlet_Year <- 2013 - combi$Outlet_Establishment_Year


# R scirpt produced this: Joining, by = "Outlet_Establishment_Year"
combi <- full_join(Outlet_Year,combi)

#combi <- combi[,-13] # remove the original 
combi <- select(combi,-c(Outlet_Establishment_Year))



q <- substr(combi$ï..Item_Identifier,1,2) #index 1 and the lenght is 2
q <- gsub("FD","Food",q) # gsub replace FD to Food, in a giving string
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)

combi$Item_Type_New <- q


## encoding

# ITEM FAT CONTENT
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

# OUTLET LOCATION TYPE
sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
combi <- cbind(combi,demo_sample)



# outlet size type " Outher - High - Medium - Small "

sample2 <- select(combi,Outlet_Size)
demo_sample2 <- data.frame(model.matrix(~.-1,sample2))
combi <- cbind(combi,demo_sample2)
 
# Item type new  3 level food drinks non-consumable
sample3 <- select(combi,Item_Type_New)
demo_sample3 <- data.frame(model.matrix(~.-1,sample3))
combi <- cbind(combi,demo_sample3)

 
# Outlet Type 4 
sample4 <- select(combi,Outlet_Type)
demo_sample4 <- data.frame(model.matrix(~.-1,sample4))
combi <- cbind(combi,demo_sample4)

#library(dummies)
#combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')

combi <- select(combi,-c(ï..Item_Identifier,
                          Item_Type_New,Outlet_Type,
                          Outlet_Size,Item_Type,
                          Outlet_Location_Type,Outlet_Identifier))



# train and test
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]
new_train <- select(new_train,-c(outlet_count)) # rmse is down by 100!
new_train <- select(new_train,-c(Item_Fat_Content)) 

linear_model <- lm(Item_Outlet_Sales ~ . , data = new_train)

library(Metrics)
# fitted values of the sales
# rmse is diffrence between the actual and predicted so the lower rmse is better.
rmse(new_train$Item_Outlet_Sales, (linear_model$fitted.values))



#####################33333333333333333333333333333333333333333333333333333333333


#create a new variable in test file
test$Item_Outlet_Sales <- 1

#combine train and test data
combi <- rbind(train, test)

#impute missing value in Item_Weight
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
#impute 0 in item_visibility
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),                         combi$Item_Visibility)

#rename level in Outlet_Size
levels(combi$Outlet_Size)[1] <- "Other"

#rename levels of Item_Fat_Content
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat","low fat" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

q <- substr(combi$ï..Item_Identifier,1,2) #index 1 and the lenght is 2
q <- gsub("FD","Food",q) # gsub replace FD to Food, in a giving string
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)

combi$Item_Type_New <- q

# Item type new  3 level food drinks non-consumable
sample3 <- select(combi,Item_Type_New)
demo_sample3 <- data.frame(model.matrix(~.-1,sample3))
combi <- cbind(combi,demo_sample3)


# Outlet Type 4 
sample4 <- select(combi,Outlet_Type)
demo_sample4 <- data.frame(model.matrix(~.-1,sample4))
combi <- cbind(combi,demo_sample4)


# outlet sizs
sample2 <- select(combi,Outlet_Size)
demo_sample2 <- data.frame(model.matrix(~.-1,sample2))
combi <- cbind(combi,demo_sample2)

#create a new column 2013 - Year
combi$Year <- 2013 - combi$Outlet_Establishment_Year

#drop variables not required in modeling
library(dplyr)

combi <- select(combi,-c(ï..Item_Identifier,Item_Type_New,
                          Outlet_Identifier,Outlet_Location_Type,
                          Outlet_Establishment_Year,Outlet_Size,
                          Item_Type,Outlet_Type))

#combi <- combi[,-1]
#combi <- combi[,-7]
#combi <- combi[,-11]
#combi <- combi[,-13]
#combi <- combi[,-5]
#combi <- combi[,-8]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# norm price - weight - year 
combi$Item_MRP <- normalize(combi$Item_MRP)
combi$Item_Weight <- normalize(combi$Item_Weight)
combi$Year <- normalize(combi$Year)


new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]



#linear regression
linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
summary(linear_model)
rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))


linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)
rmse(new_train$Item_Outlet_Sales, linear_model$fitted.values)






#### rmse 1009


train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$Item_Outlet_Sales <- 1

combi <- rbind(train, test)

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)

levels(combi$Outlet_Size)[1] <- "Other"

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat","low fat" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

q <- substr(combi$ï..Item_Identifier,1,2) #index 1 and the lenght is 2
q <- gsub("FD","Food",q) # gsub replace FD to Food, in a giving string
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
combi$Item_Type_New <- q

new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
summary(linear_model)
rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))






