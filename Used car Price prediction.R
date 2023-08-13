library(rpart)
library(rpart.plot)
library(caret)
library(sqldf)
library(dplyr)
library(tidyr)
library(ggplot2)


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------Loading Data---------------------------------------------
#---------------------------------------------------------------------------------------------------------

setwd("C:\\Users\\parth\\Downloads\\Sem 1\\BA with R\\project")

og <-read.csv("vehicles.csv")

names(og)

d_subset = og[,c('region','price','year','model','condition','cylinders','fuel','odometer','title_status','transmission','drive','size','type','paint_color','state','posting_date','manufacturer')]



names(d_subset)

#freeing up space by removing and clearing the OG data set since we have already taken the required columns from the OG Data
og <- NULL;gc()

statemap <- read.csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
statemap$State.Code


#write.csv(d_subset, file="vehicles_subset.csv")

#For Github, I have uploaded a vehicles_subset.csv

#d_subset <- read.csv("vehicles_subset.csv")

#---------------------------------------------------------------------------------------------------------
#----------------------------------------Data Pre-Processing & Cleaning-----------------------------------
#---------------------------------------------------------------------------------------------------------


#Calculating Car Age from posting date and model year

class(d_subset$year)
d_subset$listyear<-as.numeric(format(as.Date(d_subset$posting_date, format="%Y-%m-%d"),"%Y"))
d_subset$age <- d_subset$listyear - d_subset$year

#removing rows having price less than 1000 and greater than 2000000, odometer & age > 0
d_subset <- d_subset %>% 
  filter(price > 1000 & price < 1000000 & odometer >= 0 & odometer < 200000 & age >=0 & age <=50)


#Converting Odometer to multiple of 1000 Miles

d_subset$odometer_1k <- d_subset$odometer/1000

#Excluding rows having NULL AGE or NULL Odometer value

d_subset <- subset(d_subset, is.na(d_subset$age) == FALSE )
d_subset <- subset(d_subset, is.na(d_subset$odometer_1k) == FALSE )


#Converting Cylinders from alphanumeric to Numeric
d_subset$cylinders_f <- as.numeric(gsub("\\D", "", d_subset$cylinders));unique(d_subset$cylinders_f)


#d_subset <- d_subset_copy
#removing Duplicates
d_subset <- distinct(d_subset)

#removing those models having less than 50 entries
d_subset <- d_subset %>%
  group_by(model) %>%
  filter(n() >= 50 | model != '') %>%
  ungroup()

#check for missing values in each column
colSums(is.na(d_subset))
100*colSums(d_subset == '')/length(d_subset$price)

#Clubbing condition

table(d_subset$condition)

length(is.na(d_subset$condition))

d_subset$new_condition <- ifelse(d_subset$condition %in% c("good", "excellent"), "good"
                                 , (ifelse(d_subset$condition %in% c("new","like new"), "new"
                                           , ifelse(d_subset$condition %in% c("fair","salvage"), "fair_salvage","NA"
                                           ))))

unique(d_subset[,c("new_condition","condition")])


#Adding Region based on State
state_indices <- match(d_subset$state, tolower(statemap$State.Code))
d_subset$StateRegion <- statemap$Region[state_indices]


#Assigning NA to missing values for various columns
d_subset$model[d_subset$model == ''] <- NA
d_subset$condition[d_subset$condition == ''] <- NA
d_subset$cylinders[d_subset$cylinders == ''] <- NA
d_subset$fuel[d_subset$fuel == ''] <- NA
d_subset$title_status[d_subset$title_status == ''] <- NA
d_subset$transmission[d_subset$transmission == ''] <- NA
d_subset$drive[d_subset$drive == ''] <- NA
d_subset$size[d_subset$size == ''] <- NA
d_subset$type[d_subset$type == ''] <- NA
d_subset$paint_color[d_subset$paint_color == ''] <- NA
d_subset$state[d_subset$state == ''] <- NA
d_subset$state[d_subset$StateRegion == ''] <- NA
d_subset$manufacturer[d_subset$manufacturer == ''] <- NA

#Creating factors for categorical variables

d_subset$model <- as.factor(d_subset$model)
d_subset$condition <- as.factor(d_subset$condition)
d_subset$cylinders <- as.factor(d_subset$cylinders)
d_subset$fuel <- as.factor(d_subset$fuel)
d_subset$title_status <- as.factor(d_subset$title_status)
d_subset$transmission <- as.factor(d_subset$transmission)
d_subset$drive <- as.factor(d_subset$drive)
d_subset$size <- as.factor(d_subset$size)
d_subset$type <- as.factor(d_subset$type)
d_subset$paint_color <- as.factor(d_subset$paint_color)
d_subset$state <- as.factor(d_subset$state)
d_subset$StateRegion <- as.factor(d_subset$StateRegion)
d_subset$manufacturer <- as.factor(d_subset$manufacturer)
d_subset$year <- as.factor(d_subset$year)
d_subset$new_condition <- as.factor(d_subset$new_condition)





#---------------------------------------------------------------------------------------------------------
#--------------------------------------------Data Exploration---------------------------------------------
#---------------------------------------------------------------------------------------------------------


#Data Exploration using sqldf library

sqldf('Select model, count(1) as c, min(price) as min_price, max(price) as max_price from d_subset group by model order by c desc')
sqldf('Select manufacturer, count(1) as c, min(price) as min_price, max(price) as max_price from d_subset group by manufacturer order by c desc')
sqldf('Select StateRegion, count(1) as c, min(price) as min_price, max(price) as max_price from d_subset group by StateRegion order by c desc')

sqldf('Select * from d_subset where model = \'f-150\' order by price desc')


# Calculate the average price by make
avgprice <- d_subset %>%
  group_by(manufacturer) %>%
  summarize(avg_price_1k = mean(price)/1000
            , max_price_1k = max(price)/1000
            , min_price_1k = min(price)/1000
            , count = n()) %>%
  top_n(40, count) %>%
  arrange(desc(avg_price_1k))

ggplot(avgprice, aes(x = manufacturer, y = avg_price_1k)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Average Price (1k)" )

ggplot(avgprice, aes(x = manufacturer, y = avg_price_1k)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Average Price (1k)")

#visualizing

#Filtering Car Model == Camry for understanding trends, and correlation

complete_vehicles_subset <- d_subset %>% filter(model == 'f-150')

# Create a scatter plot of price vs. age
ggplot(complete_vehicles_subset, aes(x = age, y = price)) +
  geom_point()

# Create a scatter plot of price vs. odometer
ggplot(complete_vehicles_subset, aes(x = odometer/1000, y = price)) +
  geom_point()

# Looking at correlation for numerical variables
#install.packages("ggcorrplot")
library(ggcorrplot)

corr <- cor(complete_vehicles_subset %>% select(price, age, odometer))
ggcorrplot(corr, lab = TRUE, title = "Correlation Matrix")



#check for outliers

ggplot(complete_vehicles_subset, aes(x = "", y = price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Prices")

ggplot(complete_vehicles_subset, aes(x = year, y = price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(complete_vehicles_subset, aes(x = year, y = price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Age")

ggplot(complete_vehicles_subset, aes(x = "", y = odometer)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Odometer")

ggplot(complete_vehicles_subset, aes(x = cylinders, y = price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Cylinders")

ggplot(complete_vehicles_subset, aes(x = new_condition, y = price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Condition")
ggplot(complete_vehicles_subset, aes(x = title_status, y = price)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Condition")


ggplot(complete_vehicles_subset, aes(x = year, y = odometer_1k)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Car Age")

#plot(complete_vehicles_subset)



#---------------------------------------------------------------------------------------------------------
#----------------------------------------------Model Building---------------------------------------------
#---------------------------------------------------------------------------------------------------------
d_subset_temp <- d_subset



model_counts <- sort(table(d_subset$model), decreasing = TRUE)
top_50_models <- names(model_counts)[1:100]
d_subset_top_50 <- d_subset[d_subset$model %in% top_50_models, ]

options(scipen = 10)

#generic Model
lin <- lm(price ~ model+age+odometer_1k+I(age^2)+odometer_1k+age:odometer_1k+new_condition+title_status+StateRegion+size+type+paint_color , data = d_subset_top_50);print(summary(lin))


#function for running regression for a car model

model_regression_evaluation <- function(model_check){
  
  # Subset data for the input Car Model
  d_subset_check <- subset(d_subset, d_subset$model == model_check)
  
  linearreg_pre <- lm(price ~ age+odometer_1k , data = d_subset_check);print(summary(linearreg_pre))
  
  # Calculate the 2 percentile values
  pct_2 = quantile(d_subset_check$price, c(0.02, 0.98))
  
  # Remove the rows based on the calculated percentiles
  d_subset_check = d_subset_check[d_subset_check$price >= pct_2[1] & d_subset_check$price <= pct_2[2], ]
  
  linearreg <- lm(price ~ age+odometer_1k , data = d_subset_check);print(summary(linearreg))
  linearreg2 <- lm(price ~ age+I(age^2)+odometer_1k , data = d_subset_check);print(summary(linearreg2))
  linearreg3 <- lm(price ~ age+I(age^2)+odometer_1k+age:odometer_1k , data = d_subset_check);print(summary(linearreg3))

  linearreg4 <- lm(price ~ age+I(age^2)+odometer_1k+age:odometer_1k+new_condition , data = d_subset_check);print(summary(linearreg4))
  linearreg5 <- lm(price ~ age+I(age^2)+odometer_1k+age:odometer_1k+fuel , data = d_subset_check);print(summary(linearreg5))
   #p<- data.frame(age = 5, odometer_1k = 100)
  
  #pred <- predict(linearreg, p)
  #print(summary(linearreg))
  #print("\\Predicted value\\")
  #return(pred)
  #return (c(linearreg, linearreg2))
  
}

#Calling function with input variable as Car Model
model_regression_evaluation('f-150')




#Final Model
#function for running regression for a car model

model_regression <- function(model_check, x, y){
  
  # Subset data for the input Car Model
  d_subset_check <- subset(d_subset, d_subset$model == model_check)
  
  # Calculate the 2 percentile values
  pct_2 = quantile(d_subset_check$price, c(0.02, 0.98))
  
  # Remove the rows based on the calculated percentiles
  d_subset_check = d_subset_check[d_subset_check$price >= pct_2[1] & d_subset_check$price <= pct_2[2], ]
  
  finalmodel <- lm(price ~ age+I(age^2)+odometer_1k+age:odometer_1k , data = d_subset_check);
  
  #price -> Age + Age^2 + Odometer + Age*Odometer
  
  print(summary(finalmodel))
  p<- data.frame(age = x, odometer_1k = y)
  
  pred <- predict(finalmodel, p)
  
  print(paste("For a car having age =", x, "and Miles Driven (1k) =", y,", Predicted value = $", (format(pred, trim = TRUE))));

}


model_regression('f-150', 6,100);

#Insights from the Model

model_regression('f-150', 1,100);
model_regression('f-150', 2,100);
model_regression('f-150', 3,100);
model_regression('f-150', 4,100);
model_regression('f-150', 5,100);
model_regression('f-150', 6,100);
model_regression('f-150', 7,100);
model_regression('f-150', 8,100);
model_regression('f-150', 9,100);


model_regression('mustang', 10,100);


price_100k <- c(38939.24,36418.51,33990.35,31654.77,29411.76,27261.33,25203.48,23238.2,21365.5,19585.38)
Age_Years <- c(1,2,3,4,5,6,7,8,9,10)
plot(Age_Years,price_100k)


model_regression('f-150', 5,10);
model_regression('f-150', 5,20);
model_regression('f-150', 5,30);
model_regression('f-150', 5,40);
model_regression('f-150', 5,50);
model_regression('f-150', 5,60);
model_regression('f-150', 5,70);
model_regression('f-150', 5,80);
model_regression('f-150', 5,90);
model_regression('f-150', 5,100);


price_5year <- c(35524.76,34845.54,34166.32,33487.09,32807.87,32128.65,31449.43,30770.21,30090.99,29411.76)
miles <- c(10,20,30,40,50,60,70,80,90,100);
plot(miles,price_5year)