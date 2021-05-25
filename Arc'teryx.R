content <- read.csv("~/Downloads/Insta_2021-03-16/Content performance.csv")

# Add column for time series index (aka period number, t) to the dataframe
content$t <- c(1:43)

# Visually inspect the data for evidence of linear trend and seasonality
plot(Engagement ~ t, data= content)

## Post Day of Week
# Create seasonal dummy variables 
content$Tuesday <- ifelse(content$Weekday == "Tuesday", 1, 0)
content$Wednesday <- ifelse(content$Weekday == "Wednesday", 1, 0)
content$Thursday <- ifelse(content$Weekday == "Thursday", 1, 0)
content$Friday <- ifelse(content$Weekday == "Friday", 1, 0)
content$Saturday <- ifelse(content$Weekday == "Saturday", 1, 0)
content$Sunday <- ifelse(content$Weekday == "Sunday", 1, 0)

# Estimate the multiple regression model with time trend + seasonal dummy variables
model1 <- lm(Engagement ~ Tuesday + Wednesday + Thursday + Friday + Saturday+ Sunday + t, data = content)
summary(model1)
# Store coefficient estimates as named numeric values
b0 <- model1$coefficients[1]
b1 <- model1$coefficients[2]
b2 <- model1$coefficients[3]
b3 <- model1$coefficients[4]
b4 <- model1$coefficients[5]
b5 <- model1$coefficients[6]
b6 <- model1$coefficients[7]
bt <- model1$coefficients[8]

# Forecast 
#Monday
(b0) + bt*44 #not this # any more based on amount of rows+day of week
11.32498 
#Tuesday
(b0 + b1) + bt*44
9.464186 
#Wednesday
(b0 + b2) + bt*44
10.66259
#Thursday
(b0 + b3) + bt*44
11.31566
#Friday
(b0 + b4) + bt*44
10.66957
#Saturday
(b0 + b5) + bt*44
10.72315 
#Sunday
(b0 + b6) + bt*44
10.98885 

## Post Time of Day
library(lubridate)
#Create bins
content$time_stamp <- as.POSIXct(content$Time, format= "%H:%M")

breaks <- hour(hm("0:00","4:00","11:00","16:00","19:00", "23:59")) 
labels <- c("Night", "Morning", "Afternoon", "Evening", "Night")
content$time_of_day <- cut(x=hour(content$time_stamp), breaks=breaks, labels=labels, include.lowest = TRUE)

#Create bins

# Create seasonal dummy variables
content$Afternoon <- ifelse(content$time_of_day == "Afternoon", 1, 0)
content$Evening <- ifelse(content$time_of_day == "Evening", 1, 0)


# Estimate the multiple regression model with time trend + seasonal dummy variables
model2 <- lm(Engagement ~ Afternoon + Evening + t, data = content)
summary(model2)
# Store coefficient estimates as named numeric values
b0 <- model2$coefficients[1]
b1 <- model2$coefficients[2]
b2 <- model2$coefficients[3]
bt <- model2$coefficients[4]

# Forecast 
#Morning
(b0) + bt*44
11.16714 
#Afternoon
(b0 + b1) + bt*44
8.709459 
#Evening
(b0 + b2) + bt*44
10.6582


##Stories
story <- read.csv("~/Downloads/01_Story performance.csv")

# Add column for time series index (aka period number, t) to the dataframe
story$t <- c(1:201)

# Visually inspect the data for evidence of linear trend and seasonality
plot(Reach.avg. ~ t, data = story)

## Story Day of Week
# Create seasonal dummy variables 
story$Monday <- ifelse(story$Weekday == "Monday", 1, 0)
story$Tuesday <- ifelse(story$Weekday == "Tuesday", 1, 0)
story$Wednesday <- ifelse(story$Weekday == "Wednesday", 1, 0)
story$Thursday <- ifelse(story$Weekday == "Thursday", 1, 0)
story$Saturday <- ifelse(story$Weekday == "Saturday", 1, 0)
story$Sunday <- ifelse(story$Weekday == "Sunday", 1, 0)

# Estimate the multiple regression model with time trend + seasonal dummy variables
model3 <- lm(Reach.avg. ~ Monday + Tuesday + Wednesday + Thursday + Saturday + Sunday + t, data = story)
summary(model3)
# Store coefficient estimates as named numeric values
b0 <- model3$coefficients[1]
b1 <- model3$coefficients[2]
b2 <- model3$coefficients[3]
b3 <- model3$coefficients[4]
b4 <- model3$coefficients[5]
b5 <- model3$coefficients[6]
b6 <- model3$coefficients[7]
bt <- model3$coefficients[8]

# Forecast 
#Friday
(b0) + bt*202
288.4145 
#Monday
(b0 + b1) + bt*202
380.9916 
#Tuesday
(b0 + b2) + bt*202
327.3913
#Wednesday
(b0 + b3) + bt*202
367.5763 
#Thursday
(b0 + b4) + bt*202
357.2187
#Saturday
(b0 + b5) + bt*202
333.2605 
#Sunday
(b0 + b6) + bt*202
289.8762 

#Story Time of Day
story$time_stamp <- as.POSIXct(story$Time, format= "%H:%M")

breaks <- hour(hm("0:00","4:00","11:00","16:00","19:00", "23:59")) 
labels <- c("Night", "Morning", "Afternoon", "Evening", "Night")
story$time_of_day <- cut(x=hour(story$time_stamp), breaks=breaks, labels=labels, include.lowest = TRUE)

#Create bins

# Create seasonal dummy variables
story$Morning <- ifelse(story$time_of_day == "Morning", 1, 0)
story$Afternoon <- ifelse(story$time_of_day == "Afternoon", 1, 0)
story$Night <- ifelse(story$time_of_day == "Night", 1, 0)


# Estimate the multiple regression model with time trend + seasonal dummy variables
model4 <- lm(Reach.avg. ~ Morning+ Afternoon + Night + t, data = story)
summary(model4)
# Store coefficient estimates as named numeric values
b0 <- model4$coefficients[1]
b1 <- model4$coefficients[2]
b2 <- model4$coefficients[3]
b3 <- model4$coefficients[4]
bt <- model4$coefficients[5]

# Forecast 
#Morning
(b0 + b1) + bt*202
334.5003
#Afternoon
(b0 + b2) + bt*202
330.3991 
#Evening
(b0) + bt*202
151.8853
#Night
(b0 + b3) + bt*202
260.5222 

write.csv(content, "postdata.csv", row.names= FALSE)

