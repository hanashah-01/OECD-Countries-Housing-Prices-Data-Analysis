# ------------------------------------------------------------------------------
# Project
# ------------------------------------------------------------------------------

install.packages('Amelia')
install.packages('stringr')
install.packages('caTools')
install.packages("ggplot2")
install.packages("ggheatmap")
install.packages("Metrics")

library(ggheatmap)
library(Amelia)
library(stringr)
library(caTools)
library(ggplot2)
library(Metrics)
library(MLmetrics)
library(dplyr)
library(GGally) 
#--------------------------HOUSING PRICES---------------------------------------
#--------------------------DATA CLEANING----------------------------------------
# Load the dataset 
# Set working directory to the folder that contains the dataset
setwd("C:/Users/Hana/Documents/USM/YEAR 3/CPC351/Project")

hp_og <- read.csv("Housing_Prices.csv")

# Create a backup for the data
house_price <- hp_og

# Data Pre-processing and Data Cleaning
# Display data classes
str(house_price)

# Display the summary statistics of the data
summary(house_price)

# Check for duplicates
sum(duplicated(house_price)) #0

# Check missing values
anyNA(house_price)

#display percentage of missing values in each column
(colMeans(is.na(house_price)))*100

#visualize missing values
missmap(house_price,col=c('blue','yellow'),y.at=1,y.labels='',legend=TRUE)

#drop Flag.Codes column because all values in this column are empty
house_price <- subset(house_price, select = -c(Flag.Codes))

#change column names
colnames(house_price)[1] = "Location"
colnames(house_price)[2] = "Indicator"
colnames(house_price)[3] = "Subject"
colnames(house_price)[4] = "Measure"
colnames(house_price)[5] = "Frequency"
colnames(house_price)[6] = "Time"

# check the format in the 'Time' column
sum(!grepl("^[0-9]{4}-Q[1-4]$", house_price$Time)) #all are in correct format

# check for invalid values in the 'Time' column
sum(substr(house_price$Time, 6, 6) > 4) #all are valid values

# Split 'Time' column into 'Year' and 'Quotient'
house_price[c('Year', 'Quotient')] <- str_split_fixed(house_price$Time, '-', 2)

# Create another duplicate
house_price_clone <- house_price

# Rearrange columns and remove original 'Time' column
house_price <- house_price[c('Location', 'Indicator', 'Subject', 'Measure', 'Frequency', 'Year', 'Quotient', 'Value')]

# Change data type of columns
house_price$Indicator <- as.factor(house_price$Indicator)
house_price$Measure <- as.factor(house_price$Measure)
house_price$Frequency <- as.factor(house_price$Frequency)
house_price$Year <- as.factor(house_price$Year)

# Convert the categorical variables to numeric
house_price$Location <- as.factor(as.numeric(factor(house_price$Location)))
house_price$Subject <- as.factor(as.numeric(factor(house_price$Subject)))
house_price$Quotient <- as.factor(as.numeric(factor(house_price$Quotient)))

# Drop three columns because all values in the columns are the same
house_price <- subset(house_price, select = -c(Indicator,Measure,Frequency))

# Display categorical variables in tables
table(house_price$Location) 
table(house_price$Subject) 
table(house_price$Quotient) 

# Display summary of non-categorical variables
summary(house_price$Year)
summary(house_price$Value)
#--------------------------EXPLORATORY DATA ANALYSIS----------------------------


# Filter the data to only include the columns of interest
data_filtered <- house_price[, c("Location", "Year", "Value")]

# Calculate the average HPI for each year
avg_HPI_by_year <- aggregate(Value ~ Year, data = data_filtered, FUN = mean)

# Create the barplot
ggplot(avg_HPI_by_year, aes(Year, Value)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Average HPI") +
  ggtitle("Average HPI by Year")

# Display top 10 location based on average housing price index
top10_location <- house_price_clone %>% 
  select(c("Location","Value")) %>% 
  group_by(Location) %>% 
  summarise_each(funs(mean))

# Create a barplot
price_eachloc <- top10_location %>% 
  arrange(desc(Location)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(Location,+Value),Value,fill = Value))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 10 Location") +
  coord_flip() + 
  labs(y = "Average Housing Price Index", x = "Location", fill="Average Housing Price Index") +
  geom_text(aes(label=round(Value,2)), hjust=-0.0) + theme(panel.background = element_blank())
price_eachloc

# Display bottom 10 location based on average housing price index
bottom10_location <- house_price_clone %>% 
  select(c("Location","Value")) %>% 
  group_by(Location) %>% 
  summarise_each(funs(mean))

# Create a barplot
price_eachloc2 <- bottom10_location %>% 
  arrange(desc(Location)) %>%
  top_n(-10) %>%
  ggplot(., aes(x=reorder(Location,+Value),Value,fill = Value))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Bottom 10 Location") +
  coord_flip() + 
  labs(y = "Average Housing Price Index", x = "Location", fill="Average Housing Price Index") +
  geom_text(aes(label=round(Value,2)), hjust=-0.0) + theme(panel.background = element_blank())
price_eachloc2

# Check outliers in data
boxplot(house_price$Value) # There are a few outliers in the data

# Calculate the upper and lower hinges
q1 <- quantile(house_price$Value, probs = 0.25)
q3 <- quantile(house_price$Value, probs = 0.75)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Remove the outliers
house_price_new <- house_price[house_price$Value > lower_bound & house_price$Value < upper_bound,]

boxplot(house_price_new$Value) # No outliers

#--------------------------MODEL PLANNING AND DEVELOPMENT-----------------------

str(house_price)

# Split the data into training and testing sets
set.seed(1)
sample <- sample.split(house_price_new$Location, SplitRatio = 0.8)
train  <- subset(house_price_new, sample == TRUE)
test   <- subset(house_price_new, sample == FALSE)

str(test)

# Create linear regression model
model <- lm(Value ~ ., data=train)
summary(model)

# Predict the values
predictions <- as.numeric(predict(model, test))

# Calculate Root Mean Square Error
RMSE(y_pred = predictions, y_true = test$Value) # The RMSE value is small

# Create a new dataframe with the actual values and the predictions
pred_df <- data.frame(Value = test$Value, Predictions = predictions)
    
# Plot the actual values against the predictions
ggplot(pred_df, aes(x = Predictions, y = Value)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  xlab("Predictions") +
  ylab("Actual Values")

