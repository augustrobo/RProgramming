library(datasets)
data = airquality

# 1. What are the column names of the dataset?
names(data)
# [1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day" 

colnames(data)
# [1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day" 

# 2. Extract the first 2 rows of the data frame and print them to the console.
data[1:2, ]
#   Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2

head(data, 2)
#   Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2

# 3. How many observations(i.e. rows) are in the data frame?
dim(data)[1] 
# [1] 153

nrow(data) 
# [1] 153

# 4. Extract the last 2 rows of the data frame and print them to the console.
tail(data, 2)
# Ozone Solar.R Wind Temp Month Day
# 152    18     131  8.0   76     9  29
# 153    20     223 11.5   68     9  30

# 5. What is the value of Ozone in the 47th row?
data$Ozone[47] 
# [1] 21

# 6. How many missing values are in the Ozone column of this data frame?
sum(is.na(data$Ozone)) 
# [1] 37

# 7. What is the mean of the Ozone column in this dataset? 
mean(data$Ozone, na.rm = TRUE) 
# [1] 42.12931

# 8. Extract the subset of the rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
mean(data$Solar.R[(data$Ozone > 31) & (data$Temp > 90)], na.rm = TRUE) 
# [1] 212.8

# 9. What is the mean of Temp when Month is equal to 6?
mean(data$Temp[data$Month == 6], na.rm = TRUE) 
# [1] 79.1

# 10. What was the maximum ozone value in the month of May(i.e. Month is equal to 5)?
max(data$Ozone[data$Month == 5], na.rm = TRUE) 
# [1] 115
