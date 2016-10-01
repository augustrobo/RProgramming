library(datasets)
data(iris)
# In this dataset, what is the mean of 'Sepal.Length' for the species virginica?
with(iris, tapply(Sepal.Length, Species, mean))
# setosa versicolor  virginica 
# 5.006      5.936      6.588 


###########################################################################

library(datasets)
data(mtcars)
# How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?

tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars, tapply(mpg, cyl, mean))
# 4        6        8 
# 26.66364 19.74286 15.10000 

###########################################################################

# What is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?

with(mtcars, tapply(hp, cyl, mean))
# 4         6         8 
# 82.63636 122.28571 209.21429 

