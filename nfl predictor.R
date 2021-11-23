# The purpose of this R script is to predict the winners of NFL games.
# Multivariate Adaptive Regression Splines and Random Forest models were compared.
# Random Forest outperforms the MARS model because it has a lower out of sample MSE. 
# Data was obtained from 538's elo ratings. 

#----------------------------------------Data Cleaning-------------------------------

#Importing 538's elo ratings

nfl <- read.csv('C:/Users/kduba/OneDrive/Desktop/projects/NFL.csv')

#Converting strings to dates 

nfl$date <- as.Date(nfl$date, '%m/%d/%Y')

# Creating a dummy variable that takes 1 if team 1 wins and 0 if team 2 wins. 

nfl$winner <- ifelse(nfl$score1 > nfl$score2,1,0)

#Removing NA values (there are some games that resulted in a tie, a rare outcome)
nfl2<- nfl[!is.na(nfl$winner), ]

# Removing values before the 1950 season, as quarterback information is not available before then


nfl2 <- subset(nfl2, date >= '1950-09-16')

# Training Data 

train <- subset(nfl2, date < '2020-09-10')

#Validation Data

valid <- subset(nfl2, date >= '2020-09-10')

#----------------------------Developing and Evaluating Predictive Models-----------

# Random Forest

library(randomForest)

#Training the model on games from 1950-2020
#The model is a classification random forest with 3000 trees 

rand1 <- randomForest(factor(winner) ~ neutral + elo1_pre + elo2_pre +
                         elo_prob1 + elo_prob2 + qb1_value_pre + qb2_value_pre, ntree = 3000, data = train)
rand1

#Making predictions using the validation data 

predictions <- predict(rand1, valid)

#Comparing the predicted winners to the actual winners 

finaldf1 <- data.frame(date = valid$date, result = valid$winner, estimate = predictions)

#Calculating the Mean Squared Error (MSE) on the validation data 

sum(finaldf1$result==finaldf1$estimate)/nrow(finaldf1)

#Multivariate Adaptive Regression Spline (MARS)

library(earth)

#Training MARS model 

earth1 <- randomForest(factor(winner) ~ neutral + elo1_pre + elo2_pre +
                          elo_prob1 + elo_prob2 + qb1_value_pre +
                          qb2_value_pre, degree = 2, data = train)

#Making predictions using the validation data

predictions2 <- predict(earth1, valid)


#Comparing the predicted winners to the actual winners 

finaldf2 <- data.frame(date = valid$date, result = valid$winner, estimate = predictions2)

#Calculating the Mean Squared Error (MSE) on the validation data

sum(finaldf2$result==finaldf2$estimate)/nrow(finaldf2)



# The Random Forest Model has a lower MSE in the validation data than MARS
# Training the MARS model on the full data set

rand1.1 <- randomForest(factor(winner) ~ neutral + elo1_pre + elo2_pre +
                          elo_prob1 + elo_prob2 + qb1_value_pre + qb2_value_pre, ntree = 3000, data = nfl2)
rand1.1

#Importing data on upcoming NFL games 

nfl3 <- read.csv('C:/Users/kduba/OneDrive/Desktop/projects/NFL_latest.csv')

#Subset to get days of interest. 
#Will update this each week to make predictions for upcoming games

test1 <- subset(nfl3, date == '11/22/2021' |  date == '11/25/2021' )

# Predicting games for the week 

test1_predictions <- predict(rand1.1, test1)

#Putting Predictions in a dataframe and exporting to Excel

test1df <- data.frame(test1$date, test1$team1, test1$team2, test1_predictions)
write.csv(test1df, file = 'C:\\Users\\kduba\\OneDrive\\Desktop\\projects\\test1.csv')
