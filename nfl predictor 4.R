# The purpose of this R script is to predict the winners of NFL games.
# Data was obtained from 538's elo ratings. 
library(ggplot2)
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


#neural net

library(neuralnet)

train2 <- train[c(7,8,9,10,17,18,34)]
valid2 <- valid[c(7,8,9,10,17,18,34)]

#Function to normalize each column of the data 

standard <- function(x){
    (x - mean(x))/sd(x)
}

# Normalizing each column in the training and validation data

train3 <- apply(train2, 2, FUN = standard)
valid3 <- apply(valid2,2,standard)
colnames(train3)[c(1,2,3,4,5,6)] <- c('elo1pre','elo2pre',  'eloprob1',  'eloprob2',  'qb1valuepre', 'qb2valuepre')
colnames(valid3)[c(1,2,3,4,5,6)] <- c('elo1pre','elo2pre',  'eloprob1',  'eloprob2',  'qb1valuepre', 'qb2valuepre')

# Running different neural nets
f <- formula(winner~ elo1pre + elo2pre + eloprob1 + eloprob2 + qb1valuepre +qb2valuepre)

net1 <- neuralnet(f, data = train3, hidden = c(3), threshold = 0.1)
net2 <- neuralnet(f, data = train3, hidden = c(6), threshold = 0.1)
net3 <- neuralnet(f, data = train3, hidden = c(6), threshold = 0.1, act.fct = "tanh" )
net4 <- neuralnet(f, data = train3, hidden = c(), threshold = 0.1)


output2 <- compute(net2, valid3)
output1 <- compute(net1, valid3)
output3 <- compute(net3, valid3)
outputresult<- output2$net.result

df <- data.frame(output1$net.result, output2$net.result, output3$net.result, outputresult)


#denormalizing the data 

destandard <- function(y){
    (y * sd(train2$winner)) + mean(train2$winner)}
destandard(outputresult)


#Measuring the mean-squared error 

mean((destandard(outputresult) - valid2$winner)^2)^.5

# Putting Results in a data frame

validResults <- data.frame(date = valid$date, actualwinner = valid$winner, winprobability =  destandard(outputresult))
validResults$winner <- ifelse(validResults$winprobability >= 0.5, 1, 0)

#Measuring accuracy of predictions 
validResults$correct <- ifelse(validResults$actualwinner==validResults$winner, 1,0)
sum(validResults$correct)/nrow(validResults)

final_valid_results <- data.frame(date = validResults$date, team1 = valid$team1, team2 = valid$team2, output = validResults$winner, predictedWinner = validResults$actualwinner)




# predicting future games
nfl3 <- read.csv('C:/Users/kduba/OneDrive/Desktop/projects/nfl_latest2.csv')
nfl3$date <- as.Date(nfl3$date, '%Y-%m-%d')

current <- subset(nfl3,  date == '2021-12-02' | date == '2021-12-05' |  date == '2021-12-06')


current2 <- current[c(7,8,9,10,17,18,33)]

current3 <- apply(current2,2,standard)
colnames(current3)[c(1,2,3,4,5,6)] <- c('elo1pre','elo2pre',  'eloprob1',  'eloprob2',  'qb1valuepre', 'qb2valuepre')

currentOutput <- compute(net3, current3)
currentResult <- currentOutput$net.result


currentPredictions<- data.frame(date = current$date, team1 = current$team1, team2 = current$team2 , winprobability =  destandard(currentResult))
currentPredictions$winner <- ifelse(currentPredictions$winprobability >= 0.5, 1, 0)



