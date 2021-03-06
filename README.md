# Neural-Net-NFL-Predictor

This R file trains a neural net with 6 neurons in the hidden layer and a tangent hyperbolicus activation function on NFL data from september 1950 to september 2020. 
The input data consists of opposing teams' and quarterbacks' elo ratings, which are generated by 538. 
Before passing the data to the neural network, the data is standardized by subtracting the mean and divding by the standard deviation within each column. 
After the neural network recieves standardized data and makes predictions on the validation data (october 2020-december 2021), the output is de-standardized. 
De-standardizing the model's output gives the probabilities of team 1 (the home team) winning the game. 
If the home team's win probability is larger than 50%, that team is projected to win. If less than 50%, the model picks the away team. 
The model was able to correctly pick the winning team 63.93% of the time and has an out-of-sample record of 296-167 record from 9/10/2021 to December 7, 2021. 

Data source: https://data.fivethirtyeight.com/#nfl-elo
