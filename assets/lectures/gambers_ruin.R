# Function to simulate a gambler's random walk (Gambler’s Ruin scenario)
random_walk <- function(start, lower, upper, price, win_probability, max_games) {
  
  # Initialize the gambler’s current position (capital)
  position <- start
  
  # Vector to record the gambler's capital at each step
  steps <- c(position)
  
  # Counter for the number of games played
  n_games <- 0
  
  # Continue playing while the gambler’s capital is between lower and upper limits
  # and the maximum number of games has not been reached
  while (position > lower && position < upper && n_games < max_games) {
    
    # Decide if the gambler wins (+1) or loses (-1) this round
    # Probability of winning is 'win_probability', losing is '1 - win_probability'
    move <- sample(c(-price, price), 1, prob = c(1 - win_probability, win_probability))
    
    # Update the gambler’s capital
    position <- position + move
    
    # Record the new position
    steps <- c(steps, position)
    
    # Increase the game counter
    n_games <- n_games + 1
  }
  
  # Return the full sequence of capital over the games played
  return(steps)
}

# Simulate one trial of the gambler’s walk:
# Start with £50, absorbing boundaries at £0 and £100,
# gambler’s probability to win each round is 1/2,
# maximum 1000 games
steps <- random_walk(50, 0, 100, 5, 1/2, 1000)

# Plot the gambler’s capital over time (games played)
plot(steps, type = "o", main = "Random Walk of Gambler",
     xlab = "Game", ylab = "Your Capital")