# Brute-force knapsack
brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  
  n <- nrow(x)  # Number of items
  max_value <- 0
  best_combination <- c()
  
  for (i in 0:(2^n - 1)) {
    elements <- as.logical(intToBits(i)[1:n])
    total_weight <- sum(x$w[elements])
    total_value <- sum(x$v[elements])
    
    if (total_weight <= W && total_value > max_value) {
      max_value <- total_value
      best_combination <- which(elements)
    }
  }
  
  return(list(value = max_value, elements = best_combination))
}

# Create a dataset of 16 items
knapsack_objects <- data.frame(
  w = sample(1:4000, size = 16, replace = TRUE),
  v = runif(n = 16, min = 0, max = 10000)
)

# Measure the time it takes to run the algorithm for 16 objects
timing <- system.time({
  result <- brute_force_knapsack(x = knapsack_objects, W = 3500)
})

# Print the result and the time taken
print(result)
print(timing)



# Dynamic Programming knapsack
knapsack_dynamic <- function(x, W) {
  # Check if input is valid
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  
  n <- nrow(x)  # Number of items
  # Initialize a matrix dp with 0s of dimension (n+1) x (W+1)
  dp <- matrix(0, n + 1, W + 1)
  
  # Fill the dp matrix
  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] <= w) {
        # Maximize the value by either including the item or not
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w + 1 - x$w[i]] + x$v[i])
      } else {
        # Item can't be included because it's too heavy
        dp[i + 1, w + 1] <- dp[i, w + 1]
      }
    }
  }
  
  # Backtrack to find which items to include in the knapsack
  best_value <- dp[n + 1, W + 1]
  elements <- c()
  w <- W
  
  for (i in n:1) {
    if (dp[i + 1, w + 1] != dp[i, w + 1]) {
      elements <- c(elements, i)
      w <- w - x$w[i]
    }
  }
  
  # Return the maximum value and the selected elements
  return(list(value = best_value, elements = rev(elements)))
}

# Example usage:
knapsack_objects <- data.frame(
  w = sample(1:4000, size = 500, replace = TRUE),
  v = runif(n = 500, min = 0, max = 10000)
)

timing <- system.time({result <- knapsack_dynamic(x = knapsack_objects, W = 3500)})

print(result)
print(timing)
