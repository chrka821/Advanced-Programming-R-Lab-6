# Brute-force knapsack solution
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
