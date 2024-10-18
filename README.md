# Advanced-Programming-R-Lab-6
Lab 6

# Set seed and version for RNG to allow for reproducability
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

knapsack_objects <- data.frame(
  w = sample(1:4000, size = 16, replace = TRUE),
  v = runif(n = 16, min = 0, max = 10000)
)

timing <- system.time({
  result <- brute_force_knapsack(x = knapsack_objects, W = 3500)
})

# Print the result and the time taken
print(result)
print(timing)

# Example usage:
knapsack_objects <- data.frame(
  w = sample(1:4000, size = 500, replace = TRUE),
  v = runif(n = 500, min = 0, max = 10000)
)

timing <- system.time({result <- knapsack_dynamic(x = knapsack_objects, W = 3500)})

print(result)
print(timing)

#sample data frame with 1,000,000 objects
n <- 1000000
knapsack_objects_greedy <- data.frame(
  w = sample(1:100, n, replace = TRUE),  # Random weights
  v = sample(1:1000, n, replace = TRUE)  # Random values
)


# Call the greedy_knapsack function
timing <- system.time({result <- greedy_knapsack(knapsack_objects_greedy[1:800, ], W = 3500)})

# Print the result
print(result)
print(timing)

