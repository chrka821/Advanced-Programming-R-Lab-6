#' @description
#' This function implements a solution to the knapsack problem through brute force.
#' This means it calculates every single possible solution (2^n) and chooses the best.
#' This is achieved through running a loop 2^n times, 
#' converting the loop variable to a binary representation, using that to slice the 
#' elements and checking whether the solution is valid and whether it  is better than all prior solutions.
#' @param x data.frame consisting of weights and values
#' @param W size of the knapsack
#' @return optimal knapsack solution
brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  # Check if all weights (w) and values (v) are greater than 0
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All weights ('w') and values ('v') must be greater than 0.")
  }
  
  
  n <- nrow(x)  # Number of items
  max_value <- 0
  best_combination <- c()
  
  for (i in 0:(2^n - 1)) {
    elements <- as.logical(intToBits(i)[1:n]) # Convert i to binary representation used for slicing
    total_weight <- sum(x$w[elements])
    total_value <- sum(x$v[elements])
    
    if (total_weight <= W && total_value > max_value) {
      max_value <- total_value
      best_combination <- which(elements)
    }
  }
  
  return(list(value = max_value, elements = best_combination))
}


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


#' @description
#' This solution to the knapsack problem utilizes a dynamic programming approach.
#' In this a matrix is used as a sort of look-up table. The columns correspond to the
#' knapsack size and the rows to the first 1...i items. For each item it is evaluated,
#' whether it fits in the knapsack or not, and whether it would improve on the previous,
#' best solution that would still allow it's inclusion given a certain knapsack size.
#' After the matrix is fully created the best combination can be retrieved through back-
#' tracking. The complexity of the algorithm is based on the matrix size which is n * W, 
#' hence the complexity is O(nW)
#' @param name description
#' @param x data.frame consisting of weights and values
#' @param W size of the knapsack
#' @return optimal knapsack solution

knapsack_dynamic <- function(x, W) {
  # Check if input is valid
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  # Check if all weights (w) and values (v) are greater than 0
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All weights ('w') and values ('v') must be greater than 0.")
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


greedy_knapsack <- function(x, W) {
  # Check if input is valid
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  # Check if all weights (w) and values (v) are greater than 0
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All weights ('w') and values ('v') must be greater than 0.")
  }
  
  # Calculate value-to-weight ratio
  x$ratio <- x$v / x$w
  
  # Sort items by ratio in descending order
  x <- x[order(-x$ratio), ]
  
  total_weight <- 0
  total_value <- 0
  elements <- c()
  
  # Add items to the knapsack until the capacity is reached
  for (i in 1:nrow(x)) {
    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      elements <- c(elements, i)  # Store the item index
    }
  }
  
  return(list(value = total_value, elements = elements))
}

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

