#' Knapsack Bruteforce Approach
#' @description
#' This function implements a solution to the knapsack problem through brute force.
#' This means it calculates every single possible solution (2^n) and chooses the best.
#' This is achieved through running a loop 2^n times, 
#' converting the loop variable to a binary representation, using that to slice the 
#' elements and checking whether the solution is valid and whether it  is better than all prior solutions.
#' @param x data.frame consisting of weights and values
#' @param W size of the knapsack
#' @return optimal knapsack solution
#' @export
#' @examples
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = 16, replace = TRUE),
#'   v = runif(n = 16, min = 0, max = 10000)
#'   )
#' result <- brute_force_knapsack(x = knapsack_objects, W = 3500)

brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  # Check if all weights (w) and values (v) are greater than 0
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All weights ('w') and values ('v') must be greater than 0.")
  } 
  
  if(W <= 0){
    stop("Knapsack capacity must be larger than 0")
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

#' Knapsack Dynamic Programming Approach
#' @description
#' This solution to the knapsack problem utilizes a dynamic programming approach.
#' In this a matrix is used as a sort of look-up table. The columns correspond to the
#' knapsack size and the rows to the first 1...i items. For each item it is evaluated,
#' whether it fits in the knapsack or not, and whether it would improve on the previous,
#' best solution that would still allow it's inclusion given a certain knapsack size.
#' After the matrix is fully created the best combination can be retrieved through back-
#' tracking. The complexity of the algorithm is based on the matrix size which is n * W, 
#' hence the complexity is O(nW)
#' @param x data.frame consisting of weights and values
#' @param W size of the knapsack
#' @return optimal knapsack solution
#' @export
#' @examples
#'  knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = 16, replace = TRUE),
#'   v = runif(n = 16, min = 0, max = 10000)
#'   )
#' result <- knapsack_dynamic(x = knapsack_objects, W = 3500)
#' 

knapsack_dynamic <- function(x, W) {
  # Check if input is valid
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  # Check if all weights (w) and values (v) are greater than 0
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All weights ('w') and values ('v') must be greater than 0.")
  }
  
  if (W <= 0) {
    stop("Knapsack capacity must be larger than 0")
  }
  
  # Prune Irrelevant Items: Remove items where weight is greater than the knapsack capacity
  x <- x[x$w <= W, ]
  
  # Extract weights and values to reduce repetitive calls
  weights <- x$w
  values <- x$v
  n <- length(weights)  # Number of items
  
  # Initialize a matrix dp with 0s of dimension (n+1) x (W+1)
  dp <- matrix(0, n + 1, W + 1)
  
  # Fill the dp matrix
  for (i in 1:n) {
    for (w in 0:W) {
      if (weights[i] <= w) {
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w + 1 - weights[i]] + values[i])
      } else {
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
      w <- w - weights[i]
    }
  }
  
  # Return the maximum value and the selected elements
  return(list(value = best_value, elements = rev(elements)))
}

#' Knappsack Greedy Implementation
#' @description
#' This method implements a greedy approach to the knapsack problem.
#' It works by filling the knapsack S1 with the "best bang for the buck" (value/weight) ratio
#' items until at full capacity. It also checks whether the first not fitting item would 
#' constitute a better solution. It might not have a ratio as good as other items,
#' but might leave less empty space in the knapsack and ultimately would be a better solution than
#' S1. 
#' The algorithm does not guarantee an optimal solution, but a solution that is at least 50%
#' that of the other algorithms solutions while having a complexity of only O(nlogn).
#' @param x data.frame consisting of weights and values
#' @param W size of the knapsack
#' @return decent knapsack solution 
#' @export
#' @examples
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = 16, replace = TRUE),
#'   v = runif(n = 16, min = 0, max = 10000)
#'   )
#' result <- greedy_knapsack(x = knapsack_objects, W = 3500)

greedy_knapsack <- function(x, W) {
  # Check if input is valid
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x))) {
    stop("Input must be a data frame with columns 'w' and 'v' for weight and value respectively.")
  }
  # Check if all weights (w) and values (v) are greater than 0
  if (any(x$w <= 0) || any(x$v <= 0)) {
    stop("All weights ('w') and values ('v') must be greater than 0.")
  }
  if(W <= 0){
    stop("Knapsack capacity must be larger than 0")
  }
  
  # Keep track of original indexes
  x$original_index <- 1:nrow(x)
  
  # Calculate value-to-weight ratio
  x$ratio <- x$v / x$w
  
  # Sort items by ratio in descending order
  x <- x[order(-x$ratio), ]
  
  total_weight <- 0
  total_value <- 0
  S1 <- c()
  first_non_fitting_item <- NULL  # Track the first item that didn't fit
  
  # Iterate over list sorted by value/weight ratio. append till capacity is reached 
  # Include pre-sorting indexes in the knapsack
  
  for (i in 1:nrow(x)) {
    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      S1 <- c(S1, x$original_index[i])  # Track original index
    } else if (is.null(first_non_fitting_item)) {
      # Store the first item that didn't fit
      S2 <- x$original_index[i]
    }
  }
  
  # Check for existence of S2 (won't exist if all items fit)
  value_S2 <- if (!is.null(S2)) x$v[x$original_index == S2] else 0
  
  # Compare S1 (greedy solution) with S2 (single item that didn't fit)
  if (value_S2 > total_value) {
    return(list(value = value_S2, elements = S2))
  } else {
    return(list(value = total_value, elements = S1))
  }
}
