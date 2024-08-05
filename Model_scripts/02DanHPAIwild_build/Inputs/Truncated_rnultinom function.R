# Load the required library
library(mvtnorm)

# Function to generate truncated multinomial samples
truncated_rmultinom <- function(n, size, prob, max_values) {
  # Generate the multinomial samples
  samples <- rmultinom(n, size, prob)
  
  # Truncate the values based on the specified maximum values
  truncated_samples <- pmin(samples, max_values)
  
  return(truncated_samples)
}







