
# Assuming your tibbles are named tibble1 and tibble2

list_mismatch_coln <- function(tibble1, tibble2){
# Get the column names of each tibble
columns_tibble1 <- colnames(tibble1)
columns_tibble2 <- colnames(tibble2)

# Find columns that are in tibble1 but not in tibble2
mismatch_tibble1 <- setdiff(columns_tibble1, columns_tibble2)

# Find columns that are in tibble2 but not in tibble1
mismatch_tibble2 <- setdiff(columns_tibble2, columns_tibble1)

# Combine mismatches into a single list or data frame for easier inspection
mismatched_columns <- list(
  in_tibble1_not_in_tibble2 = mismatch_tibble1,
  in_tibble2_not_in_tibble1 = mismatch_tibble2
)

# Print mismatched columns
print(mismatched_columns)
}


# Function to compute correlation and p-value
compute_correlation <- function(col1, col2) {
  test <- cor.test(col1, col2, use = "complete.obs")
  tibble(
    correlation = test$estimate,
    p_value = test$p.value
  )
}
