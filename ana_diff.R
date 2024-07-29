require(rio)
require(dplyr)
require(stringr)
require(tidyr)
require(purrr)
require(ggplot2)
require(stats)
require(broom)
require(fs) # for creating the subfolder

source('tidy_funcs.R')

load('proc_data/dmotor_all.RData')

dmotor_all2 <- dmotor_all %>%
  select(-ID, -AGE, -SEX)

# Split the tibble into two based on the ses column
df_ses1 <- dmotor_all2 %>% filter(ses == 1) %>% select(-ses)
df_ses2 <- dmotor_all2 %>% filter(ses == 2) %>% select(-ses)

# Ensure both data frames have the same number of rows
if (nrow(df_ses1) != nrow(df_ses2)) {
  stop("The number of rows for ses=1 and ses=2 are not equal. Computation cannot proceed.")
}

# Compute the difference for each column
difference_df <- df_ses1 - df_ses2

# Perform a paired t-test for each column
t_test_results <- map_df(names(df_ses1), ~ {
  t_test <- t.test(df_ses1[[.x]], df_ses2[[.x]], paired = TRUE)
  tidy(t_test) %>%
    mutate(column = .x)
})

# Print the differences
print(difference_df)

# Print the t-test results
print(t_test_results)

# Correct for multiple comparisons using the False Discovery Rate (FDR) method
t_test_results <- t_test_results %>%
  mutate(p_adjusted = p.adjust(p.value, method = "fdr"))

# Mark significant correlations
alpha <- 0.05
t_test_results <- t_test_results %>%
  mutate(significant_adjusted = p_adjusted < alpha,
         significant_unadjusted = p.value < alpha)

# Determine the minimum significant t-value for adjusted p-values
sorted_results_adjusted <- t_test_results %>%
  arrange(abs(statistic))

threshold_t_adjusted <- sorted_results_adjusted %>%
  filter(significant_adjusted) %>%
  slice(1) %>%
  pull(statistic)

# Determine the minimum significant t-value for unadjusted p-values
sorted_results_unadjusted <- t_test_results %>%
  arrange(abs(statistic))

threshold_t_unadjusted <- sorted_results_unadjusted %>%
  filter(significant_unadjusted) %>%
  slice(1) %>%
  pull(statistic)

# Print the t-test results
print(t_test_results)

# Separate column names into vartype
t_test_results <- t_test_results %>%
  separate(column, into = c("vartype", "rest"), sep = "_", extra = "merge", remove = FALSE) %>%
  select(-rest)

# Plot the histogram of t-values
histogram <- ggplot(t_test_results, aes(x = statistic, fill = vartype)) +
  geom_histogram(binwidth = 0.5, alpha = 0.4, position = "identity") +
  geom_vline(xintercept = c(threshold_t_adjusted, -threshold_t_adjusted), linetype = "dashed", color = "red", size = 1.5) +
  geom_vline(xintercept = c(threshold_t_unadjusted, -threshold_t_unadjusted), linetype = "dashed", color = "blue", size = 1.5) +
  labs(title = "Histogram of T-Values",
       x = "T-Value",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

# Print the histogram
print(histogram)

# Filter for significant unadjusted differences
significant_unadjusted_results <- t_test_results %>%
  filter(significant_unadjusted)

# Create a combined dataset for box plots
df_combined <- bind_rows(
  df_ses1 %>% mutate(ses = "ses1"),
  df_ses2 %>% mutate(ses = "ses2")
) %>%
  gather(key = "column", value = "value", -ses)

# Filter for significant columns only
df_combined <- df_combined %>%
  filter(column %in% significant_unadjusted_results$column)

# Separate column names into vartype for plotting
df_combined <- df_combined %>%
  separate(column, into = c("vartype", "rest"), sep = "_", extra = "merge", remove = FALSE) %>%
  select(-rest)

# Custom labeller to remove the "column:" string
custom_labeller <- labeller(.cols = function(labels) {
  sub("column:", "", labels)
})

# Split the significant_unadjusted_results into chunks of 9
chunks <- split(significant_unadjusted_results, ceiling(seq_along(significant_unadjusted_results$column) / 9))

# Create the subfolder
dir_create("figures/diff/")

# Function to create box plots for each chunk
create_box_plots <- function(chunk, df_combined, t_test_results) {
  chunk_columns <- chunk$column
  df_chunk <- df_combined %>% filter(column %in% chunk_columns)
  
  # Merge chunk with t_test_results to get t-values and p-values
  chunk_with_stats <- t_test_results %>%
    filter(column %in% chunk_columns) %>%
    select(column, statistic, p.value)
  
  # Adding labels to the plot
  labels <- chunk_with_stats %>%
    mutate(label = paste0("t = ", round(statistic, 2), "\np = ", 
                          round(p.value, 4)))
  
  plot <- ggplot(df_chunk, aes(x = ses, y = value, fill = ses)) +
    geom_boxplot(alpha = 0.6) +
    facet_wrap(~ column, scales = "free", labeller = custom_labeller)+
    labs(title = "Box Plots of Unadjusted Significant Differences",
         x = "Session",
         y = "Value") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90")) +
    geom_text(data = labels, aes(x = 1.5, y = 0.95 * max(df_chunk$value), label = label), inherit.aes = FALSE, hjust = 1.1, vjust = 1.1, size = 3)
  
  return(plot)
}

# Create and save box plots for each chunk
walk2(chunks, seq_along(chunks), function(chunk, i) {
  plot <- create_box_plots(chunk, df_combined, t_test_results)
  filename <- paste0("figures/diff/box_plot_", i, ".png")
  ggsave(filename, plot = plot, width = 12, height = 8, bg = "white")
})

# Save the histogram
ggsave("figures/diff/t_histogram.png", plot = histogram, width = 12, height = 8, bg = "white")
