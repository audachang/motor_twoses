library(rio)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(stats)
library(broom)
library(fs) # for creating the subfolder

source('tidy_funcs.R')

load('proc_data/dmotor_all.RData')

dmotor_all2 <- dmotor_all %>%
  select(-ID, -AGE, -SEX)

# Split the tibble into two based on the ses column
df_ses1 <- dmotor_all2 %>% filter(ses == 1) %>% select(-ses)
df_ses2 <- dmotor_all2 %>% filter(ses == 2) %>% select(-ses)

# Ensure both data frames have the same number of rows
if (nrow(df_ses1) != nrow(df_ses2)) {
  stop("The number of rows for ses=1 and ses=2 are not equal. Correlation cannot be computed.")
}

# Compute correlations and p-values
correlation_results <- map2_df(names(df_ses1), names(df_ses2), ~ {
  result <- compute_correlation(df_ses1[[.x]], df_ses2[[.y]])
  result <- result %>%
    mutate(column = .x)
  result
})

# Correct for multiple comparisons using the False Discovery Rate (FDR) method
correlation_results <- correlation_results %>%
  mutate(p_adjusted = p.adjust(p_value, method = "fdr"))

# Mark significant correlations
alpha <- 0.05
correlation_results <- correlation_results %>%
  mutate(significant_adjusted = p_adjusted < alpha,
         significant_unadjusted = p_value < alpha)

# Assuming correlation_results is your tibble
correlation_results <- correlation_results %>%
  separate(column, into = c("vartype", "rest"), sep = "_", extra = "merge", remove = FALSE) %>%
  select(-rest)

# Determine the minimum significant correlation coefficient for adjusted p-values
sorted_results_adjusted <- correlation_results %>%
  arrange(abs(correlation))

threshold_correlation_adjusted <- sorted_results_adjusted %>%
  filter(significant_adjusted) %>%
  slice(1) %>%
  pull(correlation)

# Determine the minimum significant correlation coefficient for unadjusted p-values
sorted_results_unadjusted <- correlation_results %>%
  arrange(abs(correlation))

threshold_correlation_unadjusted <- sorted_results_unadjusted %>%
  filter(significant_unadjusted) %>%
  slice(1) %>%
  pull(correlation)

# Print the results
print(sorted_results_adjusted)

# Plot the histogram of correlation coefficients
histogram <- ggplot(correlation_results, aes(x = correlation, fill = vartype)) +
  geom_histogram(binwidth = 0.05, alpha = 0.4) +
  geom_vline(xintercept = c(threshold_correlation_adjusted, -threshold_correlation_adjusted), linetype = "dashed", color = "red", size = 1.5) +
  geom_vline(xintercept = c(threshold_correlation_unadjusted, -threshold_correlation_unadjusted), linetype = "dashed", color = "blue", size = 1.5) +
  labs(title = "Histogram of Correlation Coefficients",
       x = "Correlation Coefficient",
       y = "Frequency") +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

# Print the histogram
print(histogram)
filename <- "figures/corr/histogram.png"
ggsave(filename, plot = histogram, width = 12, height = 8)

# Filter for significant correlations
significant_results <- correlation_results %>%
  filter(significant_adjusted)

# Split significant_results into chunks of 9
chunks <- split(significant_results, ceiling(seq_along(significant_results$column) / 9))

# Function to create scatter plots for each chunk
create_scatter_plots <- function(chunk, df_ses1, df_ses2, correlation_results) {
  scatter_plots <- map2_df(chunk$column, chunk$column, ~ {
    tibble(
      x = df_ses1[[.x]],
      y = df_ses2[[.y]],
      column = .x
    )
  })
  
  scatter_plots <- scatter_plots %>%
    left_join(correlation_results, by = "column")
  
  plot <- ggplot(scatter_plots, aes(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    facet_wrap(~ column, scales = "free", labeller = labeller(column = label_both)) +
    geom_text(aes(label = paste("r =", round(correlation, 2))), x = Inf, y = Inf, vjust = 1, hjust = 1, size = 3, color = "red", data = scatter_plots %>% group_by(column) %>% summarise(correlation = unique(correlation))) +
    labs(title = "Scatterplots of Significant Correlations",
         x = "ses=1",
         y = "ses=2") +
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"))
  
  return(plot)
}

# Create the subfolder
dir_create("corr")

# Create and save scatter plots for each chunk
walk2(chunks, seq_along(chunks), function(chunk, i) {
  plot <- create_scatter_plots(chunk, df_ses1, df_ses2, correlation_results)
  filename <- paste0("figures/corr/plot_", i, ".png")
  ggsave(filename, plot = plot, width = 12, height = 8)
})
