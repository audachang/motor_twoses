require(rio)
require(dplyr)
require(stringr)
require(tidyr)
require(purrr)
require(ggplot2)
require(stats)
require(broom)

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

# Compute the correlation between ses-01 values and the difference values
correlation_results <- map_df(names(df_ses1), ~ {
  cor_test <- cor.test(df_ses1[[.x]], difference_df[[.x]])
  tibble(
    column = .x,
    correlation = cor_test$estimate,
    p_value = cor_test$p.value
  )
})

# Apply FDR to the p-values
correlation_results <- correlation_results %>%
  mutate(p_adjusted = p.adjust(p_value, method = "fdr"))

# Mark significant correlations
alpha <- 0.05
correlation_results <- correlation_results %>%
  mutate(significant_adjusted = p_adjusted < alpha, 
         significant_unadjusted = p_value < alpha)

correlation_results <- correlation_results %>%
  separate(column, into = c("vartype", "rest"), sep = "_", extra = "merge", remove = FALSE) %>%
  select(-rest)

# Determine the minimum significant correlation coefficient for adjusted p-values
sorted_results_adjusted <- correlation_results %>%
  arrange(desc(p_value))

threshold_correlation_adjusted <- sorted_results_adjusted %>%
  filter(significant_adjusted) %>%
  slice(1) %>%
  pull(correlation)

# Determine the minimum significant correlation coefficient for unadjusted p-values
sorted_results_unadjusted <- correlation_results %>%
  arrange(desc(p_value))

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
filename <- "figures/corr_diff/histogram.png"
ggsave(filename, plot = histogram, width = 12, height = 8)


# Filter for significant results
significant_results <- correlation_results %>%
  filter(significant_adjusted)

# Split significant_results into chunks of 9
chunks <- split(significant_results, 
                ceiling(seq_along(significant_results$column) / 9))

# Custom labeller to remove the "column:" string
custom_labeller <- labeller(.cols = function(labels) {
  sub("column:", "", labels)
})

# Function to create scatter plots for each chunk
create_scatter_plots <- function(chunk, df_ses1, difference_df, correlation_results) {
  scatter_plots <- map2_df(chunk$column, chunk$column, ~ {
    tibble(
      x = df_ses1[[.x]],
      y = difference_df[[.y]],
      column = .x
    )
  })
  
  scatter_plots <- scatter_plots %>%
    left_join(correlation_results, by = "column")
  
  plot <- ggplot(scatter_plots, aes(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    facet_wrap(~ column, scales = "free", labeller = custom_labeller) +
    geom_text(data = scatter_plots %>% 
                group_by(column) %>% 
                summarise(correlation = unique(correlation), p_value = unique(p_value)),
              aes(label = paste("r =", round(correlation, 2), "\np =", round(p_value, 4))), 
              x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3, color = "red", inherit.aes = FALSE) +
    labs(title = "Scatterplots of Significant Correlations",
         x = "ses=1",
         y = "Difference (ses=1 - ses=2)") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"))
  
  return(plot)
}

# Create the subfolder
dir_create("figures/corr_diff")

# Create and save scatter plots for each chunk
walk2(chunks, seq_along(chunks), function(chunk, i) {
  plot <- create_scatter_plots(chunk, df_ses1, difference_df, correlation_results)
  filename <- paste0("figures/corr_diff/plot_", i, ".png")
  ggsave(filename, plot = plot, width = 12, height = 8)
})