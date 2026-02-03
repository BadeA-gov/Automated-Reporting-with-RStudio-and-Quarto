#source code of functions created during workshop.

#example function that does addition
function_name <- function(argument1, argument2) { #1. Arguments
  result <- argument1 + argument2 #2. Body
  return(result) } #Return Value (Output)

# Define the function to summarize fish stats
summarize_fish_stats <- function(df, group_var) {
  summary_table <- df %>%
    group_by({{ group_var }}) %>%
    summarise(
      count = n(),
      min_length = min(Total_Length, na.rm = TRUE),
      avg_length = mean(Total_Length, na.rm = TRUE),
      max_length = max(Total_Length, na.rm = TRUE)
    ) %>%
    arrange(desc(avg_length))
  return(summary_table)
}

#Function to plot fish lenghs
plot_fish_lengths <- function(df, group_var) {
  var_name <- rlang::as_label(rlang::enquo(group_var))
  p <- ggplot(df, aes(x = factor({{ group_var }}), y = Total_Length, fill = factor({{ group_var }}))) +
    geom_boxplot() +
    labs(title = "Length Distribution", x = var_name, y = "Total Length (mm)") +
    theme_minimal() +
    guides(fill = "none")
  return(p)
}

# Plotting function with two grouping variables and automated axis labeling.
plot_fish_comparison <- function(df, group_var, facet_var) {
  # 1. Capture labels for both variables
  x_lab <- rlang::as_label(rlang::enquo(group_var))
  f_lab <- rlang::as_label(rlang::enquo(facet_var))
  # 2. Build the plot
  p <- ggplot(df, aes(x = factor({{ group_var }}), 
                      y = Total_Length, 
                      fill = factor({{ group_var }}))) +
    geom_boxplot(alpha = 0.7) +
    # Use vars() to wrap the injected facet variable
    facet_wrap(vars({{ facet_var }})) + 
    labs(
      title = paste("Length Distribution:", x_lab, "by", f_lab),
      subtitle = "Automated summary generated from expanded_reporting_data.csv",
      x = x_lab,
      y = "Total Length (mm)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  return(p)
}

#Site-Specific Summary Function
get_site_summary <- function(df, site_num) {
  summary <- df %>%
    # Use site_num directly because it is a value (e.g., 2), not a column name
    filter(Waterbody_ID == site_num) %>%
    group_by(Species) %>%
    summarise(
      n_fish = n(),
      avg_len = mean(Total_Length, na.rm = TRUE)
    )
  return(summary)
}