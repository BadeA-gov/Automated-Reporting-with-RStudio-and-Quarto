library(quarto)

# Get a list of all unique site IDs
sites <- c(1, 2, 3, 4)

# Loop through and render a report for each
for (s in sites) {
  quarto_render(
    input = "Waterbody Survey Report B.qmd",
    execute_params = list(site_id = s),
    output_file = paste0("Report_Site_", s, ".html") #names the resulting files
  )
}
