bookdown::render_book(
  input = c("index.Rmd", "internet_report.Rmd", "urban_index.Rmd", "early_childhood.Rmd", "Black_wealth.Rmd"),
  output_dir = "docs", 
  new_session = TRUE)

# https://stackoverflow.com/questions/45360998/code-folding-in-bookdown
# 
# in_header: ./tabs_header.html