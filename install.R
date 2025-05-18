options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "tidyverse", "shiny", "bs4Dash", "plotly", 
  "wordcloud2", "DT", "tidytext", "textdata",
  "syuzhet", "rvest"
)

# Install only missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

purrr::walk(required_packages, install_if_missing)

