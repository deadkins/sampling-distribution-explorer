# Export the Shiny app as a static shinylive site.
# Install packages manually if needed:
# install.packages(c("shiny", "shinylive"))

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Package 'shinylive' is required. Install it with install.packages('shinylive').")
}

shinylive::export("app", "site")
