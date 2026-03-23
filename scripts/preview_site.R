# Preview the exported static site locally.
# Install packages manually if needed:
# install.packages("httpuv")

if (!requireNamespace("httpuv", quietly = TRUE)) {
  stop("Package 'httpuv' is required. Install it with install.packages('httpuv').")
}

httpuv::runStaticServer("site")
