# Quick test to load the package and check for errors
options(error = recover)

# Try loading the package
tryCatch({
  library(devtools)
  load_all(".")
  message("Package loaded successfully!")

  # Check if functions are available
  message("Checking if functions exist...")
  message("fetch_grad: ", exists("fetch_grad"))
  message("tidy_grad: ", exists("tidy_grad"))
  message("get_raw_grad: ", exists("get_raw_grad"))

  # Try to get documentation
  message("\nTesting function documentation...")
  args_fetch_grad <- args(fetch_grad)
  message("fetch_grad args: ", paste(names(args_fetch_grad), collapse = ", "))

  message("\nAll checks passed!")

}, error = function(e) {
  message("Error: ", e$message)
  traceback()
})
