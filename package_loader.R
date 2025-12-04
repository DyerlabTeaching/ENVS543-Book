#' Some functions to make sure all the libraries are installed on this computer.
#' 
#' If some are missing, it will automatically install them for you.


# Function to extract library calls from markdown files
extract_libraries_from_markdown <- function() {
  
  # Find all markdown files in the directory
  md_files <- list.files(
    path = ".",
    pattern = "narrative_*",
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(md_files) == 0) {
    return(character(0))
  }
  
  # Vector to store all package names
  all_packages <- character(0)
  
  # Regular expressions to match library() and require() calls
  # Matches: library(pkg), library("pkg"), library('pkg'), require(pkg), etc.
  lib_pattern <- "(?:library|require)\\s*\\(\\s*['\"]?([a-zA-Z0-9._]+)['\"]?\\s*\\)"
  
  # Read each markdown file and extract package names
  for (file in md_files) {
    tryCatch({
      # Read file content
      content <- readLines(file, warn = FALSE)
      
      # Look for R code chunks (between ```{r} and ```)
      in_r_chunk <- FALSE
      
      for (line in content) {
        # Check if we're entering or leaving an R code chunk
        if (grepl("^```+\\s*\\{r", line, ignore.case = TRUE)) {
          in_r_chunk <- TRUE
          next
        } else if (grepl("^```+\\s*$", line) && in_r_chunk) {
          in_r_chunk <- FALSE
          next
        }
        
        # If we're in an R chunk, look for library/require calls
        if (in_r_chunk) {
          matches <- gregexpr(lib_pattern, line, perl = TRUE)
          if (matches[[1]][1] != -1) {
            # Extract the package names from matches
            match_data <- regmatches(line, matches)[[1]]
            # Get just the package names (first capture group)
            for (match in match_data) {
              pkg_match <- regexec(lib_pattern, match, perl = TRUE)
              pkg_name <- regmatches(match, pkg_match)[[1]][2]
              all_packages <- c(all_packages, pkg_name)
            }
          }
        }
      }
    }, error = function(e) {
      warning(sprintf("Error reading file %s: %s", file, e$message))
    })
  }
  
  # Return unique package names
  unique_packages <- unique(all_packages)
  return(unique_packages)
}

# Function to check which packages are missing
check_missing_packages <- function(packages) {
  installed <- installed.packages()[, "Package"]
  missing <- setdiff(packages, installed)
  return(missing)
}

# Function to install missing packages
install_missing_packages <- function(packages, repos = "https://cloud.r-project.org") {
  if (length(packages) == 0) {
    return(invisible(NULL))
  }
  
  message(sprintf("\n%d package(s) need to be installed:", length(packages)))
  message(paste("  -", packages, collapse = "\n"))
  
  # Ask for confirmation
  response <- readline(prompt = "\nProceed with installation? (y/n): ")
  for (pkg in packages) {
    tryCatch({
      install.packages(pkg, repos = repos, dependencies = TRUE, quiet = FALSE)
    }, error = function(e) {
      warning(sprintf("    ✗ Failed to install %s: %s", pkg, e$message))
    })
  }
}
