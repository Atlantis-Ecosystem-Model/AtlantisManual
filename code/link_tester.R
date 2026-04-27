library(tidyverse)
library(fs)
library(stringr)
library(httr2)

# --- Configuration ---
search_path <- "user_guides/quarto_site/" # Change this to your directory

# 1. Find all .qmd files recursively
files <- dir_ls(search_path, recurse = TRUE, glob = "*.qmd")

# 2. Extract URLs, filenames, and line numbers
extract_urls <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Refined Regex: captures the URL but forbids ending on trailing punctuation
  # specifically common in Markdown/Quarto like ), or ].
  url_pattern <- "https?://[\\w\\d:#@%/;$()~_?\\+-=\\\\\\.&]+(?<![\\.,\\)\\!\\?])"
  
  map_df(seq_along(lines), function(i) {
    matches <- str_extract_all(lines[i], url_pattern)[[1]]
    
    if (length(matches) > 0) {
      # Clean up any weird edge cases
      matches <- str_trim(matches)
      
      tibble(
        file = as.character(file_path),
        line = i,
        url = matches
      )
    } else {
      NULL
    }
  })
}

url_data <- map_df(files, extract_urls)

# 3. Test URLs (Asynchronously)
# We use httr2 for "multi-request" to speed things up significantly
test_urls_advanced <- function(urls) {
  # 1. We use GET instead of HEAD because some paywalls only 
  # trigger on a full page request.
  reqs <- map(urls, ~request(.x) %>% 
                req_options(followlocation = TRUE) %>% 
                req_timeout(10))
  
  # 2. Perform parallel requests
  resps <- req_perform_parallel(reqs, on_error = "continue")
  
  map_df(resps, function(res) {
    # Scenario A: The request was successful (even if it's a 404 or redirect)
    if (inherits(res, "httr2_response")) {
      tibble(
        url = res$request$url,      # The URL we started with
        status = as.character(resp_status(res)),
        final_url = res$url,        # Where we ended up
        likely_locked = str_detect(res$url, "login|signin|paywall"),
        is_active = (resp_status(res) == 200 && !likely_locked)
      )
    } 
    # Scenario B: The request failed entirely (DNS, Timeout, etc.)
    else {
      # 'res' here is an error object. It usually contains the original request.
      # We extract the URL from the failed request so it's not NA.
      failed_url <- if (!is.null(res$request$url)) res$request$url else "Unknown"
      
      tibble(
        url = failed_url,
        status = "Failed",
        final_url = "CONNECTION_ERROR", 
        likely_locked = FALSE,
        is_active = FALSE
      )
    }
  })
}

# Apply testing
if (nrow(url_data) > 0) {
  cat("Testing", nrow(url_data), "unique URLs...\n")
  
  # 1. Get unique URLs to save time
  unique_urls_to_test <- unique(url_data$url)
  
  # 2. Run the advanced test (returns a data frame)
  test_results <- test_urls_advanced(unique_urls_to_test)
  
  # 3. Join the results back to the original list of file/line locations
  final_results <- url_data %>%
    left_join(test_results, by = "url")
  
  # 4. Show a summary of the suspicious links
  flagged_links <- final_results %>% 
    filter(!is_active)
  
  if (nrow(flagged_links) > 0) {
    cat("\nFound", nrow(flagged_links), "problematic or gated links:\n")
    print(flagged_links)
  } else {
    cat("\nAll links appear active and open!")
  }
  
} else {
  message("No URLs found.")
}

write.csv(final_results,here::here('code','link_testing.csv'),row.names =F)
