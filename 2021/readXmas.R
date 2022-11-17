
# Function to read data from AOC
# Copied from https://colin-fraser.net/post/a-quick-tutorial-on-importing-data-from-advent-of-code-into-r/

library(glue)
library(httr)
library(readr)
library(memoise)
library(dplyr)


# Set URL ----------------------------------------------------------
aoc_build_url <- function(day, year = 2022) {
  formatted_url <- glue("adventofcode.com/{year}/day/{day}/input")
  return(formatted_url)
}

# Use keyring::keyring_create("AOC") and enter a password for the keyring
# Use keyring::key_set("Rstudio Keyring secrets", keyring = "AOC") to set session cookie from AOC webpage
# --> This prompts you to enter the session cookie.
# --> Session cookie can be retrieved from opening up a dataset from one of the AOC puzzles
# ----> Right-click + Inspect
# ----> Press Network. Do Ctrl+R.
# ----> Press the "input" file that appears.
# ----> Scroll down to cookie and copy the "session=" value


# Retrieve data
.aoc_get_response <- function(day, 
                             session_cookie = keyring::key_get("Rstudio Keyring secrets", keyring = "AOC"), 
                             year = 2021) {
  aoc_url <- aoc_build_url(day, year)
  cookie <- set_cookies(session = session_cookie)
  response <- GET(aoc_url, cookie)
  return(response)
}

# Cache response so we don't query the server every time we run the function
aoc_get_response <- memoise::memoise(.aoc_get_response, cache = memoise::cache_filesystem("~/.aoc"))


# Wrapper with encoding and formating
get_pzl_data <- function(day, year = 2021) {
  
  aoc_get_response(day, year = year) %>% 
    content(encoding = 'UTF-8') %>% 
    read_lines()
  
}


