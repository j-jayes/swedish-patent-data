#' ---
#' title: "Google Patents Scraper"
#' author: "JJayes"
#' date: "20/01/2022"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
## --------------------------------------------------------------------------------------------------------------
# knitr::purl("code/013-Google-patents_scraper_beta.Rmd", documentation = 2)

#' 
#' # Purpose
#' 
#' Scraping google patents for information about electricity related patents in Sweden.
#' 
#' # Process
#' 
#' Has three parts:
#' 
#' 1. List of pages to scrape
#' 
#' 2. Scraper script
#' 
#' 3. Iterator
#' 
#' ## List of pages to scrape
#' 
## --------------------------------------------------------------------------------------------------------------
# library(dplyr)
# library(readr)
# library(stringr)
# library(tidyr)
# library(purrr)

# or
library(tidyverse)
library(rvest)


#' 
## --------------------------------------------------------------------------------------------------------------
df <- read_rds("data/patent-info-google/Electricity_patents_list_Google_Patents.rds")

list_of_pages <- df %>% 
  select(id, patent_url = result_link)

#' 
#' ## Scraper Script
#' 
#' Things I want from the patent page:
#' 
#' * Title
#' * Description and claims text
#' * Countries where patent is registered
#' * Other citations
#' * Forward links to other patents
#' * Patents that this patent cites
#' * Also published as
#' * Similar documents
#' * Concepts that are mentioned in the patent
#' 
## --------------------------------------------------------------------------------------------------------------
pivot_long_in_nested_col <- function(tbl) {
  tbl %>% 
    pivot_longer(c(everything()), names_to = "key", values_to = "value") %>%
    mutate(value = str_squish(value)) %>% 
    nest(contents = everything())
}

#' 
#' 
## --------------------------------------------------------------------------------------------------------------
get_patent_info <- function(patent_url) {
  
  Sys.sleep(.5)
  
  # message so we know it's doing something
  message(paste0("Getting patent info from ", patent_url))
  
  # get html
  html <- read_html(patent_url)

# title
  title <- html %>%
    html_nodes("title") %>%
    html_text(trim = T) %>%
    str_squish() %>%
    str_remove(., " - Google Patents") %>% 
    ifelse(nchar(.) == 0, "missing", .)
  
# filingDate
  filing_date <- html %>% 
    html_nodes("[itemprop='filingDate']") %>% 
    html_attr("datetime") %>% 
    as_tibble() %>% 
    filter(!is.na(.)) %>% 
    rename(filing_date = value) %>% 
    ifelse(dim_desc(.) %>% parse_number() == 0, tibble(missing = "missing"), .)
  
# abstract
  abstract <- html %>% 
    html_nodes(".abstract") %>% 
    html_text(trim = T) %>%
    ifelse(identical(., character(0)), "missing", .)
  
# text
  description_text <- html %>%
    html_nodes(".description-paragraph") %>%
    # html_nodes(".notranslate") %>%
    # html_nodes("span") %>%
    html_text(trim = T) %>%
    as_tibble() %>%
    nest(desc_text = c(value)) %>% 
    ifelse(dim_desc(.) %>% parse_number() == 0, tibble(missing = "missing"), .)

  claims_text <- html %>%
    html_nodes(".claim-text") %>%
    html_text(trim = T) %>%
    as_tibble() %>%
    nest(claim_text = c(value)) %>% 
    ifelse(dim_desc(.) %>% parse_number() == 0, tibble(missing = "missing"), .)
  
# countries
  countries <- html %>%
    html_nodes("[itemprop='countryStatus']") %>%
    html_nodes("[itemprop='countryCode']") %>%
    html_text() %>% paste0(collapse = ", ") %>% 
    as_tibble() %>% 
    rename(countries = value) %>% 
    ifelse(dim_desc(.) %>% parse_number() == 0, tibble(missing = "missing"), .)
 
# concepts
  ids <- html %>%
    html_nodes("[itemprop='concept']") %>%
    html_nodes("[itemprop='id']") %>%
    html_text()

  names <- html %>%
    html_nodes("[itemprop='concept']") %>%
    html_nodes("[itemprop='name']") %>%
    html_text()

  counts <- html %>%
    html_nodes("[itemprop='concept']") %>%
    html_nodes("[itemprop='count']") %>%
    html_text()

  concepts <- tibble(ids, names, counts) %>%
    nest(concepts = c(ids, names, counts)) %>% 
    ifelse(dim_desc(.) %>% parse_number() == 0, tibble(missing = "missing"), .)
  
# tables
  tables <- html %>%
    html_table() %>%
    map_dfr(., ~ nest(.)) %>%
    mutate(
      table = row_number(),
      data = map(data, janitor::clean_names)
    ) %>%
    mutate(data = map(data, pivot_long_in_nested_col)) %>%
    unnest(data) %>%
    nest(tables_tbl = everything()) %>% 
    ifelse(dim_desc(.) %>% parse_number() == 0, tibble(missing = "missing"), .)
  
  output <- tibble(
    title,
    filing_date,
    abstract,
    description_text,
    concepts,
    claims_text,
    countries,
    tables
  )
  output
}


#' 
#' ## Iterator
#' 
## --------------------------------------------------------------------------------------------------------------
# testing

list_of_pages <- tibble(
  id = "1",
  patent_url = "https://stackoverflow.com/questions/52505923/error-in-bind-rows-x-id-argument-1-must-have-names"
) %>% bind_rows(list_of_pages)
list_of_pages

test <- list_of_pages %>% 
  slice_head(n = 3) %>% 
  mutate(patent_info = map_df(patent_url, possibly(get_patent_info, tibble(failed = "failed"))))

st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")

test %>% 
  write_rds(paste0("data/patent-info-google/patent_df_test_", st, ".rds"), compress = "gz")

#' 
