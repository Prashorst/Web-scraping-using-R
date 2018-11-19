
#Note about what is to follow:
#Trustpilot is a website for writing reviews about different websites. In this scraping exercise, we are going to scrape user 
#ratings/reviews for Spotify. For this purpose, we will use Rvest library in R.


#Loading the necessary libraries

# General purpose data wrangling
library(tidyverse)

#Parsing HTML/XML files
library(rvest)

#String manipulation
library(stringr)

#Verbose regular expressions
library(rebus)

#Eases Datetime manipulation
library(lubridate)

#To export data to an excel file
library(xlsx)

#Creating a variable to store the raw html of the source url
url = "https://www.trustpilot.com/review/www.spotify.com"
download.file(url, destfile = "scrapedpage1.html", quiet=TRUE)
content1 <- read_html("scrapedpage1.html")
content1

#getting the total number of pages which are to be scraped
get_last_page <- function(html){
  pages_data <- html %>% 
    html_nodes('.pagination-page') %>%
    html_text()
  
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric() 
  
}


#Finding the number of pages to be scraped  
first_page <- read_html(url)
(latest_page_number <- get_last_page(first_page))

#This gives us 15 pages to scrape in total

#Generating a list of all urls to be scraped from those 15 pages
list_of_pages <- str_c(url, '?page=', 1:latest_page_number)

#Extracting information of one page
get_reviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.review-info__body__text') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

#Get reviewer names
get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.consumer-info__details__name') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}

#Getting the review dates
get_review_dates <- function(html){
  
  status <- html %>% 
    html_nodes('time') %>% 
    # The status information is this time a tag attribute
    html_attrs() %>%             
    # Extract the second element
    map(2) %>%                    
    unlist() 
  
  dates <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>% 
    map(1) %>% 
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%                 
    unlist()
  
  # Combine the status and the date information to filter one via the other
  return_dates <- tibble(status = status, dates = dates) %>%   
    # Only these are actual reviews
    filter(status == 'ndate') %>%              
    # Select and convert to vector
    pull(dates) %>%                            
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00') 
  
}

#Extracting the star ratings
get_star_rating <- function(html){
  
  
  # The pattern we look for
  pattern = 'star-rating-'%R% capture(DIGIT)  
  
  vector <- html %>% 
    html_nodes('.star-rating') %>% 
    html_attr('class') %>% 
    str_match(pattern) %>% 
    map(1) %>% 
    as.numeric()  
  vector <-  vector[!is.na(vector)]
  vector <- vector[3:length(vector)]
  vector
}


#Creating a function to extract reviewer, date, rating and review from raw html

get_data_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  dates <- get_review_dates(html)
  ratings <- get_star_rating(html)
  
  # Minimum length
  min_length <- min(length(reviews), length(reviewer_names), length(dates), length(ratings))
  
  # Combine into a tibble
  combined_data <- tibble(reviewer = reviewer_names[1:min_length],
                          date = dates[1:min_length],
                          rating = ratings[1:min_length],
                          review = reviews[1:min_length]) 
  
  # Tag the individual data with the company name
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company, reviewer, date, rating, review)
  
  return( combined_data )
}

#Shortening the above function as follows
get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html, company_name)
}


#Creating a function to scrape html from all the 15 pages 
scrape_write_table <- function(html, company_name){
  
  # Read first page
  first_page <- html
  
  # Extract the number of pages that have to be queried
  latest_page_number <- get_last_page(first_page)
  
  # Generate the target URLs
  list_of_pages <- str_c(html, '?page=', 1:latest_page_number)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    map(get_data_from_url, company_name) %>%  # Apply to all URLs
    bind_rows() %>%                           # Combines the tibbles into one tibble
    write_tsv(str_c(company_name,'.tsv'))     # Writes a tab separated file
}

#Applying the function to the spotify ratings pages
scrape_write_table(content1, 'spotify')
spotify_table <- read_tsv('spotify.tsv')

#Displaying a part of the tibble
tail(spotify_table, 5)

#Exporting the results to an excel file
write.xlsx(spotify_table, file="Spotify viewer ratings.xlsx")










