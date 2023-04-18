library(tibble)
install.packages("rvest")
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() 
  # Get the nodes with the id
> rank <-  html %>% 
  +     html_nodes(css = ".titleColumn") %>% #search for title class
  +     html_text()
> rank <-  html %>% 
  +     html_nodes(css = "#constituents") %>% #search for id constituents not found in movie table 
  +     html_text()


#json file operations
bike_data_lst <- fromJSON("bike_data.json")
# Open the data by clicking on it in the environment or by running View()
View(bike_data_lst)