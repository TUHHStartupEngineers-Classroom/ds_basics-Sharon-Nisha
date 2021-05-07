library(httr)
resp <- GET("https://swapi.dev/api/people/1/")
#https://www.skyscanner.de/transport/fluge/ham/muni/210512/210519/?adults=1&adultsv2=1&cabinclass=economy&children=0&childrenv2=&destinationentityid=27545034&inboundaltsenabled=false&infants=0&originentityid=27536295&outboundaltsenabled=false&preferdirects=false&preferflexible=false&ref=home&rtn=1

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp


#take api save t in a var convert contents to json format
library(jsonlite)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()
##status obtained and now in json format


#token
token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}")) %>% 
  .$content%>% 
  rawToChar() %>% 
fromJSON()


################own try
library(httr)
resp_fl <- GET("http://18.232.62.17:3000/")
library(jsonlite)
resp_fl %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()


content(resp_fl, as = "text")
content(resp_fl, as = "parsed")
content(resp_fl)

library(jsonlite)

thepage =GET("http://18.232.62.17:3000/")
content(thepage, as = "text")
content(thepage, as = "parsed")
content(thepage)



#####reading from orange
library(httr)

#orange=GET("https://thinkorange.com/#")
#orange##html
url="https://thinkorange.com/#"
html <- url %>% 
  read_html()
read<-  html %>% 
  html_nodes(css = ".sub-menu elementor-nav-menu--dropdown .menu-item menu-item-type-custom menu-item-object-custom menu-item-3297>a") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()





library(xml2)
library(httr)
url  <- "https://www.radon-bikes.de/e-bike/mountainbike/bikegrid/"
html <- url %>% 
  read_html()
rank <-  html %>% 
  html_nodes(css = ".small-12 .a-heading--small")%>% 
  html_text()
  rank_non<-stringr::str_replace(rank, "\n", "")  %>%   #remove /n
  discard(.p = ~stringr::str_detect(.x,"Welcome to our Radon-Bikes Instagram!|#radonbikes #bisbaldimwald"))
  rank_non2<-stringr::str_extract(rank_non,"(?<= ).*(?=\n)")

  ##price of each bike type 
  bikeprice<-  html %>% 
    html_nodes(css = ".small-12 .currency_eur .m-bikegrid__price--active") %>% 
    html_text()
  
  rank_non3<-stringr::str_extract(bikeprice,"[0-9]+")%>%
  as.numeric()
  
  #vector to table 
  

#fill the names to a table m-bikegrid__price--active
  imdtbl <- tibble(rank_non2,rank_non3)%>%
    mutate(rank_non3)

#
