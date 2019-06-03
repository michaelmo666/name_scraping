# Data wrangling
library(tidyverse)

# Parsing of HTML/XML fils
library(rvest)

# Verbose regular expressions
library(rebus)

# Define 'not in' operator
`%!in%` <- compose(`!`, `%in%`)  



# Define 'get_names' function
# Extract most popular names showd on the webpage
get_names <- function(html){
  html.read <- html %>% 
    read_html()
  
  name1 <- html.read %>% 
    html_nodes('a b') %>% 
    html_text()
  
  name2 <- html.read %>% 
    html_nodes('table tr td a') %>% 
    html_text()
  
  top_names <- c(name1,name2[-1:-28])
  
  top_names <- top_names[which(top_names %!in% c("go to the end of the list","Rank","Name","△"))] %>% 
    unique()
}


# Extract possible variation of name from its own page
get_variation <- function(html){
  variation <- html %>% 
    read_html() %>% 
    html_nodes(xpath='//*[@id="variations"]/p[1]/a') %>% 
    html_text()
  if (length(variation)==0){
    variation <-  ""
  } else{
    variation <-  variation
  }
  return(variation)
}


# Define the function build_data
# It will create a data frame contains all the first name,and its variation
# The input is the landing page's url and gender indication(1 or 0) > boy or girl
build_data <- function(html){
  top_names <- get_names(html) # call get_name, get all the popular names
  gender <- str_sub(html,39,39)
  urls <- str_c("http://www.thinkbabynames.com/meaning/",gender,"/",top_names) # generate subsequent url for each name
  
  variation <- matrix(ncol=2) # an empty matrix to store data
  try(
  for (i in 1:length(top_names)){
    nick_name <- get_variation(urls[i])
    len <- length(nick_name)
    name_matrix <- matrix(NA,len,ncol=2)
    name_matrix[1:len,1] <- nick_name[1:len]
    name_matrix[1:len,2] <- top_names[i]
    variation <- rbind(variation,name_matrix)
  })
  variation <- data.frame(variation)
  colnames(variation) <- c("Variation","First Name")
  return(variation[-1,]) #remove the first 'NA' row
}




# Execution
boy_first_url <- "http://www.thinkbabynames.com/popular/1/us"
boy_name_data <- build_data(boy_first_url)

girl_first_url <- "http://www.thinkbabynames.com/popular/0/us"
girl_name_data <- build_data(girl_first_url)





