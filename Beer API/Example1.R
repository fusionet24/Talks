#install.packages("plumber")
library(plumber)
library(tidyverse)# I'm lazy, only load what you need when in production
beer_data <- readr::read_csv("C:\\Users\\s-b\\Desktop\\Scotts\\NewCastleR\\Untapped.csv")# change this to use Here at somepoint
beer_data %>% dplyr::select(beer_type) %>% dplyr::distinct() %>% tibble::rowid_to_column(var ='styleid') -> BeerStyles # Set up Beer Styles

#* @apiTitle Beer Recomendations API
#* @apiDescription Get Recomendations on Beer Based upon Scotts Untapped Data!

#' Get Recomendations
#'
#' @param beertypeId:int Beer Style ID
#' @param numberOfRecomendations:int Number of Recomendations Required. Default = 5
#' @return Beer Recomendations based upon a style
#' @response 200 Successful Recomednations
#' @response 400 Bad Request, No Beer Style Provided
#' @response 404 No Beer Style Found with that ID
#* 
#' @get /RecommendABeerByType
function(beertypeId,numberOfRecomendations = 5,res, req)
{
  sink.reset()
  #log_file_name <- "C:\\Users\\s-b\\Desktop\\Scotts\\NewCastleR\\log.txt"
  #SetFileforSink("C:\\Users\\s-b\\Desktop\\Scotts\\NewCastleR\\log.txt")
  #sink(log_file)
  print(glue::glue('Request Recieved for beer type: {beertypeId}, number of recomednations = {numberOfRecomendations}, from {}'))
  numberOfRecomendations  = as.numeric(numberOfRecomendations)
  
  print(req)
  if(is.na(beertypeId))
  {
    res$status <- 400# Bad Request
    res$body <- "Please Provide Beer Style"
    return(res) ## We need more informaiton
  }

  BeerStyles[beertypeId,2]  -> filter_data
  
  if(is.na(filter_data))
  {
    print("NA Found")
    res$status <- 404
    res$body <- "Beer Style Doesn't Exist, Please check the BeerStyles Endpoint"
    return(res)
  }
  
  ## Probably should spilt the below into 2 that way you can check Recomendations < Number requested. Or enable replacement for safe returning always
  Recomendations <- beer_data %>% dplyr::filter(beer_type %in% filter_data) %>% 
                    select(beer_name,brewery_name, rating_score, global_rating_score,global_weighted_rating_score) %>% 
                    sample_n(size = numberOfRecomendations,replace = TRUE)
  
  
return(Recomendations)
  
}

#' Get Beer Styles
#' @get /BeerStyles
function()
{
  return(BeerStyles)
}


#' Say Hello
#' @get /Hello
function()
{
  return("Hello Newcastle R")
}



SetFileforSink <- function(filename = "NotSET.txt") {
  
  log <- file(filename, open = "wt")
  
  return(log)
  
}
RemoveFileForSink <- function(log, filename) {
  unlink(log)
  sink.reset()
  slackr_upload(filename = filename, title = "Meta table for Report", channels = "#performance")
  print("SINKS removed")
}

##Utility Function
sink.reset <- function() {
  for (i in seq_len(sink.number())) {
    sink(NULL)
  }
}