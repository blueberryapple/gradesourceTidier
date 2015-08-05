#helper function to find the columns with percents and elimgetinate
#multiple variables per column
findIndices <- function(column)
{
  column[1] == "Score"
}

getGrades <- function(url)
{
  #libraries
  library(XML)
  library(dplyr)
  
  #html to table data frame
  standingsTable <- readHTMLTable(url, which = 4, 
                                  as.data.frame = TRUE)
  
  #print(standingsTable)
  
  #get the table names
  htmlNodes <- htmlTreeParse(url, useInternalNodes = TRUE)
  tableNames <- xpathSApply(htmlNodes, 
                            "//table[@border='1']/
                            tr[@bgcolor='#CCCCFF']/td/font/*", 
                            xmlValue)
  
  #get the indices to keep
  indicesBool <- sapply(standingsTable, findIndices)
  indices <- which(indicesBool %in% TRUE)
  
  #tidied grades
  grades <- 
  {
    select(standingsTable, 1, indices) %>%
      sapply(gsub, pattern = "%", replacement = "") %>%
      data.frame() %>%
      tbl_df() %>%
      slice(-c(1,2))
  }
  
  #sets table names
  names(grades) <- tableNames
  grades
}

urlBank <- function(url)
{
  library(XML)
  
  htmlNodes <- htmlTreeParse(url, useInternalNodes = TRUE)
  links <- xpathSApply(htmlNodes, 
                       "(//table[@cellpadding='4'][@width='100%'])
                       [3]/tr/td/font/a[@href]/@href")
  
  lastIndex <- length(links)
  categoriesUrl <- paste(url, links[1], sep = "")
  assessmentUrl <- paste(url, links[lastIndex], sep = "")
  
  c(categoriesUrl, assessmentUrl)
}