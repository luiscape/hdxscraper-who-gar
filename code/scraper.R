# Scraper for WHO's Global Alert and Response website

# dependencies
library(XML)
library(RCurl)


###################################################
###################################################
######### Scraping a List of Reports List #########
###################################################
###################################################

# function that gets the list of documents from WHO
# website and assembles a nice data.frame
scrapeList <- function(year = 2014) {
  cat('-------------------------------\n')
  cat('Assembling a list of documents.\n')
  cat('-------------------------------\n')

  for (i in 1:length(year)) {
    
    # print progress
    cat('## Year:', year[i], '\n')
    
    # list of urls
    list_url = paste('http://www.who.int/csr/don/archive/year/', year[i], '/en/', sep="")
    
    # getting the html
    doc <- htmlParse(list_url)
    
    # ISSUE
    # apparently the name field is returning an extra name somewhere
    # name = xpathSApply(doc, '//*[@id="content"]/div/div[1]/ul/li/span', xmlValue),
    # extracting the attributes of interest from their pages.
    # this is important as we need the doc names to encode the desease properly
    it <- data.frame(
      date =  xpathSApply(doc, '//*[@id="content"]/div/div[1]/ul/li/a', xmlValue),
      url = xpathSApply(doc, '//*[@id="content"]/div/div[1]/ul/li/a', xmlGetAttr, "href"),
      year = year[i])
    
    # fixing URLs
    it$url <- sub('/entity/', 'http://www.who.int/', it$url)
    it$url <- sub('index.html', '', it$url)
    
    # adding data to df
    if (i == 1) output <- it
    else output <- rbind(output, it)
  }
  
  # adding desease specific variable
  # creating vector with nas
  output$desease <- NA
  output$desease <- ifelse(grepl("ebola", output$url), 'Ebola', output$desease)
  output$desease <- ifelse(grepl("polio", output$url), 'Polio', output$desease)
  output$desease <- ifelse(grepl("enterovirus", output$url), 'Enterovirus', output$desease)
  output$desease <- ifelse(grepl("avian_influenza", output$url), 'Avian Influenza', output$desease)
  output$desease <- ifelse(grepl("mers", output$url), 'MERS', output$desease)
  output$desease <- ifelse(grepl("h7n9", output$url), 'H7N9', output$desease)
  output$desease <- ifelse(grepl("cholera", output$url), 'Cholera', output$desease)
  output$desease <- ifelse(grepl("coronavirus", output$url), 'Coronavirus', output$desease)
  output$desease <- ifelse(grepl("ncov", output$url), 'NCOV', output$desease)
  
  # results
  cat('-------------------------------\n')
  cat('Done!\n')
  cat('-------------------------------\n')
  return(output)
}
updateList <- scrapeList(year = c(2014:1996))

# writing output
write.csv(updateList, 'data/report_list.csv', row.names = F)