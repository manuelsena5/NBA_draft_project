library(tidyverse)
library(rvest)
library(dplyr)



link <- "https://www.basketball-reference.com/draft/NBA_"


years <- c(1997:2017)  #years to look at

links_df <- data.frame(html=character())  # list of links

all_players <- data.frame(Players = character(), link = character()) #List of players of all selected drafts and links

drafts_list <- list()


#To get the draft tables form basketball reference

for (i in 1:length(years)) {
  theurl <- paste0(link, years[i],".html")
  draft <- read_html(theurl)
  assign(paste0("draft_", years[i]), draft %>% html_nodes("table") %>% html_table() %>% .[[1]])
    drafts_list <- rbind(drafts_list, paste0("draft_", years[i]))
}


#Column Names and to make unique variable names


newnames <- draft_1997[1,]
newnames[15:18] <- list("MP.per", "PTS.per", "TRB.per", "AST.per")  


#To replace column names 


#Also to replace blank spaces with NA


for (i in 1:length(drafts_list)) {
  temp <- get(paste(drafts_list[1]))
  colnames(temp) <- newnames

  
# Since the draft table does not have defensive stats per game. Code gets player page in website to scrape them.      
 #the issue I am having right now is getting the iteration for the temp link to the current year in the loop. 
  
   for (j in 1:62) {
     
    temp_link <- draft %>% html_nodes(paste0("tr:nth-child(", j , ") a")) 
    if (length(temp_link) == 0) next
    temp_link <- temp_link %>% html_attr("href") %>% .[[3]]
    links_df <- rbind(links_df, paste0("https://www.basketball-reference.com",temp_link))
    
  }
  
#To delete extra rows with totals and column names repetitions in each draft data frame  
  
  temp <- temp[-1,]
  temp <- temp %>%
    subset(Yrs != "Yrs") %>%
    subset(MP != "Totals")%>%
    na_if("")
  newnames1 <- c(colnames(temp))
  temp[,-3:-5] <- lapply(temp[,newnames1[-3:-5]], as.numeric)
  temp[6:22] <- temp[6:22] %>% replace(is.na(.), 0)
  assign(paste0("draft_", years[1]), temp)
  
  for (i in 1:length(temp$Player)) {
    all_players <- rbind(all_players, temp$Player[i])
    colnames(all_players) <- "Player"
  }
  colnames(links_df) <- "Html_links"
  all_players <- cbind(all_players, links_df)
}

r



# for (i in 1:length(drafts_list)) {
#   temp <- get(paste(drafts_list[i]))
#   colnames(temp) <- newnames
#   temp <- temp[-1,]
#   temp <- temp %>%
#     subset(Yrs != "Yrs") %>%
#     subset(MP != "Totals")%>%
#     na_if("")
#   newnames1 <- c(colnames(temp))
#   temp[,-3:-5] <- lapply(temp[,newnames1[-3:-5]], as.numeric)
#   temp[6:22] <- temp[6:22] %>% replace(is.na(.), 0)
#   assign(paste0("draft_", years[i]), temp)
# }











