library(tidyverse)
library(rvest)
library(dplyr)



link <- "https://www.basketball-reference.com/draft/NBA_"


years <- c(1997:2017)  #years to look at

links_df <- data.frame(html=character())  # list of links

drafts_list <- list()


#To get the draft tables form basketball reference

for (i in 1:length(years)) {
  theurl <- paste0("https://www.basketball-reference.com/draft/NBA_",years[i],".html")
  draft <- read_html(theurl)
  assign(paste0("draft_", years[i]), draft %>% html_nodes("table") %>% html_table() %>% .[[1]])
  links_df <- rbind(links_df, theurl)
  drafts_list <- rbind(drafts_list, paste0("draft_", years[i]))
}

#Column Names and to make unique variable names


newnames <- draft_1997[1,]
newnames[15:18] <- list("MP.per", "PTS.per", "TRB.per", "AST.per")  


#To replace column names 

#To delete extra rows with totals and column names repetitions in each draft data frame
#Also to replace blank spaces with NA


for (i in 1:length(drafts_list)) {
  temp <- get(paste(drafts_list[i]))
  colnames(temp) <- newnames
  temp <- temp[-1,]
  temp <- temp %>%
    subset(Yrs != "Yrs") %>%
    subset(MP != "Totals")%>%
    na_if("")
  newnames1 <- c(colnames(temp))
  temp[,-3:-5] <- lapply(temp[,newnames1[-3:-5]], as.numeric)
  temp[6:22] <- temp[6:22] %>% replace(is.na(.), 0)
  assign(paste0("draft_", years[i]), temp)
}











