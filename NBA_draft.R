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

#Column Names

newnames <- draft_1997[1,]

#To replace column names 

for (i in 1:length(drafts_list)) {
  temp <- get(paste(drafts_list[i]))
  colnames(temp) <- newnames
  temp <- temp[-1,]
  assign(paste0("draft_", years[i]), temp)
}

#To delete extra rows with totals and column names repetitions in each draft data frame
#Also to replace blank spaces with NA (this converts all variables into numeric)

for (i in 1:length(drafts_list)) {
  temp <- get(paste(drafts_list[i]))
  temp <- temp %>%
    subset(Yrs != "Yrs") %>%
    subset(MP != "Totals")%>%
    na_if("")
  assign(paste0("draft_", years[i]), temp)
}
str(draft_1997)


rm(temp)

draft <-apply(draft_1997, as.numeric)

sapply(draft_1997, class)
character <- draft_1997[, 6:22]


col(draft_1997)
character

draft_1997[ , character] <- as.data.frame(apply(draft_1997[ , character], 2, as.numeric))



temp
view(temp)
