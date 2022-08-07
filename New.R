pack <- c("tidyverse", "rvest", "dplyr")
install.packages(pack)
library(tidyverse)
library(rvest)
library(dplyr)



link <- "https://www.basketball-reference.com/draft/NBA_"


years <- c(1997:2017)  #years to look at

links_df <- data.frame(html=character())  # list of links

all_players <- data.frame(Players = character(), link = character()) #List of players of all selected drafts and links

players_yrs <- data.frame()

def_stats <- data.frame(Player = character(), STL = double(), BLK = double())

df_list <- list()


for (i in 1:length(years)) {
  theurl <- paste0(link, years[i],".html")
  draft <- read_html(theurl)
  df_list[[i]] <- draft %>% html_nodes("table") %>% html_table() %>% .[[1]]
  
}

newnames <- df_list[[1]][1,]
newnames[15:18] <- list("MP.per", "PTS.per", "TRB.per", "AST.per")

Col_name_change <- function(df_list){
  colnames(df_list) <- newnames
  
}

lapply(df_list, colnames())

colnames(temp) <- newnames
view(df_list[[1]])

for (i in 1:length(df_list)) {
  assign(paste0("draft_", years[1]),left_join(df_list[[1]],all_players, "Player"))
  
  
}
