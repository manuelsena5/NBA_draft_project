pack <- c("tidyverse", "rvest", "dplyr")
install.packages(pack)
library(tidyverse)
library(rvest)
library(dplyr)



link <- "https://www.basketball-reference.com/draft/NBA_"

drafts_list <- list()

years <- c(1997:2017)  #years to look at

links_df <- data.frame()  # list of links

all_players <- data.frame(Players = character(), link = character()) #List of players of all selected drafts and links

players_yrs <- data.frame()

def_stats <- data.frame(Player = character(), STL.per = double(), BLK.per = double())

# df_list <- list()
# 
# 
# for (i in 1:length(years)) {
#   theurl <- paste0(link, years[i],".html")
#   draft <- read_html(theurl)
#   df_list[[i]] <- assign(paste0("draft_", years[i]), draft %>% html_nodes("table") %>% html_table() %>% .[[1]])
#  
# }


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
  temp <- get(paste(drafts_list[i]))
  colnames(temp) <- newnames
  
  #To delete extra rows with totals and column names repetitions in each draft data frame  
  
  temp <- temp[-1,]
  temp <- temp %>%
    subset(Yrs != "Yrs") %>%
    subset(MP != "Totals")%>%
    na_if("")
  newnames1 <- c(colnames(temp))
  temp[,-3:-5] <- lapply(temp[,newnames1[-3:-5]], as.numeric)
  temp[6:22] <- temp[6:22] %>% replace(is.na(.), 0)
  assign(paste0("draft_", years[i]), temp)
  
  for (i in 1:length(temp$Player)) {
    all_players <- rbind(all_players, temp$Player[i])
    all_players <- na.omit(all_players)
    colnames(all_players) <- "Player"
  }
  
  for (i in 1:length(temp$Player)) {
    players_yrs <- rbind(players_yrs, temp$Yrs[i])
    players_yrs <- na.omit(players_yrs)
    colnames(players_yrs) <- "Yrs"
  }
  
}

# Since the draft table does not have defensive stats per game. Code gets player page in website to scrape them.      
#the issue I am having right now is getting the iteration for the temp link to the current year in the loop. 


for (k in 1:length(years)){
  theurl2 <- paste0(link, years[k], ".html")
  get_links <- read_html(theurl2)
  
  for (j in 1:62) {  
    temp_link <- get_links %>% html_nodes(paste0("tr:nth-child(", j , ") a")) 
    if (length(temp_link) == 0) next
    temp_link <- temp_link %>% html_attr("href") %>% .[[3]]
    links_df <- rbind(links_df, paste0("https://www.basketball-reference.com",temp_link))
  }
  
}

colnames(links_df) <- "Html_links"
all_players <- cbind(all_players, links_df)


#for (i in 1:length(drafts_list)) {
  #assign(paste0("draft_", years[i]),left_join(get(paste0(drafts_list[i])),all_players, "Player"))
  
  
#}
 
for (i in 1:length(all_players$Html_links)) {
  url2 <- read_html(all_players$Html_links[i]) 
  player_table <- url2 %>%   html_nodes("table#per_game")
  
  if (length(player_table) == 0) {
  player_table <- cbind("STL" = 0, "BLK" = 0, "Player" = all_players$Player[i])
  def_stats <- rbind(def_stats, player_table)
  }
  
  else {
  
  player_table <- player_table %>% html_table() %>% .[[1]]
  player_table <- player_table %>% filter(Season == "Career")
  player_table <- cbind(player_table[26:27], "Player" = all_players$Player[i])
  def_stats <- rbind(def_stats, player_table)
  }
  
}  
 
for (i in 1:length(years)) {
  
  assign(paste0("draft_", years[i]),left_join(get(paste0("draft_", years[i])), def_stats, "Player"))
 
}


#rm(deft_stats)


redraft_func <- function(redraft){
#   
#   draft = readline(prompt = "Enter draft year:")
#   draft = assign(paste0("draft_",draft),, temp)

  temp <- redraft
  
  
  #to calculate the z-score for all player stats
  
  temp[23] <- as.numeric(unlist(temp[23]))
  temp[24] <- as.numeric(unlist(temp[24]))
  temp[23:24] <- temp[23:24] %>% replace(is.na(.), 0)
  
  
  temp$G.z_score <- ifelse(round((temp$G-mean(temp$G))/sd(temp$G),2) < 0.00, 0.00, 
                           round((temp$G-mean(temp$G))/sd(temp$G),2))
  temp$MP.per.z_score <- ifelse(round((temp$MP.per-mean(temp$MP.per))/sd(temp$MP.per),2) < 0.00, 0.00, 
                           round((temp$MP.per-mean(temp$MP.per))/sd(temp$MP.per),2))
  temp$PTS.per.z_score <- ifelse(round((temp$PTS.per-mean(temp$PTS.per))/sd(temp$PTS.per),2) < 0.00, 0.00, 
                           round((temp$PTS.per-mean(temp$PTS.per))/sd(temp$PTS.per),2))
  temp$TRB.per.z_score <- ifelse(round((temp$TRB.per-mean(temp$TRB.per))/sd(temp$TRB.per),2) < 0.00, 0.00, 
                           round((temp$TRB.per-mean(temp$TRB.per))/sd(temp$TRB.per),2))
  temp$AST.per.z_score <- ifelse(round((temp$AST.per-mean(temp$AST.per))/sd(temp$AST.per),2) < 0.00, 0.00, 
                           round((temp$AST.per-mean(temp$AST.per))/sd(temp$AST.per),2))
  temp$STL.z_score <- ifelse(round((temp$STL-mean(temp$STL))/sd(temp$STL),2) < 0.00, 0.00, 
                           round((temp$STL-mean(temp$STL))/sd(temp$STL),2))
  temp$BLK.z_score <- ifelse(round((temp$BLK-mean(temp$BLK))/sd(temp$BLK),2) < 0.00, 0.00, 
                           round((temp$BLK-mean(temp$BLK))/sd(temp$BLK),2))
  
  temp$TOTAL.z_score <- temp$G.z_score + temp$MP.per.z_score + temp$PTS.per.z_score + temp$TRB.per.z_score +
                            temp$AST.per.z_score + temp$STL.z_score + temp$BLK.z_score 
  
  
  temp <- temp[order(temp$TOTAL.z_score, decreasing = TRUE),]
  
 view(temp)
  
}
  

redraft_func(draft_2003)

# ####What you are requesting is the use of assign and get, and you will find a significant amount of
# opposition to using those functions: they almost always indicate poor design. Since the frames all 
# look the same I suggest you look into using a list of frames: since you're likely to do the 
# same or similar things to each frame, the use of lapply can greatly simplify 
# the maintainability/flexibility of your code

#https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames/24376207#24376207
