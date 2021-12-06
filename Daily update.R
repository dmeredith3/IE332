library(DBI)
library(odbc)
library(RMySQL) #if already installed
library(RMariaDB)
library(plyr)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

get_skater_stats <- function(year){
  #Pull from html
  skaters_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_skaters.html", sep = '')) #Reads html of hockey-reference
  skaters_stats <- skaters_url %>% html_node("table") %>% html_table(fill=TRUE) #Pulls table of players stats
  #Moves around headers
  names(skaters_stats) <- as.character(skaters_stats[1,])
  skaters_stats <- skaters_stats[-1,]
  row_nums <- skaters_stats[1]
  
  #Removes duplicate players and header rows
  rows <- c()
  count <- 1
  num <- 1
  for (row in row_nums) {
    for(r in row) {
      if(r == "Rk" || r != num){
        rows <- append(rows, count)
      }
      else{
        num <- num + 1
      }
      count <- count + 1
    }
  }
  skaters_stats <- skaters_stats[-rows,]
  
  #Formats power play and shorthanded goals and assists
  skaters_stats$PPG <- skaters_stats$PP
  skaters_stats$PP <- NULL
  skaters_stats$PPA <- skaters_stats$PP
  skaters_stats$SHG <- skaters_stats$SH
  skaters_stats$SH <- NULL
  skaters_stats$SHA <- skaters_stats$SH
  
  #Takes rows necessary for fantasy
  skaters_stats <- skaters_stats[, c('Player','Age','Pos','GP','G','A','PPG','PPA','SHG', 'SHA','PIM','S','HIT','BLK')]
  #Formats data into numeric form
  if (year == 2021){
    fact <- 82/56
  }else if (year == 2020){
    fact <- 82/70
  }else{
    fact <- 1
  }
  skaters_stats$GP <- as.numeric(skaters_stats$GP) * fact
  skaters_stats$G <- as.numeric(skaters_stats$G) * fact
  skaters_stats$A <- as.numeric(skaters_stats$A) * fact
  skaters_stats$PPG <- as.numeric(skaters_stats$PPG) * fact
  skaters_stats$PPA <- as.numeric(skaters_stats$PPA) * fact
  skaters_stats$SHG <- as.numeric(skaters_stats$SHG) * fact
  skaters_stats$SHA <- as.numeric(skaters_stats$SHA) * fact
  skaters_stats$PIM <- as.numeric(skaters_stats$PIM) * fact
  skaters_stats$S <- as.numeric(skaters_stats$S) * fact
  skaters_stats$HIT <- as.numeric(skaters_stats$HIT) * fact
  skaters_stats$BLK <- as.numeric(skaters_stats$BLK) * fact
  skaters_stats <- separate(skaters_stats, Player, c('First', 'Last'), sep = ' ', extra='merge')
  skaters_stats$Id <- tolower(paste(substr(skaters_stats$First,1,1), str_replace_all(substr(skaters_stats$Last,1,10), pattern=" ", repl=""), (year - as.numeric(skaters_stats$Age)), sep = ''))
  skaters_stats <- skaters_stats[, c('Id','First','Last','Age','Pos','GP','G','A','PPG','PPA','SHG', 'SHA','PIM','S','HIT','BLK')]
  return(skaters_stats)
}


get_goalie_stats <- function(year){
  #Pull from html
  goalies_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_goalies.html",sep = '')) #Reads html of hockey-reference
  goalies_stats <- goalies_url %>% html_node("table") %>% html_table(fill=TRUE) #Pulls table of players stats
  #Moves around headers
  names(goalies_stats) <- as.character(goalies_stats[1,])
  goalies_stats <- goalies_stats[-1,]
  row_nums <- goalies_stats[1]
  
  #Removes duplicate players and header rows
  rows <- c()
  count <- 1
  num <- 1
  for (row in row_nums) {
    for(r in row) {
      if(r == "Rk" || r != num){
        rows <- append(rows, count)
      }
      else{
        num <- num + 1
      }
      count <- count + 1
    }
  }
  goalies_stats <- goalies_stats[-rows,]
  #Takes rows necessary for fantasy
  goalies_stats <- goalies_stats[, c('Player','Age','GP','GS','W','SV','SV%','SO')]
  names(goalies_stats)[7] <- 'SVP'
  #Formats data into numeric form
  if (year == 2021){
    fact <- 82/56
  }else if (year == 2020){
    fact <- 82/70
  }else{
    fact <- 1
  }
  goalies_stats$GP <- as.numeric(goalies_stats$GP) * fact
  goalies_stats$GS <- as.numeric(goalies_stats$GS) * fact
  goalies_stats$W <- as.numeric(goalies_stats$W) * fact
  goalies_stats$SV <- as.numeric(goalies_stats$SV) * fact
  goalies_stats$SVP <- as.numeric(goalies_stats$SVP)
  goalies_stats$SO <- as.numeric(goalies_stats$SO) * fact
  goalies_stats <- separate(goalies_stats, Player, c('First', 'Last'), sep = ' ', extra='merge')
  goalies_stats$Id <- tolower(paste(substr(goalies_stats$First,1,1), str_replace_all(substr(goalies_stats$Last,1,10), pattern=" ", repl=""), (year - as.numeric(goalies_stats$Age)), sep = ''))
  goalies_stats <- goalies_stats[, c('Id','First','Last','Age','GP','GS','W','SV','SVP','SO')]
  return(goalies_stats)
}


skater_stats <- get_skater_stats(2022)
goalie_stats <- get_goalie_stats(2022)
stats <- rbind.fill(goalie_stats,skater_stats)
stats$Pos <- stats$Pos %>% replace_na("G")
players_stats <- stats[,c(1,2,3,11)]
names(players_stats) <- c('Id', 'first', 'last', 'pos')
stats <- stats[,-c(2,3,4,11)]
stats <- stats[order(stats$Id),]
dbWriteTable(mydb, "stats", stats, overwrite = TRUE)

players <- dbReadTable(mydb, "players")
players <- rbind(players, players_stats)
players <- players[!duplicated(players),]
players <- dbWriteTable(mydb, "players", players, overwrite = TRUE)

