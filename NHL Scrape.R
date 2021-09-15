library(rvest)
library(dplyr)
library(stringr)

get_stats <- function(year){
  #Pull from html
  skaters_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_skaters.html")) #Reads html of hockey-reference
  skaters_stats <- skaters_url %>% html_node("table") %>% html_table(fill=TRUE) #Pulls table of players stats
  #Moves around headers
  names(skaters_stats) <- skaters_stats[1,] 
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
  skaters_stats$GP <- as.numeric(skaters_stats$GP)
  skaters_stats$G <- as.numeric(skaters_stats$G)
  skaters_stats$A <- as.numeric(skaters_stats$A)
  skaters_stats$PPG <- as.numeric(skaters_stats$PPG)
  skaters_stats$PPA <- as.numeric(skaters_stats$PPA)
  skaters_stats$SHG <- as.numeric(skaters_stats$SHG)
  skaters_stats$SHA <- as.numeric(skaters_stats$SHA)
  skaters_stats$PIM <- as.numeric(skaters_stats$PIM)
  skaters_stats$S <- as.numeric(skaters_stats$S)
  skaters_stats$HIT <- as.numeric(skaters_stats$HIT)
  skaters_stats$BLK <- as.numeric(skaters_stats$BLK)
  #Calculates fantasy points
  skaters_stats$Points <- skaters_stats$G * 2 + skaters_stats$A * 1 + (skaters_stats$PPG + skaters_stats$PPA + skaters_stats$SHG + skaters_stats$SHA + skaters_stats$BLK) * 0.5 + (skaters_stats$S + skaters_stats$HIT) * 0.1  
  return(skaters_stats)
}
#Gets stats from the last three years
current_stats <- get_stats(2021)
last_stats <- get_stats(2020)
prev_stats <- get_stats(2019)
