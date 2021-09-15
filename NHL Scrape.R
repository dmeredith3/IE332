library(rvest)
library(dplyr)
library(stringr)

get_stats <- function(year){
  skaters_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_skaters.html"))
  skaters_stats <- skaters_url %>% html_node("table") %>% html_table(fill=TRUE)
  skaters_ref <- skaters_stats
  names(skaters_stats) <- skaters_stats[1,]
  skaters_stats <- skaters_stats[-1,]
  row_nums <- skaters_stats[1]
  
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
  skaters_stats$PP1 <- skaters_stats$PP
  skaters_stats$PP <- NULL
  skaters_stats$PP <- as.numeric(skaters_stats$PP1) + as.numeric(skaters_stats$PP)
  skaters_stats$SH1 <- skaters_stats$SH
  skaters_stats$SH <- NULL
  skaters_stats$SH <- as.numeric(skaters_stats$SH1) + as.numeric(skaters_stats$SH)
  skaters_stats <- skaters_stats[, c('Player','Age','Pos','GP','G','A','PP','SH','S','HIT','BLK')]
  skaters_stats$GP <- as.numeric(skaters_stats$GP)
  skaters_stats$G <- as.numeric(skaters_stats$G)
  skaters_stats$A <- as.numeric(skaters_stats$A)
  skaters_stats$PP <- as.numeric(skaters_stats$PP)
  skaters_stats$SH <- as.numeric(skaters_stats$SH)
  skaters_stats$S <- as.numeric(skaters_stats$S)
  skaters_stats$HIT <- as.numeric(skaters_stats$HIT)
  skaters_stats$BLK <- as.numeric(skaters_stats$BLK)
  skaters_stats$points <- skaters_stats$G * 2 + skaters_stats$A * 1 + (skaters_stats$PP + skaters_stats$SH + skaters_stats$BLK) * 0.5 + (skaters_stats$S + skaters_stats$HIT) * 0.1  
  return(skaters_stats)
}
current_stats <- get_stats(2021)
last_stats <- get_stats(2020)
prev_stats <- get_stats(2019)
