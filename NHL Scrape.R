library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)

get_stats <- function(year){
  #Pull from html
  skaters_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_skaters.html")) #Reads html of hockey-reference
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
  }
  else if (year == 2020){
    fact <- 82/70
  }
  else{
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
  #Calculates fantasy points
  skaters_stats$Points <- skaters_stats$G * 2 + skaters_stats$A * 1 + (skaters_stats$PPG + skaters_stats$PPA + skaters_stats$SHG + skaters_stats$SHA + skaters_stats$BLK) * 0.5 + (skaters_stats$S + skaters_stats$HIT) * 0.1
  skaters_stats$Pos[skaters_stats$Pos == "C"] <- "F"
  skaters_stats$Pos[skaters_stats$Pos == "LW"] <- "F"
  skaters_stats$Pos[skaters_stats$Pos == "RW"] <- "F"
  skaters_stats$Pos[skaters_stats$Pos == "W"] <- "F"
  return(skaters_stats)
}

