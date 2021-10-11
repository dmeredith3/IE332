library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)

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

calc_averages <- function(year){
  current_stats <- get_stats(year)
  last_stats <- get_stats(year - 1)
  prev_stats <- get_stats(year - 2)
  players <- current_stats[1]
  player_num <- 1
  stats <- (rbind(last_stats, prev_stats))
  stats <- (rbind(stats, current_stats))
  stats$Pos[stats$Pos == "C"] <- "F"
  stats$Pos[stats$Pos == "LW"] <- "F"
  stats$Pos[stats$Pos == "RW"] <- "F"
  stats$Pos[stats$Pos == "W"] <- "F"
  stats$GPG <- stats$G/stats$GP
  stats <- stats[,c('Pos','Age','GPG','GP')]
  stats <- stats %>%
    group_by(Pos, Age) %>% 
    summarise(uGPG = weighted.mean(GPG, GP), varGPG = weightedVar(GPG, GP), GP=sum(GP)) 
  stats$error <- qt(0.975, df=(stats$GP-1/200))*sqrt(stats$varGPG)/sqrt(stats$GP/200)
  stats$LBG <- stats$uGPG - stats$error
  stats$UBG <- stats$uGPG + stats$error
  return(stats)
}

create_model <- function(year){
  pred_stats <- get_stats(year)
  last_stats <- get_stats(year - 1)
  prev_stats <- get_stats(year - 2)
  prev_stats <- get_stats(year - 3)
  
  
  players <- current_stats[1]
  player_num <- 1
  stats <- (merge(last_stats, prev_stats, by = 'Player'))
  stats <- (merge(stats, current_stats, by = 'Player'))
  stats$GPG.x <- stats$G.x/stats$GP.x
  stats$GPG.y <- stats$G.y/stats$GP.y
  stats$GPG <- stats$G/stats$GP
  #stats$uG <- min(c(1, stats$GP/100)) * rowMeans(subset(stats, select = c('GPG', 'GPG.x','GPG.y')), na.rm = TRUE) + (1-min(c(1, stats$GP/100))) * 
  stats$uG <- rowMeans(subset(stats, select = c('GPG', 'GPG.x','GPG.y')), na.rm = TRUE)
  stats$varG <- rowVars(as.matrix(stats[,c('GPG', 'GPG.x','GPG.y')]))
  goals <- stats[,c('Player', 'Pos', 'Age', 'Age.x', 'Age.y', 'GPG', 'GPG.x', 'GPG.y','uG','varG')]
  goals <- goals[,c('Player','Pos', 'Age', 'uG','varG')]
  goals$Pos[goals$Pos == "C"] <- "F"
  goals$Pos[goals$Pos == "LW"] <- "F"
  goals$Pos[goals$Pos == "RW"] <- "F"
  goals$Pos[goals$Pos == "W"] <- "F"
  return(goals)
}

stats <- calc_averages(2021)
goals <- create_model(2021)
ggplot(stats, aes(x = Age, y = uGPG, colour = Pos, group = Pos)) + geom_line() +
  geom_ribbon(aes(x = Age, ymin= LBG, ymax = UBG), fill= "red", alpha = 0.3)
  #geom_line(aes(x = Age, y = uGPG, color = key))
