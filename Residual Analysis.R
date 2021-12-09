library(DBI)
library(odbc)
library(RMySQL) #if already installed
library(RMariaDB)
library(plyr)
library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)

mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

stats <- dbReadTable(mydb, "stats")[,-c(1)]
predictions <- dbReadTable(mydb, "predictions")[,-c(1)]
players <- dbReadTable(mydb, "players")

stats <- merge(stats, players, by = 'Id')
stats_goalies <- stats[(stats$pos == 'G'),][,c(1,3,4,5,6,7)]
stats_skaters <- stats[(stats$pos != 'G'),][,c(1,20,8,9,10,11,12,13,14,15,16,17)]

predictions <- merge(predictions, players, by = 'Id')
predictions_goalies <- predictions[(predictions$pos == 'G'),][,c(1,2,3,4,5,6)]
predictions_skaters <- predictions[(predictions$pos != 'G'),][,c(1,19,7,8,9,10,11,12,13,14,15,16)]

skater_cats <- c('G', 'A','S','HIT','BLK')
goalie_cats <- c('W','SV','SVP','SO')

#gets skater stats for a specific year
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
  #Formats data into numeric form and normalizes for COVID-19 affected years
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

get_skater_score <- function(stats, skater_cats){
  players <- stats[,c("Id","pos", skater_cats)]
  F_D <- c()
  pos <- c()
  for (row in 1:nrow(players)){
    if (players[row,2] != 'D'){
      F_D[row] <- 'F'
      pos[row] <- players[row,2]
    }
    else{
      F_D[row] <- 'D'
      pos[row] <- players[row,2]
    }
  }
  players$pos <- F_D
  avg_table <- aggregate(players[, 3:length(players)], list(players$pos), mean)
  names(avg_table) <- 'pos'
  avg_table <- avg_table[order(avg_table$pos),]
  scores <- c()
  for (row in 1:nrow(players)){
    column_score <- c()
    if (players[row,2] == 'F'){
      for (column in 1:(length(skater_cats))){
        if (skater_cats[column] == 'PIM'){
          column_score[column] <- as.numeric(((avg_table[2,1+column] + 1)/(players[row,2+column] + 2)) * 100)
        }
        else{
          column_score[column] <- as.numeric(((players[row,2+column])/ avg_table[2,1+column]) * 100)
        }
      }
      scores[row] <- mean(column_score)
    }
    else{
      for (column in 1:(length(skater_cats))){
        if (skater_cats[column] == 'PIM'){
          column_score[column] <- as.numeric(((avg_table[1,1+column] + 1)/(players[row,2+column] + 2)) * 100)
        }
        else{
          column_score[column] <- as.numeric(((players[row,2+column])/ avg_table[1,1+column]) * 100)
        }
        scores[row] <- mean(column_score)
      }
    }
  }
  players$pos <- pos
  players <- data.frame(players,scores)
  return(players)
}


#get goalie stats given year
get_goalie_stats <- function(year){
  #Pull from html
  goalies_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_goalies.html", sep = '')) #Reads html of hockey-reference
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
  #makes stats numeric
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

get_goalie_score <- function(stats, goalie_cats){
  players <- stats[,c("Id", goalie_cats)]
  if(length(goalie_cats) == 1){
    average <- mean(players[,2])
    for (row in 1:nrow(players)){
      scores <- as.numeric(((players[row,1+column])/ average) * 100)
    }
    players <- data.frame(players,scores)
  }
  else{
    avg_table <- (colMeans(players[, 2:length(players)]))
    scores <- c()
    for (row in 1:nrow(players)){
      column_score <- c()
      for (column in 1:(length(goalie_cats))){
        column_score[column] <- as.numeric(((players[row,1+column])/ avg_table[column]) * 100)
      }
      scores[row] <- mean(column_score)
    }
    players <- data.frame(players,scores)
  }
  return(players)
}

# skater_stats21 <- get_skater_stats(2021)
# names(skater_stats21)[5] <- 'pos'
# goalie_stats21 <- get_goalie_stats(2021)
# skater_stats20 <- get_skater_stats(2020)
# names(skater_stats20)[5] <- 'pos'
# goalie_stats20 <- get_goalie_stats(2020)
# skater_stats19 <- get_skater_stats(2019)
# names(skater_stats19)[5] <- 'pos'
# goalie_stats19 <- get_goalie_stats(2019)
# 
# stats_goalies21 <- get_goalie_score(goalie_stats21, goalie_cats)[,c(1,length(goalie_cats) + 2)]
# names(stats_goalies21) <- c('Id', 'Scores21')
# stats_goalies20 <- get_goalie_score(goalie_stats20, goalie_cats)[,c(1,length(goalie_cats) + 2)]
# names(stats_goalies20) <- c('Id', 'Scores20')
# stats_goalies19 <- get_goalie_score(goalie_stats19, goalie_cats)[,c(1,length(goalie_cats) + 2)]
# names(stats_goalies19) <- c('Id', 'Scores19')
# stats_skaters21 <- get_skater_score(skater_stats21, skater_cats)[,c(1,length(skater_cats) + 3)]
# names(stats_skaters21) <- c('Id', 'Scores21')
# stats_skaters20 <- get_skater_score(skater_stats20, skater_cats)[,c(1,length(skater_cats) + 3)]
# names(stats_skaters20) <- c('Id', 'Scores20')
# stats_skaters19 <- get_skater_score(skater_stats19, skater_cats)[,c(1,length(skater_cats) + 3)]
# names(stats_skaters19) <- c('Id', 'Scores19')

#pulls predicted scores
predictions_goalies <- get_goalie_score(predictions_goalies, goalie_cats)[,c(1,length(goalie_cats) + 2)]
names(stats_goalies) <- c('Id', 'PScores')
predictions_skaters <- get_skater_score(predictions_skaters, skater_cats)[,c(1,length(skater_cats) + 3)]
names(predictions_skaters) <- c('Id', 'PScores')

#boom or bust
# skaters_bb <- merge(stats_skaters21, predictions_skaters, by = 'Id', all = TRUE)
# skaters_bb <- merge(stats_skaters20, skaters_bb, by = 'Id', all = TRUE)
# skaters_bb <- merge(stats_skaters19, skaters_bb, by = 'Id', all = TRUE)
# skaters_bb$avr <- rowMeans(skaters_bb[,c(2,3,4)], na.rm = TRUE)
# skaters_bb$sd <- rowSds(as.matrix(skaters_bb[,c(2,3,4)]), na.rm = TRUE)
# skaters_bb$sds <- (skaters_bb$PScores - skaters_bb$avr)/skaters_bb$sd
# 
# skaters_bb$sd <- sd(skaters_bb$`Actual Scores 19`)
# goalies_bb <- merge(stats_goalies21, predictions_goalies, by = 'Id', all = TRUE)
# goalies_bb <- merge(stats_goalies20, goalies_bb, by = 'Id', all = TRUE)
# goalies_bb <- merge(stats_goalies19, goalies_bb, by = 'Id', all = TRUE)
# goalies_bb$avr <- rowMeans(goalies_bb[,c(2,3,4)])
# 
# skaters_bb <- skaters_bb[complete.cases(skaters_bb[,c(5,6)]), ]
# goalies_bb <- skaters_bb[complete.cases(goalies_bb), ]
# plot(avr ~ PScores, data = skaters_bb)
#combines scores and predicted scoeres
skater_stats22 <- get_skater_stats(2022)
names(skater_stats22)[5] <- 'pos'
stats_skaters22 <- get_skater_score(skater_stats22, skater_cats)[,c(1,length(skater_cats) + 3)]
names(stats_skaters22) <- c('Id', 'Scores22')
skaters_bb <- merge(stats_skaters22, predictions_skaters, by = 'Id', all.y = TRUE)
goalie_stats22 <- get_goalie_stats(2022)
names(goalie_stats22)[5] <- 'pos'
stats_goalies22 <- get_goalie_score(goalie_stats22, goalie_cats)[,c(1,length(goalie_cats) + 2)]
names(stats_goalies22) <- c('Id', 'Scores22')
goalies_bb <- merge(stats_goalies22, predictions_goalies, by = 'Id', all.y = TRUE)
names(goalies_bb) <- c('Id', 'Scores22', 'PScores')
names(skaters_bb) <- c('Id', 'Scores22', 'PScores')
#creates graphs
stats <- rbind(skaters_bb, goalies_bb)
fit <- lm(Scores22 ~ PScores, data = stats)
ggplot(data = stats, aes(x = Scores22, y = PScores), main='2022 Predicted Scores vs Current Scores') + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color='red')+
  labs(title= "2022 Predicted and Curent Scores",
       y="Predicted Scores", x = "Current Scores")
plot(fit, which=1, col=c("blue")) # Residuals vs Fitted Plot

