library(DBI)
library(odbc)
library(RMySQL) #if already installed
library(RMariaDB)
library(plyr)

mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

stats <- dbReadTable(mydb, "stats")
predictions <- dbReadTable(mydb, "predictions")
players <- dbReadTable(mydb, "players")

stats <- merge(stats, players, by = 'Id')
stats_goalies <- stats[(stats$pos == 'G'),][,c(1,3,4,5,6,7)]
stats_skaters <- stats[(stats$pos != 'G'),][,c(1,20,8,9,10,11,12,13,14,15,16,17)]

predictions <- merge(predictions, players, by = 'Id')
predictions_goalies <- predictions[(predictions$pos == 'G'),][,c(1,2,3,4,5,6)]
predictions_skaters <- predictions[(predictions$pos != 'G'),][,c(1,19,7,8,9,10,11,12,13,14,15,16)]

skater_cats <- c('G', 'A','S','HIT','BLK')
goalie_cats <- c('W','SV','SVP','SO')

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

skater_stats21 <- get_skater_stats(2021)
names(skater_stats21)[5] <- 'pos'
goalie_stats21 <- get_goalie_stats(2021)
skater_stats20 <- get_skater_stats(2020)
names(skater_stats20)[5] <- 'pos'
goalie_stats20 <- get_goalie_stats(2020)
skater_stats19 <- get_skater_stats(2019)
names(skater_stats19)[5] <- 'pos'
goalie_stats19 <- get_goalie_stats(2019)

stats_goalies21 <- get_goalie_score(goalie_stats21, goalie_cats)[,c(1,length(goalie_cats) + 2)]
names(stats_goalies21) <- c('Id', 'Scores21')
stats_goalies20 <- get_goalie_score(goalie_stats20, goalie_cats)[,c(1,length(goalie_cats) + 2)]
names(stats_goalies20) <- c('Id', 'Scores20')
stats_goalies19 <- get_goalie_score(goalie_stats19, goalie_cats)[,c(1,length(goalie_cats) + 2)]
names(stats_goalies19) <- c('Id', 'Scores19')
stats_skaters21 <- get_skater_score(skater_stats21, skater_cats)[,c(1,length(skater_cats) + 3)]
names(stats_skaters21) <- c('Id', 'Scores21')
stats_skaters20 <- get_skater_score(skater_stats20, skater_cats)[,c(1,length(skater_cats) + 3)]
names(stats_skaters20) <- c('Id', 'Scores20')
stats_skaters19 <- get_skater_score(skater_stats19, skater_cats)[,c(1,length(skater_cats) + 3)]
names(stats_skaters19) <- c('Id', 'Scores19')

predictions_goalies <- get_goalie_score(predictions_goalies, goalie_cats)[,c(1,length(goalie_cats) + 2)]
names(stats_goalies) <- c('Id', 'PScores')
predictions_skaters <- get_skater_score(predictions_skaters, skater_cats)[,c(1,length(skater_cats) + 3)]
names(predictions_skaters) <- c('Id', 'PScores')

skaters_bb <- merge(stats_skaters21, predictions_skaters, by = 'Id', all = TRUE)
skaters_bb <- merge(stats_skaters20, skaters_bb, by = 'Id', all = TRUE)
skaters_bb <- merge(stats_skaters19, skaters_bb, by = 'Id', all = TRUE)
skaters_bb$avr <- rowMeans(skaters_bb[,c(2,3,4)], na.rm = TRUE)
skaters_bb$sd <- rowSds(as.matrix(skaters_bb[,c(2,3,4)]), na.rm = TRUE)
skaters_bb$sds <- (skaters_bb$PScores - skaters_bb$avr)/skaters_bb$sd

skaters_bb$sd <- sd(skaters_bb$`Actual Scores 19`)
goalies_bb <- merge(stats_goalies21, predictions_goalies, by = 'Id', all = TRUE)
goalies_bb <- merge(stats_goalies20, goalies_bb, by = 'Id', all = TRUE)
goalies_bb <- merge(stats_goalies19, goalies_bb, by = 'Id', all = TRUE)
goalies_bb$avr <- rowMeans(goalies_bb[,c(2,3,4)])

skaters_bb <- skaters_bb[complete.cases(skaters_bb), ]
goalies_bb <- skaters_bb[complete.cases(goalies_bb), ]
plot(skaters_bb$`Actual Scores`,skaters_bb$`Predicted Scores`)
