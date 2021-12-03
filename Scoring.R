library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)
library(plyr)

mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

stats <- dbReadTable(mydb, "stats")
players <- dbReadTable(mydb, "players")

stats <- merge(stats, players, by = 'Id')
stats_goalies <- stats[(stats$pos == 'G'),][,c(1,3,4,5,6,7)]
stats_skaters <- stats[(stats$pos != 'G'),][,c(1,20,8,9,10,11,12,13,14,15,16,17)]

skater_cats <- c('G', 'A', 'PIM')
goalie_cats <- c('W','SV','SO')

get_skater_score <- function(stats, skater_cats){
  stats<- stats_skaters
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
  players$pos <- 'G'
  return(players)
}

skater_rankings <- get_skater_score(stats_skaters, skater_cats)
goalie_rankings <- get_goalie_score(stats_goalies, goalie_cats)
rankings <- rbind.fill(goalie_rankings, skater_rankings)
rankings <- rankings %>% replace(.=="NULL", "G") # replace with G

get_vb_scores <- function(rankings){
  goalies <- rankings[rankings$pos == "G",]
  goalies <- goalies[order(-goalies$scores),]
  defenders <- rankings[rankings$pos == "D",]
  defenders <- defenders[order(-defenders$scores),]
  forwards <- rankings[rankings$pos == "C" | rankings$pos == "LW" | rankings$pos == "RW"| rankings$pos == "F" | rankings$pos == "W",]
  forwards <- forwards[order(-forwards$scores),]
  avg_each_other_pos <- mean(mean(goalies$scores) + mean(defenders$scores) + mean(forwards$scores))
  #print(avg_each_other_pos)
  #mean_scores_goalies <- mean(goalies$scores)
  #mean_scores_defenders <- mean(defenders$scores)
  #mean_scores_forwards <- mean(forwards$scores)
  #updated_defenders <- data.frame(defenders$scores * (mean_scores_forwards/mean_scores_defenders))
  #updated_goalies
  
  for (row in 1:nrow(defenders)){
    defenders$vb1[row] <- defenders$scores[row]/ defenders$scores[row+1]
    defenders$vb2[row] <- defenders$scores[row]/ 100
    defenders$vb3[row] <- defenders$scores[row]/ avg_each_other_pos
  }
  #print(defenders)
  for (row in 1:nrow(forwards)){
    forwards$vb1[row] <- forwards$scores[row]/ forwards$scores[row+1]
    forwards$vb2[row] <- forwards$scores[row]/ 100
    forwards$vb3[row] <- forwards$scores[row]/ avg_each_other_pos
  }
  #print(forwards)
  for (row in 1:nrow(goalies)){
    goalies$vb1[row] <- goalies$scores[row]/ goalies$scores[row+1]
    goalies$vb2[row] <- goalies$scores[row]/ 100
    goalies$vb3[row] <- goalies$scores[row]/ avg_each_other_pos
  }
  rankings_with_vbscores <- rbind.fill(defenders, goalies, forwards)
  #print(rankings_with_vbscores)
  return(rankings_with_vbscores)
}


vb_scores <- get_vb_scores(rankings)

