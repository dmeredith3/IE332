library(DBI)
library(odbc)
library(RMySQL) #if already installed
library(RMariaDB)
library(plyr)
mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

Goalies$pos <- 'G'
players <- rbind(Goalies,Skaters)
players <- arrange(players, Id)
dbWriteTable(mydb, "players", players, overwrite = TRUE)
predictions <- rbind.fill(goalie_predictions,skater_predictions)
predictions <- arrange(predictions, Id)
dbWriteTable(mydb, "predictions", predictions, overwrite = TRUE)

skater_stats <- get_skater_stats(2022)
goalie_stats <- get_goalie_stats(2022)
stats <- rbind.fill(goalie_stats,skater_stats)
stats <- arrange(stats, Id)
dbWriteTable(mydb, "stats", stats, overwrite = TRUE)
stats <- dbReadTable(mydb, "stats")

