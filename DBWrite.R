library(DBI)
library(odbc)
library(RMySQL) #if already installed
library(RMariaDB)
library(plyr)
mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

Goalies$pos <- 'G'
players <- rbind(Goalies,Skaters)
players <- arrange(players, Id)
dbWriteTable(mydb, "players", players, append = TRUE,  row.names = FALSE)
predictions <- rbind.fill(goalie_predictions,skater_predictions)
predictions <- arrange(predictions, Id)
predictions$pred_id <- paste('p_', seq_along(predictions[,1]), sep = '')
dbWriteTable(mydb, "predictions", predictions, append = TRUE, row.names = FALSE)

