library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)

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


get_skater_prev <- function(year, stat){
  cur_stats <- get_skater_stats(year)[,c('Id', 'Pos', 'Age', stat)]
  last_stats <- get_skater_stats(year - 1)[,c('Id', 'Pos', 'Age', stat)]
  prev_stats <- get_skater_stats(year - 2)[,c('Id', 'Pos', 'Age', stat)]
  #dprev_stats <- get_skater_stats(year - 3)
  
  goals <- merge(cur_stats, last_stats, by = 'Id', all = TRUE)
  goals <- merge(goals, prev_stats, by = 'Id', all = TRUE)
  
  return(goals)
}

get_skater_pred <- function(stat){
  stats <- get_skater_stats(2018)[,c('Id',stat)]
  names(stats)[2] <- 'Stat'
  goalie_data <- get_skater_prev(2017, stat)
  goalie_data <- merge(goalie_data, stats, by = 'Id',all = FALSE)
  goalie_data <- goalie_data[,-1]
  goalie_data[is.na(goalie_data)] <- 0 
  
  stats2 <- get_skater_stats(2017)[,c('Id',stat)]
  names(stats2)[2] <- 'Stat'
  goalie_data2 <- get_skater_prev(2016, stat)
  goalie_data2 <- merge(goalie_data2, stats2, by = 'Id',all = FALSE)
  goalie_data2 <- goalie_data2[,-1]
  goalie_data2[is.na(goalie_data2)] <- 0 
  goalie_data <- rbind(goalie_data,goalie_data2)
  
  stats2 <- get_skater_stats(2016)[,c('Id', stat)]
  names(stats2)[2] <- 'Stat'
  goalie_data2 <- get_skater_prev(2015, stat)
  goalie_data2 <- merge(goalie_data2, stats2, by = 'Id',all = FALSE)
  goalie_data2 <- goalie_data2[,-1]
  goalie_data2[is.na(goalie_data2)] <- 0 
  goalie_data <- rbind(goalie_data,goalie_data2)
  
  stats2 <- get_skater_stats(2015)[,c('Id',stat)]
  names(stats2)[2] <- 'Stat'
  goalie_data2 <- get_skater_prev(2014, stat)
  goalie_data2 <- merge(goalie_data2, stats2, by = 'Id',all = FALSE)
  goalie_data2 <- goalie_data2[,-1]
  goalie_data2[is.na(goalie_data2)] <- 0 
  goalie_data <- rbind(goalie_data,goalie_data2)
  # first we split between training and testing sets
  
  max <- max(goalie_data$Stat)
  goalie_data$Stat <- goalie_data$Stat/max(goalie_data$Stat)
  
  split <- initial_split(goalie_data, prop = 4/5)
  train <- training(split)
  val <- testing(split)
  
  df_to_dataset <- function(df, shuffle = TRUE, batch_size = 64) {
    ds <- df %>% 
      tensor_slices_dataset()
    
    if (shuffle)
      ds <- ds %>% dataset_shuffle(buffer_size = nrow(df))
    
    ds %>% 
      dataset_batch(batch_size = batch_size)
  }
  
  train_ds <- df_to_dataset(train)
  val_ds <- df_to_dataset(val, shuffle = FALSE)
  
  #train_ds %>% 
  #reticulate::as_iterator() %>% 
  #reticulate::iter_next() %>% 
  #str()
  
  spec <- feature_spec(train_ds, Stat ~ .)
  
  spec <- spec %>% 
    step_numeric_column(
      all_numeric(), 
      normalizer_fn = scaler_standard()
    ) %>% 
    step_categorical_column_with_vocabulary_list(c(Pos.x, Pos.y, Pos))
  
  spec_prep <- fit(spec)
  #str(spec_prep$dense_features())
  

  model <- keras_model_sequential() %>% 
    layer_dense_features(dense_features(spec_prep)) %>% 
    layer_dense(units = 256, activation = "sigmoid") %>% 
    layer_dense(units = 128, activation = "sigmoid") %>% 
    layer_dense(units = 128, activation = "sigmoid") %>% 
    layer_dense(units = 1, activation = "sigmoid")


  model %>% compile(
    loss = "mean_squared_error",
    optimizer = "adam", 
  )

  
  history <- model %>% 
    fit(
      dataset_use_spec(train_ds, spec = spec_prep),
      epochs = 100, 
      validation_data = dataset_use_spec(train_ds, spec_prep),
      verbose = 2
    )
  
  prev <- get_skater_prev(2021,stat)
  Id <-get_skater_prev(2022, "G")[,c('Id')]
  ids <- data.frame(Id)
  eval <- merge(ids, prev, by = 'Id',all.x = TRUE)
  eval[is.na(eval)] <- 0 
  return(as.matrix(predict(model, eval) * max))
}


cur_stats <- get_skater_stats(2022)[,c('Id', 'First','Last','Pos')]
last_stats <- get_skater_stats(2021)[,c('Id', 'First','Last','Pos')]
prev_stats <- get_skater_stats(2020)[,c('Id', 'First','Last','Pos')]
#dprev_stats <- get_skater_stats(year - 3)

Skaters <- merge(cur_stats, last_stats, by = 'Id', all = TRUE)
Skaters <- merge(Skaters, prev_stats, by = 'Id', all = TRUE)
for (row in 1:nrow(Skaters)) {
  if (is.na(Skaters[row,4])){
    if(is.na(Skaters[row,7])){
      Skaters[row,4] <- Skaters[row,10]
      Skaters[row,2] <- Skaters[row,8]
      Skaters[row,3] <- Skaters[row,9]
    }
    else{
      Skaters[row,4] <- Skaters[row,7]
      Skaters[row,2] <- Skaters[row,5]
      Skaters[row,3] <- Skaters[row,6]
    }
  }
}
Skaters <- Skaters[,c('Id','First.x','Last.x','Pos.x')]
names(Skaters) <- c('Id','first','last','pos')
G <- as.numeric(get_skater_pred('G'))
G[is.na(G)] <- 0
A<- as.numeric(get_skater_pred('A'))
A[is.na(A)] <- 0
PPG <- as.numeric(get_skater_pred('PPG'))
PPG[is.na(PPG)] <- 0
PPA <- as.numeric(get_skater_pred('PPA'))
PPA[is.na(PPA)] <- 0
SHG <- as.numeric(get_skater_pred('SHG'))
SHG[is.na(SHG)] <- 0
SHA <- as.numeric(get_skater_pred('SHA'))
SHA[is.na(SHA)] <- 0
PIM <- as.numeric(get_skater_pred('PIM'))
PIM[is.na(PIM)] <- 0
S <- as.numeric(get_skater_pred('S'))
S[is.na(S)] <- 0
HIT <- as.numeric(get_skater_pred('HIT'))
HIT[is.na(HIT)] <- 0
BLK <- as.numeric(get_skater_pred('BLK'))
BLK[is.na(BLK)] <- 0

skater_predictions <- data.frame(Skaters[1],G,A,PPG,PPA,SHG,SHA,PIM,S,HIT,BLK)


