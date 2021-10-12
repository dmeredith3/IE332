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

create_prev <- function(year){
  prev_stats <- get_stats(year)
  last_stats <- get_stats(year - 1)
  prev_stats <- get_stats(year - 2)
  #dprev_stats <- get_stats(year - 3)
  
  goals <- merge(prev_stats, last_stats, by = 'Player', all = TRUE)
  goals <- merge(goals, prev_stats, by = 'Player', all = TRUE)
  
  return(goals)
}

get_pred <- function(stat){
  stats <- get_stats(2021)[,c('Player',stat)]
  names(stats)[2] <- 'Goals'
  goals <- create_prev(2020)
  goals <- merge(goals, stats, by = 'Player',all = FALSE)
  goals <- goals[,-1]
  goals[is.na(goals)] <- 0 
  
  # first we split between training and testing sets
  split <- initial_split(goals, prop = 4/5)
  train <- training(split)
  val <- testing(split)
  
  df_to_dataset <- function(df, shuffle = TRUE, batch_size = 32) {
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
  
  spec <- feature_spec(train_ds, Goals ~ .)
  
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
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1, activation = "relu")
  
  
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = "adam", 
  )
  
  history <- model %>% 
    fit(
      dataset_use_spec(train_ds, spec = spec_prep),
      epochs = 50, 
      validation_data = dataset_use_spec(train_ds, spec_prep),
      verbose = 2
    )
  
  test <- create_prev(2021)[,-1]
  return(as.matrix(predict(model, test)))
}

player <- create_prev(2021)[,1]
GP <- as.numeric(get_pred('GP'))
GP[is.na(GP)] <- 0
G <- as.numeric(get_pred('G'))
G[is.na(G)] <- 0
A<- as.numeric(get_pred('A'))
A[is.na(A)] <- 0
PPG <- as.numeric(get_pred('PPG'))
PPG[is.na(PPG)] <- 0
PPA <- as.numeric(get_pred('PPA'))
PPA[is.na(PPA)] <- 0
SHG <- as.numeric(get_pred('SHG'))
SHG[is.na(SHG)] <- 0
SHA <- as.numeric(get_pred('SHA'))
SHA[is.na(SHA)] <- 0
PIM <- as.numeric(get_pred('PIM'))
PIM[is.na(PIM)] <- 0
S <- as.numeric(get_pred('S'))
S[is.na(S)] <- 0
HIT <- as.numeric(get_pred('HIT'))
HIT[is.na(HIT)] <- 0
BLK <- as.numeric(get_pred('BLK'))
BLK[is.na(BLK)] <- 0
predictions <- data.frame(player,GP,G,A,PPG,PPA,SHG,SHA,PIM,S,HIT,BLK)
predictions$Points <- predictions$G * 2 + predictions$A * 1 + (predictions$PPG + predictions$PPA + predictions$SHG + predictions$SHA + predictions$BLK) * 0.5 + (predictions$S + predictions$HIT) * 0.1


