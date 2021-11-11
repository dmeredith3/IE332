library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)

get_goalie_stats <- function(year){
  #Pull from html
  goalies_url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_", as.character(year), "_goalies.html")) #Reads html of hockey-reference
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

get_goalie_prev <- function(year, stat){
  cur_stats <- get_goalie_stats(year)[,c('Id', 'Age', stat)]
  last_stats <- get_goalie_stats(year - 1)[,c('Id', 'Age', stat)]
  prev_stats <- get_goalie_stats(year - 2)[,c('Id', 'Age', stat)]
  #dprev_stats <- get_goalie_stats(year - 3)
  
  goals <- merge(cur_stats, last_stats, by = 'Id', all = TRUE)
  goals <- merge(goals, prev_stats, by = 'Id', all = TRUE)
  return(goals)
}

get_goalie_pred <- function(stat){
  stats <- get_goalie_stats(2018)[,c('Id',stat)]
  names(stats)[2] <- 'Stat'
  goalie_data <- get_goalie_prev(2017, stat)
  goalie_data <- merge(goalie_data, stats, by = 'Id',all = FALSE)
  goalie_data <- goalie_data[,-1]
  goalie_data[is.na(goalie_data)] <- 0 

  stats2 <- get_goalie_stats(2017)[,c('Id',stat)]
  names(stats2)[2] <- 'Stat'
  goalie_data2 <- get_goalie_prev(2016, stat)
  goalie_data2 <- merge(goalie_data2, stats2, by = 'Id',all = FALSE)
  goalie_data2 <- goalie_data2[,-1]
  goalie_data2[is.na(goalie_data2)] <- 0 
  goalie_data <- rbind(goalie_data,goalie_data2)
  
  stats2 <- get_goalie_stats(2016)[,c('Id', stat)]
  names(stats2)[2] <- 'Stat'
  goalie_data2 <- get_goalie_prev(2015, stat)
  goalie_data2 <- merge(goalie_data2, stats2, by = 'Id',all = FALSE)
  goalie_data2 <- goalie_data2[,-1]
  goalie_data2[is.na(goalie_data2)] <- 0 
  goalie_data <- rbind(goalie_data,goalie_data2)
  
  stats2 <- get_goalie_stats(2015)[,c('Id',stat)]
  names(stats2)[2] <- 'Stat'
  goalie_data2 <- get_goalie_prev(2014, stat)
  goalie_data2 <- merge(goalie_data2, stats2, by = 'Id',all = FALSE)
  goalie_data2 <- goalie_data2[,-1]
  goalie_data2[is.na(goalie_data2)] <- 0 
  goalie_data <- rbind(goalie_data,goalie_data2)
  # first we split between training and testing sets
  
  split <- initial_split(goalie_data, prop = 9/10)
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
    ) 
  spec_prep <- fit(spec)
  #str(spec_prep$dense_features())
  
  
  model <- keras_model_sequential() %>% 
    layer_dense_features(dense_features(spec_prep)) %>% 
    layer_dense(units = 256, activation = "relu") %>% 
    layer_dense(units = 128, activation = "relu") %>% 
    layer_dense(units = 128, activation = "relu") %>% 
    layer_dense(units = 1, activation = "relu")
  
  
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
  
  prev <- get_goalie_prev(2021,stat)
  Id <-get_goalie_prev(2022, "GS")[,c('Id')]
  ids <- data.frame(Id)
  eval <- merge(ids, prev, by = 'Id',all.x = TRUE)
  eval[is.na(eval)] <- 0 
  return(as.matrix(predict(model, eval)))
}

cur_stats <- get_goalie_stats(2022)[,c('Id', 'First','Last')]
last_stats <- get_goalie_stats(2021)[,c('Id', 'First','Last')]
prev_stats <- get_goalie_stats(2020)[,c('Id', 'First','Last')]
#dprev_stats <- get_goalie_stats(year - 3)

Goalies <- merge(cur_stats, last_stats, by = 'Id', all = TRUE)
Goalies <- merge(Goalies, prev_stats, by = 'Id', all = TRUE)
for (row in 1:nrow(Goalies)) {
  if (is.na(Goalies[row,2])){
    if(is.na(Goalies[row,4])){
      Goalies[row,2] <- Goalies[row,6]
      Goalies[row,3] <- Goalies[row,7]
    }
    else{
      Goalies[row,2] <- Goalies[row,4]
      Goalies[row,3] <- Goalies[row,5]
    }
  }
}
Goalies <- Goalies[,c('Id','First.x','Last.x')]
names(Goalies) <- c('Id','first','last')

GS <- as.numeric(get_goalie_pred('GS'))
GS[is.na(GS)] <- 0
W<- as.numeric(get_goalie_pred('W'))
W[is.na(W)] <- 0
SV <- as.numeric(get_goalie_pred('SV'))
SV[is.na(SV)] <- 0
SVP <- as.numeric(get_goalie_pred('SVP'))
SVP[is.na(SVP)] <- 0
SO <- as.numeric(get_goalie_pred('SO'))
SO[is.na(SO)] <- 0

goalie_predictions <- data.frame(Goalies[1],GS,W,SV,SVP,SO)

