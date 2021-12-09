stat = 'G'
#makes all testing data
stats <- get_skater_stats(2018)[,c('Id',stat)]
names(stats)[2] <- 'Stat'
skater_data <- get_skater_prev(2017, stat)
skater_data <- merge(skater_data, stats, by = 'Id',all = FALSE)
skater_data <- skater_data[,-1]
skater_data[is.na(skater_data)] <- 0 

stats2 <- get_skater_stats(2017)[,c('Id',stat)]
names(stats2)[2] <- 'Stat'
skater_data2 <- get_skater_prev(2016, stat)
skater_data2 <- merge(skater_data2, stats2, by = 'Id',all = FALSE)
skater_data2 <- skater_data2[,-1]
skater_data2[is.na(skater_data2)] <- 0 
skater_data <- rbind(skater_data,skater_data2)

stats2 <- get_skater_stats(2016)[,c('Id', stat)]
names(stats2)[2] <- 'Stat'
skater_data2 <- get_skater_prev(2015, stat)
skater_data2 <- merge(skater_data2, stats2, by = 'Id',all = FALSE)
skater_data2 <- skater_data2[,-1]
skater_data2[is.na(skater_data2)] <- 0 
skater_data <- rbind(skater_data,skater_data2)

stats2 <- get_skater_stats(2015)[,c('Id',stat)]
names(stats2)[2] <- 'Stat'
skater_data2 <- get_skater_prev(2014, stat)
skater_data2 <- merge(skater_data2, stats2, by = 'Id',all = FALSE)
skater_data2 <- skater_data2[,-1]
skater_data2[is.na(skater_data2)] <- 0 
skater_data <- rbind(skater_data,skater_data2)
# first we split between training and testing sets

#predicts statistics
get_skater_pred <- function(skater_data, size, nodes){
  max <- max(skater_data$Stat)
  skater_data$Stat <- skater_data$Stat/max(skater_data$Stat)
  
  split <- initial_split(skater_data, prop = 4/5)
  train <- training(split)
  val <- testing(split)
  
  df_to_dataset <- function(df, shuffle = TRUE, batch_size = size) {
    ds <- df %>% 
      tensor_slices_dataset()
    
    if (shuffle)
      ds <- ds %>% dataset_shuffle(buffer_size = nrow(df))
    
    ds %>% 
      dataset_batch(batch_size = batch_size)
  }
  
  train_ds <- df_to_dataset(train)
  val_ds <- df_to_dataset(val, shuffle = FALSE)
  
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
    layer_dense(units = nodes, activation = "sigmoid") %>% 
    layer_dense(units = nodes, activation = "sigmoid") %>% 
    layer_dense(units = nodes, activation = "sigmoid") %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  
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
  
  prev <- get_skater_prev(2021,stat)
  Id <-get_skater_prev(2022, "G")[,c('Id')]
  ids <- data.frame(Id)
  eval <- merge(ids, prev, by = 'Id',all.x = TRUE)
  eval[is.na(eval)] <- 0 
  return(as.matrix(predict(model, eval) * max))
}

#current stats
cur_stats <- get_skater_stats(2022)[,c('Id', 'First','Last','Pos')]
last_stats <- get_skater_stats(2021)[,c('Id', 'First','Last','Pos')]
prev_stats <- get_skater_stats(2020)[,c('Id', 'First','Last','Pos')]
#dprev_stats <- get_skater_stats(year - 3)

#gets skater positions
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
#can change based on experiment
G <- as.numeric(get_skater_pred(skater_data, 256, 256))
G[is.na(G)] <- 0