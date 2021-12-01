name <- "Drew-Meredith"
key <- sample(0:26, nchar(name), replace=T)
my_letters <- letters[1:26]

encode <- function(text, key){
  text_split <- strsplit(text, "")[[1]]
  text_encrypt <- c()
  for (letter in text_split){
    letter <- tolower(letter)
    if (letter == '-'){
      text_encrypt <- c(text_encrypt, 0)
    }
    else{
      text_encrypt <- c(text_encrypt, match(letter, my_letters))
    }
  }
  
  for (i in 1:length(text_encrypt)){
    first_dig <- as.numeric((trunc(text_encrypt[i]/10) + trunc(key[i]/10))%%10) 
    second_dig <- as.numeric((trunc(text_encrypt[i]%%10) + trunc(key[i]%%10))%%10) 
    text_encrypt[i] <- as.numeric(paste(first_dig,second_dig, sep = ''))
  }
  return(text_encrypt)
}

decode <- function(encrypted, key){
  text_decrypt <- c()
  for (i in 1:length(encrypted)){
    first_dig <- as.numeric((trunc(encrypted[i]/10) - trunc(key[i]/10))%%10) 
    if (first_dig < 0){
      first_dig <- first_dig + 10
    }
    second_dig <- as.numeric((trunc(encrypted[i]%%10) - trunc(key[i]%%10))%%10)
    if (second_dig < 0){
      second_dig <- second_dig + 10
    }
    text_decrypt[i] <- as.numeric(paste(first_dig,second_dig, sep = ''))
  }
  
  string <- ''
  for (number in text_decrypt){
    if (number == 0){
      string <- paste(string, '-', sep = '')
    }
    else{
      string <- paste(string, letters[number], sep = '')
    }
  }
  return(string)
}
