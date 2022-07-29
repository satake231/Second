library(openssl)

SEED <- '2022_data_science'

password_generator <- function(x=SEED,y=URL){
  SEED_and_URL <- paste(x,y,sep='')
  sha_string <- sha256(SEED_and_URL)
  Archduke <- bignum(sha_string, hex=TRUE)
  password_vector <- c(LETTERS,letters,0:9)
  pass <- c()
  for (i in 1:10){
    id <- Archduke %% bignum(length(password_vector)) +1
    pass <- c(pass,password_vector[as.integer(id)])
    Archduke <- Archduke %/% bignum(length(password_vector))
  }
  return(paste(pass, collapse = ""))
}

password_generator(SEED,URL)

Data <- read.csv( "site_list.csv")
for (i in 1:10){
  URL <- Data$URL[i]
  Data$password[i] <- password_generator()
}

Data
write.csv(Data,"Result of generator 2225705.csv" )