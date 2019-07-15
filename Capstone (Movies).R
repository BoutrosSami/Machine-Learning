
## Capstone (Movies)



###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)




##### 
## Q1 Number of rows and columns in 'edx'

nrow(edx)
ncol(edx)

## Q2 how many zero ratings in 'edx' and how many three ratings
## Q7 what are the five most given ratings in order from most to least
## Q8 are half-star ratings more common or less common that whole start ratings

zero_rating <- 0
one_half_rating <- 0
one_rating <- 0
one_and_half_rating <- 0
two_rating <- 0
two_and_half_rating <- 0
three_rating <- 0
three_and_half_rating <- 0
four_rating <- 0
four_and_half_rating <- 0
five_rating <- 0


for (row in 1:nrow(edx)){
  if(edx[row, 3] == 0.0){
    zero_ratings <- zero_ratings + 1
  }
  else if(edx[row,3] == 0.5){
    one_half_rating <- one_half_rating + 1
  }
  else if(edx[row,3] == 1.0){
    one_rating <- one_rating + 1
  }
  else if(edx[row,3] == 1.5){
    one_and_half_rating <- one_and_half_rating + 1
  }
  else if(edx[row,3] == 2.0){
    two_rating <- two_rating + 1
  }
  else if(edx[row,3] == 2.5){
    two_and_half_rating <- two_and_half_rating + 1
  }
  else if(edx[row,3] == 3.0){
    three_rating <- three_rating + 1
  }
  else if(edx[row,3] == 3.5){
    three_and_half_rating <- three_and_half_rating + 1
  }
  else if(edx[row,3] == 4.0){
    four_rating <- four_rating + 1
  }
  else if(edx[row,3] == 4.5){
    four_and_half_rating <- four_and_half_rating + 1
  }
  else if(edx[row,3] == 5.0){
    five_rating <- five_rating + 1
  }
}

zero_rating
one_half_rating
one_rating
one_and_half_rating
two_rating
two_and_half_rating
three_rating
three_and_half_rating
four_rating
four_and_half_rating
five_rating

dup <- 0

for (row in 1:nrow(edx)){
  if (duplicated(unique(edx[row, 5])) == TRUE){
    dup <- dup + 1
  }
}

### Q4 how many different userIDs

unique(edx[,1])


### Q5 how many movie ratings for: Drama, Comedy, Thriller, Romance

drama <- "Drama"
comedy <- "Comedy"
thriller <- "Thriller"
romance <- "Romance"

for (row in 1:nrow(edx)){
  if(grepl())
    
    ####### unfinished
    
    
    ### Q6 Which movie has the greatest number of ratings
    
    
    for (row in 1:nrow(edx)){
      if(edx[row, 6] == 0.0){
        zero_ratings <- zero_ratings + 1
      }
      else if(edx[row, 6] == 0.5){
        one_half_rating <- one_half_rating + 1
      }
      else if(edx[row, 6] == 1.0){
        one_rating <- one_rating + 1
      }
      else if(edx[row, 6] == 1.5){
        one_and_half_rating <- one_and_half_rating + 1
      }
    }
  
  
  