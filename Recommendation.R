library(recommenderlab)

library(ggplot2)                       

library(data.table) #For converting the Data into more Usable form 

library(reshape2)

#For importing the data setting the Working Directory
setwd("G:\\Movie-Recommendation-System") 

#Loading the Movie and Rating Data

movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE) 

rating_data <- read.csv("ratings.csv")

#Eliminating the '|' symbol and converting the genre into a sparse matrix

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE) #Taking the genres as a data frame

#Converting the genres into a matrix like data frame 

movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE) 

colnames(movie_genre2) <- c(1:10) #Set names for the matrix like object

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0,10330,18) #Creating a matrix of 10330 rows(observations) and 18 columns(list of genres)

colnames(genre_mat1) <- list_genre #Set Column names as genres

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) #Set Values as 1 for the present genre
    genre_mat1[index+1,gen_col] <- 1
  }
}

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
}

#Create a matrix to search for a movie by genre and year

years <- as.data.frame(movie_data$title, stringsAsFactors=FALSE)## getting the movie names into years

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))## getting the last characters in a string
}

#Separating the years

years <- as.data.frame(substr(substrRight(substrRight(years$`movie_data$title`, 6),5),1,4))

#Building the Original Matrix (Search Matrix by changing the genre in movie_data to the modified genre sparse matrix)

SearchMatrix <- cbind(movie_data[,1], substr(movie_data[,2],1,nchar(movie_data[,2])-6), years, genre_mat2)

colnames(SearchMatrix) <- c("movieId", "title", "year", list_genre)

#Write it into a file

write.csv(SearchMatrix, "search.csv")

# Example of search an Action movie produced in 1995:

subset(SearchMatrix, Action == 1 & year == 1995)$title


##Creating a user profile

binary_rating<-rating_data

# ratings of 4 and 5 are mapped to 1, 
# representing likes, and ratings of 3 
# and below are mapped to -1, representing 
# dislikes:

for(x in 1:nrow(binary_rating))
{
  if(binary_rating[x,3]>3){
    binary_rating[x,3]<-1
  }
  else{
    binary_rating[x,3]<-0
  }
}

# convert binaryratings matrix to the correct format:

binaryrating_new <- dcast(binary_rating, movieId~userId, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryrating_new)){
  binaryrating_new[which(is.na(binaryrating_new[,i]) == TRUE),i] <- 0 #remove na
}
binaryratings_new = binaryrating_new[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

#Remove rows that are not rated from movies dataset

movieId <- length(unique(movie_data$movieId)) #10329

ratingmovieIds <- length(unique(rating_data$movieId)) #10325

movies_new <- movie_data[-which((movie_data$movieId %in% rating_data$movieId) == FALSE),]

rownames(movies_new) <- NULL

#Remove rows that are not rated from genre_matrix2

genre_matrix3 <- genre_mat2[-which((movie_data$movieId %in% rating_data$movieId) == FALSE),]

rownames(genre_matrix3) <- NULL

# calculate the dot product of the genre matrix and 
# the ratings matrix and obtain the user profiles

#Calculate dot product for User Profiles

result = matrix(0,18,668) # Here, 668=no of users/raters, 18=no of genres

for (c in 1:ncol(binaryratings_new)){

    for (i in 1:ncol(genre_matrix3)){
  
        result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings_new[,c])) #ratings per genre
  }
}

#Convert to Binary scale

for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

## Assume that users like similar items, and retrieve movies 
# that are closest in similarity to a user's profile, which 
# represents a user's preference for an item's feature.
# use Jaccard Distance to measure the similarity between user profiles
## Jaccard distance calculats the similarity among clusters
# The User-Based Collaborative Filtering Approach


