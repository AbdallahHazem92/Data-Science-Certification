# Install all needed libraries if it is not present

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")


# Loading all needed libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(forcats)
library(kableExtra)
library(caret)
library(anytime)
library(tidyverse)
library(lubridate)

############################################################
# Create edx set, validation set, and submission file
#############################################################

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

set.seed(1)
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

########################################################

# Explore Dataset
head(edx) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

########################################################

# Convert timestamp to a human readable date

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

# Extract the year and month of rate in both dataset

edx$yearOfRate <- format(edx$date,"%Y")
edx$monthOfRate <- format(edx$date,"%m")

validation$yearOfRate <- format(validation$date,"%Y")
validation$monthOfRate <- format(validation$date,"%m")


########################################################

# Extract the year of release for each movie in both dataset
# edx dataset

edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,as.integer(str_split(release, "-",
                                                simplify = T)[1]),as.integer(release))) %>%
  mutate(title = if_else(is.na(titleTemp),title,titleTemp)) %>%
  select(-titleTemp)

# validation dataset

validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",simplify = T)[1]),
                           as.integer(release))) %>%
  mutate(title = if_else(is.na(titleTemp),title,titleTemp)) %>%
  select(-titleTemp)

head(edx) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)
################################################
#Ratings distribution
#---------------------
vec_ratings <- as.vector(edx$rating)
vec_ratings <- vec_ratings[vec_ratings != 0]
vec_ratings <- factor(vec_ratings)
qplot(vec_ratings) +
  ggtitle("Ratings' Distribution")
#The above rating distribution shows that the users have a general tendency to rate movies between
#3 and 4. This is a very general conclusion. We should further explore the effect of different features
#to make a good predictive model.

#--------------------------------------
#Movies distribution by Average Rating
edx%>% group_by(movieId)%>% summarise(Average_Rating=mean(rating))%>%
  ggplot(aes(Average_Rating))+geom_histogram(bins=30)+
  ggtitle("Movies by average rating")+theme_bw()+
  geom_vline(xintercept = mean(edx$rating),color="black")  + 
  scale_x_continuous(breaks=c(1,2,3,mean(edx$rating),4,5), labels=c("1","2","3","mean","4","5")) 

#Users distribution by Average Rating
edx%>% group_by(userId)%>% summarise(Average_Rating=mean(rating))%>%
  ggplot(aes(Average_Rating))+geom_histogram(bins=30)+
  ggtitle("Users by average rating")+theme_bw()+
  geom_vline(xintercept = mean(edx$rating),color="black") + 
  scale_x_continuous(breaks=c(1,2,3,mean(edx$rating),4,5), labels=c("1","2","3","mean","4","5")) 

#Movies and users distribution by Average Rating look “Normal”. We can see that there is movie and user
#effects/bias, as “good” movies tempt to be rated higher than others, as well as some users are more easygoing
#than others.



edx %>% group_by(movieId , release)%>% summarise(Average_Rating=mean(rating))%>%
  select(release, movieId, Average_Rating) %>% 
  ggplot(aes(release, Average_Rating)) + 
  geom_point(alpha = 0.50,color = "blue" ) +
  geom_smooth(color = "red")

#There is clearly an effect where the average rating goes down. More striking is that recent movies
#are more likely to receive a bad rating, where the variance of ratings for movies before the early
#seventies is much lower.

# Very early years: very few ratings (very pale colour) possibly since fewer people
#decide to watch older movies.
#• Early years: Strong effect where many ratings are made when the movie is first
#screen, then very quiet period.
#• Recent years: More or less constant colour.


#-----------------------------------------
#Ratings distribution by count

edx %>% group_by(movieId)%>% summarise(Average_Rating=mean(rating), Counts = n())%>%
  select(Counts, movieId, Average_Rating) %>% 
  ggplot(aes(Counts, Average_Rating)) + 
  geom_point(alpha = 0.50,color = "blue" ) +
  geom_smooth(color = "red")

#If a movie is very good, many people will watch it and rate it. In other words, we
#should see some correlation between ratings and numbers of ratings. Again, some sort of
#rescaling of time, logarithmic or other, need considering.
#The effect of good movies attracting many spectators is noticeable. It is also very clear that movies
#with few spectators generate extremely variable results.

#-----------------------------------------
#3- The Model
#3-1 Crating Train and Test set
#First we wust create the train set and test set.

edx <- edx %>% select(userId, movieId, rating)

test_index <- createDataPartition(edx$rating, times = 1, p = .2, list = F)
# Create the index

train <- edx[-test_index, ] # Create Train set
test <- edx[test_index, ] # Create Test set
test <- test %>% # The same movieId and usersId appears in both set. (Not the same cases)
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")
dim(train)
dim(test)

#-----------------------------------------
#3-2 Creating Baseline 
#We will generate basic model which consider the most common rating from the train set to be predicted into the test set. This is the baseline model.  

MU_hat <- mean(train$rating) # Mean accross all movies.
RMSE_baseline <- RMSE(test$rating, MU_hat) # RMSE in test set.
RMSE_baseline

#then, we have the RMSE to be *beaten* by our model.  

TableOfRmse <- data_frame(Method = "Baseline", RMSE = RMSE_baseline)
TableOfRmse %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#We can observe that the RMSE of the most basic model is `r RMSE_baseline`. It's bigger than 1! In this context, this is a very bad model.

#-----------------------------------------
#3-3 User and Movie effect Model
#We are trying to get a better RMSE by considering the user effect and the movie effect as predictors. 

  
MU <- mean(train$rating)

MovieAvarages <- train %>%
  group_by(movieId) %>%
  summarize(MI = mean(rating - MU))

UserAvarages <- test %>%
  left_join(MovieAvarages, by = "movieId") %>%
  group_by(userId) %>%
  summarize(UI = mean(rating - MU - MI))

RatingsOfPredicted <- test %>%
  left_join(MovieAvarages, by = "movieId") %>%
  left_join(UserAvarages, by = "userId") %>%
  mutate(pred = MU + MI + UI) %>% .$pred

RmseModel <- RMSE(RatingsOfPredicted, test$rating)
RmseModel 


TableOfRmse <- rbind(TableOfRmse,
                     data_frame(Method = "User & Movie Effect", RMSE = RmseModel))

TableOfRmse %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#We've got obtained a better RMSE so we can make predictions on Validation data.

##3-4 User and Movie effect Model on validation data

#Validation data set needs to be handled the same as the train data set was handled after that we will be apple to make predictions.


validation <- validation %>% select(userId, movieId, rating)

validation$userId <- as.numeric(validation$userId)
validation$movieId <- as.numeric(validation$movieId)

validation <- validation[complete.cases(validation), ]

MU <- mean(train$rating)

MovieAvarages <- train %>%
  group_by(movieId) %>%
  summarize(MI = mean(rating - MU))

UserAvarages <- test %>%
  left_join(MovieAvarages, by = "movieId") %>%
  group_by(userId) %>%
  summarize(UI = mean(rating - MU - MI))

RatingsOfPredicted <- test %>%
  left_join(MovieAvarages, by = "movieId") %>%
  left_join(UserAvarages, by = "userId") %>%
  mutate(pred = MU + MI + UI) %>% .$pred

PredictedValues <- validation %>%
  left_join(MovieAvarages, by = 'movieId') %>%
  left_join(UserAvarages, by = 'userId') %>%
  mutate(pred = MU + MI + UI) %>% .$pred

RmseTableVal <- data_frame(Method = "User & Movie Effect on validation", RMSE = val_RMSE)
RmseTableVal %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#We can see above that this RMSE is higher than the RMSE on the test set. This is highly probable, given that this is unseeing data. The good thing is that the difference is just `r val_RMSE - model_RMSE`. Now, let's see if *regularisation* give us better results.


#-----------------------------------------
##3-5 Getting Regularisation 
#Regularisation will evaluate the  various values for lambda to give us the corresponding RMSE.

ValuesOfLambda <- seq(0, 7, .2)

RmseFunctionReg <- sapply(ValuesOfLambda, function(l){
  
  MU <- mean(train$rating)
  
  MI <- train %>%
    group_by(movieId) %>%
    summarize(MI = sum(rating - MU)/(n()+l))
  
  UI <- train %>%
    left_join(MI, by="movieId") %>%
    group_by(userId) %>%
    summarize(UI = sum(rating - MI - MU)/(n()+l))
  
  RatingsOfPredicted <- test %>%
    left_join(MI, by = "movieId") %>% 
    left_join(UI, by = "userId") %>%
    mutate(pred = MU + MI + UI) %>% .$pred
  
  return(RMSE(RatingsOfPredicted, test$rating))
})

qplot(ValuesOfLambda, RmseFunctionReg,
      main = "Regularisation",
      xlab = "RMSE", ylab = "Lambda") # lambda vs RMSE

LambdaOpt <- ValuesOfLambda[which.min(RmseFunctionReg)]
LambdaOpt # Lambda which minimizes RMSE

TableOfRmse <- rbind(TableOfRmse,
                     data_frame(Method = "User & Movie Effect Regularisation",
                                RMSE = min(RmseFunctionReg)))
TableOfRmse %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#The regularisation give as a higher RMSE than the first "User & Movie Effect" model. This is unexpected


#-----------------------------------------
##3-6 Getting Regularisation on validation data set
#let's see what is the result and performance of the regularisation on the validation data .

FuncRmseRegValue <- sapply(ValuesOfLambda, function(l){
  
  MU <- mean(train$rating)
  
  MI <- train %>%
    group_by(movieId) %>%
    summarize(MI = sum(rating - MU)/(n()+l))
  
  UI <- train %>%
    left_join(MI, by="movieId") %>%
    group_by(userId) %>%
    summarize(UI = sum(rating - MI - MU)/(n()+l))
  
  PredictedRegValue <- validation %>%
    left_join(MI, by = "movieId") %>% 
    left_join(UI, by = "userId") %>%
    mutate(pred = MU + MI + UI) %>% .$pred
  
  return(RMSE(PredictedRegValue, validation$rating, na.rm = T))
})

qplot(ValuesOfLambda, FuncRmseRegValue,
      main = "Regularisation on validation data set",
      xlab = "Lambda", ylab = "RMSE")

LambdaRegOpt <- ValuesOfLambda[which.min(FuncRmseRegValue)]
LambdaRegOpt # Lambda which minimizes RMSE
RmseMin <- min(FuncRmseRegValue) # Best RMSE
RmseMin

RmseTableVal <- rbind(RmseTableVal,
                      data_frame(Method = "User & Movie Effect Reg. on validation",
                                 RMSE = min(FuncRmseRegValue)))
RmseTableVal %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)
#-----------------------------------------
#4- Results

rbind(TableOfRmse, RmseTableVal) %>% knitr::kable(caption = "RMSEs Summary")

#We can observe that the better RMSE is obtained from the User & Movie Effect model. However, this RMSE only obtained on the test set. When we move to the validation data set, we obtain the worse RMSE (ignoring the baseline).




