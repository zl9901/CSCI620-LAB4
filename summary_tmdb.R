require(dplyr)
require(readr)
require(lubridate)
require(stringr)
require(tidyr)
require(sqldf)

# change file path here
setwd("~/Downloads/tmdb-movie-metadata")

##################################analyze data set######################################
# tmdb summary 
tmdb_5000_movies <- read.csv("tmdb_5000_movies.csv")
moviedata<-tmdb_5000_movies
summary(moviedata)
Quantitivedata<-sqldf("select budget, homepage, id, original_language, original_title, overview, popularity, production_companies, production_countries, release_date, revenue, runtime, spoken_languages, status, vote_average, vote_count from moviedata")
Textualdata<-sqldf("select homepage, original_title, overview, tagline from moviedata")

average_rating <- moviedata$vote_average
budget <- moviedata$budget
genres <- moviedata$genres
original_language <- moviedata$original_language
popularity <- moviedata$popularity
revenue <-  moviedata$revenue
runtime <- moviedata$runtime
vote_count <- moviedata$vote_count
pairs(~ average_rating + budget + genres + original_language + popularity + revenue + runtime + vote_count)


library(plyr)             #For Data transformation
library(tidyverse)        #For data cleaning
library(jsonlite)         #For manipulating JSON data
library(wordcloud)        #For generating Word Cloud
library(RColorBrewer)     #For further formatting
library(ggplot2)          #Extension of ggplot2
library(tm)               #For text mining
library(zoo)              #For handling irregular time series of numeric vectors/matrices and factors

movie = read_csv("tmdb_5000_movies.csv",col_names = TRUE, na = "NA")
credits = read_csv("tmdb_5000_credits.csv",col_names = TRUE,na = "NA")

glimpse(movie)
glimpse(credits)

#data cleaning

# movie
## removing spurious characters
movie$title <- (sapply(movie$title,gsub,pattern = "\\Â",replacement = ""))

## deleting duplicate rows
movie <- movie[!duplicated(movie$title), ]
dim(movie)

## transformation of "keywords" column into tibble
keywords <- movie %>%    
  filter(nchar(keywords) > 2) %>%                 # fiter out blank keywords field
  mutate(                                         # create a new field 
    js = lapply(keywords, fromJSON)               # containing a LIST of keyword and value pairs
  ) %>%                                           # called id and name
  unnest(js,.names_repair = "check_unique") %>%   # turn each keyword/value pairs in the LIST into a row
  select(id, title, keywords = name)

## Combining the keywords of a movie in a single column
keywords <- aggregate(keywords ~.,data = keywords, paste, collapse = ",")

#Combining the genres of a movie in a single column
genres <- movie %>% filter(nchar(genres) > 2) %>%                   
  mutate( js = lapply(genres, fromJSON)) %>%                                           
  unnest(js,.names_repair = "check_unique") %>%                                  
  select(id, title, genres = name) 

genres <- aggregate(genres ~.,data = genres, paste, collapse = ",")

# Combining production_companies
production_companies <- movie %>% filter(nchar(production_companies) > 2) %>%                   
  mutate( js = lapply(production_companies, fromJSON)) %>%                                           
  unnest(js,.names_repair = "check_unique") %>%                                  
  select(id, title, production_companies = name) 

production_companies <- aggregate(production_companies ~.,data = production_companies, paste, collapse = ",")

# Combining production countries
production_countries <- movie %>%    
  filter(nchar(production_countries) > 2) %>%     
  mutate(                                         
    js = lapply(production_countries, fromJSON)   
  ) %>%                                          
  unnest(js) %>%                                  
  select(id, title, production_countries = name)

countries <- movie %>%    
  filter(nchar(production_countries) > 2) %>%     
  mutate(                                         
    js = lapply(production_countries, fromJSON)   
  ) %>%                                          
  unnest(js,.names_repair = "check_unique") %>%                                  
  select(id, title, production_countries = name)

production_countries <- aggregate(production_countries ~.,data = production_countries, paste, collapse = ",")

# combining spoken languages
spoken_languages <- movie %>%    
  filter(nchar(spoken_languages) > 2) %>%        
  mutate(                                         
    js = lapply(spoken_languages, fromJSON)      
  ) %>%                                          
  unnest(js,.names_repair = "check_unique") %>%                                 
  select(id, title, spoken_languages = iso_639_1) 

spoken_languages <- aggregate(spoken_languages ~.,data = spoken_languages, paste, collapse = ",")

movies <- subset(movie, select = -c(genres, keywords, production_companies, production_countries,spoken_languages))
glimpse(movies)

# Dropped existing unformatted columns in the main dataset, creating a new dataset "movies"
movies <- subset(movie, select = -c(genres, keywords, production_companies, production_countries, spoken_languages))


movies <- movies %>%
  full_join(keywords, by = c("id", "title")) %>%
  full_join(genres, by = c("id", "title")) %>%
  full_join(production_companies, by = c("id", "title")) %>%
  full_join(production_countries, by = c("id", "title")) %>%
  full_join(spoken_languages, by = c("id", "title"))

glimpse(movies)

# credit
all_crew <- credits %>%      # start with the raw tibble 
  filter(nchar(crew) > 2) %>%        # filter out movies with empty crew  
  mutate(                                 
    js  =  lapply(crew, fromJSON)  # turn the JSON into a list
  )  %>%                           #
  unnest(js) 

all_cast <- credits %>%      # start with the raw tibble 
  filter(nchar(cast) > 2) %>%        # filter out movies with empty crew  
  mutate(                          #       
    js  =  lapply(cast, fromJSON)  # turn the JSON into a list
  )  %>%                           #
  unnest(js) 
cast <- subset(all_cast, select = -c(movie_id, title, cast, crew))
crew <- subset(all_cast, select = -c(movie_id, title, cast, crew))

library(DT)
datatable(head(movies,30))
datatable(head(cast, 10))
datatable(head(crew, 10))

#analysis by average vote
#ggplot(movies,aes(vote_average)) +
# geom_histogram(bins = 100) +
#geom_vline(xintercept = mean(movie$vote_average,na.rm = TRUE),colour = "red") + 
#ylab("Count of Movies") + 
#  xlab("Average Vote") + 
# ggtitle("Histogram for average vote rating")

movies %>% select(title,vote_average,vote_count, budget) %>% 
  filter(vote_count > 500 ) %>% arrange(desc(vote_average)) %>% head(20) %>%
  ggplot(aes(x = title,y = vote_average,fill = budget )) + geom_bar(stat = "identity") + coord_flip(ylim = c(7, 9)) +
  scale_fill_continuous()

movies %>% select(title,vote_average,vote_count, popularity) %>% 
  filter(vote_count > 300 ) %>%  head(30) %>%
  ggplot(aes(x = title,y = popularity, fill = vote_count)) + geom_bar(stat = "identity") + coord_flip() +
  scale_fill_continuous()

##################################extract features from data set######################################
actor_experience<-dplyr::summarise(group_by(all_cast,id,name),experience_count=n())
producer_experience<-dplyr::summarise(group_by(all_crew,id,name,job),experience_count=n()) %>% filter(job == "Producer" )
director_experience<-dplyr::summarise(group_by(all_crew,id,name,job),experience_count=n()) %>% filter(job == "Director" )

tmp <- subset(merge(all_cast,actor_experience,by=c("id","name")),
              select = c(movie_id, title, id, name,experience_count))
tmp <- tmp[!duplicated(tmp),]
tmp0<-dplyr::summarise(group_by(tmp,movie_id,title),act_experience_mean=mean(experience_count))

# top 3 stuff
max2 = function(x){
  t = which.max(x$experience_count)
  data = x[-t,]
  actor_max2= max(data$experience_count)
  return(data.frame(actor_max2))
}

max3 = function(x){
  t = which.max(x$experience_count)
  data = x[-t,]
  t = which.max(data$experience_count)
  data = data[-t,]
  actor_max3= max(data$experience_count)
  return(data.frame(actor_max3))
}

act_greatest1 <- dplyr::summarise(group_by(tmp,movie_id,title),actor_max1=max(experience_count))
act_greatest2 <- tmp %>% group_by (movie_id,title) %>% do(max2(.))
act_greatest3 <- tmp %>% group_by (movie_id,title) %>% do(max3(.))

tmp <- subset(merge(all_crew,producer_experience,by=c("id","name")),
              select = c(movie_id, title, id, name,experience_count))
tmp <- tmp[!duplicated(tmp),]
tmp1<-dplyr::summarise(group_by(tmp,movie_id,title),pro_experience_mean=mean(experience_count))

max2 = function(x){
  t = which.max(x$experience_count)
  data = x[-t,]
  producer_max2= max(data$experience_count)
  return(data.frame(producer_max2))
}

pro_greatest1 <- dplyr::summarise(group_by(tmp,movie_id,title),producer_max1=max(experience_count))
pro_greatest2 <- tmp %>% group_by (movie_id,title) %>% do(max2(.))

tmp <- subset(merge(all_crew,director_experience,by=c("id","name")),
              select = c(movie_id, title, id, name,experience_count))
tmp <- tmp[!duplicated(tmp),]
tmp2<-dplyr::summarise(group_by(tmp,movie_id,title),dir_experience_mean=mean(experience_count))

max2 = function(x){
  t = which.max(x$experience_count)
  data = x[-t,]
  director_max2= max(data$experience_count)
  return(data.frame(director_max2))
}

dir_greatest1 <- dplyr::summarise(group_by(tmp,movie_id,title),directormax1=max(experience_count))
dir_greatest2 <- tmp %>% group_by (movie_id,title) %>% do(max2(.))

#merge all the feature and keep missing value as NA
experience_feature<- merge(tmp2,tmp1,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,tmp0,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,act_greatest1,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,act_greatest2,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,act_greatest3,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,pro_greatest1,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,pro_greatest2,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,dir_greatest1,by=c("movie_id","title"), all = TRUE)
experience_feature<- merge(experience_feature,dir_greatest2,by=c("movie_id","title"), all = TRUE)

experience_feature[is.na(experience_feature)]<- 0

remove(act_greatest1)
remove(act_greatest2)
remove(act_greatest3)
remove(pro_greatest1)
remove(pro_greatest2)
remove(dir_greatest1)
remove(dir_greatest2)
remove(tmp)
remove(tmp0)
remove(tmp1)
remove(tmp2)

# extract localization feature from data set
library(stringr)
tmp <- subset(movies, select = c(id,original_title,spoken_languages))
tmp <- transform(tmp, language_number= str_count(tmp$spoken_languages,",")+1) 
localization_feature <- subset(tmp, select = -c(spoken_languages))
remove(tmp)

#############################prepare train data###################################
label <- subset(movies,select = c(id,original_title,vote_average))
# set rateing interval
label$vote_average <- ceiling(label$vote_average)
raw_set <- subset(movies,select = c(id,original_title,runtime, budget,revenue,popularity))
names(experience_feature)[2] = "original_title"
names(experience_feature)[1] = "id"
raw_set <- merge(raw_set,experience_feature,by=c("id","original_title"))
raw_set <- merge(raw_set,localization_feature,by=c("id","original_title"))
raw_set <- merge(raw_set,label,by=c("id","original_title"))
raw_set <- subset(raw_set, select = -c(actor_max1,actor_max2,actor_max3,
                                       producer_max1,producer_max2,directormax1,director_max2))
raw_set <- na.omit(raw_set)
decision_raw_set <- raw_set 

#############################data discretization###################################
library(infotheo)
runtime <- discretize(raw_set$runtime,"equalfreq",10)
plot(raw_set$runtime, col = runtime$X)
names(runtime)[1] <- "runtime_class"
raw_set <- cbind(raw_set,runtime)

raw_set <- subset(raw_set,select = -c(runtime))
raw_set$runtime_class <- as.factor(raw_set$runtime_class )

budget <- discretize(raw_set$budget,"equalfreq",10)
plot(raw_set$budget, col = budget$budget_class)
names(budget)[1] <- "budget_class"
raw_set <- cbind(raw_set,budget)

raw_set <- subset(raw_set,select = -c(budget))
raw_set$budget_class <- as.factor(raw_set$budget_class )

revenue <- discretize(raw_set$revenue,"equalfreq",10)
plot(raw_set$revenue, col = revenue$revenue_class)
names(revenue)[1] <- "revenue_class"
raw_set <- cbind(raw_set,revenue)

raw_set <- subset(raw_set,select = -c(revenue))
raw_set$revenue_class <- as.factor(raw_set$revenue_class )

popularity <- discretize(raw_set$popularity,"equalfreq",10)
plot(raw_set$popularity, col = popularity$popularity_class)
names(popularity)[1] <- "popularity_class"
raw_set <- cbind(raw_set,popularity)

raw_set <- subset(raw_set,select = -c(popularity))
raw_set$popularity_class <- as.factor(raw_set$popularity_class )

for (i in c(1:nrow(raw_set))){
  if (raw_set[i,]$vote_average <= 5 && raw_set[i,]$vote_average >= 0) raw_set[i,]$vote_average <- 1 
  if (raw_set[i,]$vote_average <= 10 && raw_set[i,]$vote_average > 5) raw_set[i,]$vote_average <- 2
}

for (i in c(1:nrow(decision_raw_set))){
  if (decision_raw_set[i,]$vote_average <= 5 && decision_raw_set[i,]$vote_average >= 0) decision_raw_set[i,]$vote_average <- 1 
  if (decision_raw_set[i,]$vote_average <= 10 && decision_raw_set[i,]$vote_average > 5) decision_raw_set[i,]$vote_average <- 2
}

#############################generate train set and validation set###################################
require(plyr)
require(e1071)
# cross validation
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #split data into k groups
  temp <- sample(n,datasize)   # 
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])  # generate k order sequence from dataseq
  return(cvlist)
}

#############################training and testing Bayes Classifier###################################
k <- 10
datasize <- nrow(raw_set)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 666)
validation <- raw_set[cvlist[[1]],]
validation <- subset(validation,select = -c(id,original_title))
validation$vote_average <- as.factor(validation$vote_average)
train <- raw_set[-cvlist[[1]],]
train <- subset(train,select = -c(id,original_title))
train$vote_average <- as.factor(train$vote_average)
model <- naiveBayes(vote_average~.,data = train)
prediction <- predict(model,validation)
actual <- validation$vote_average
confusion_matrix <- table(prediction,actual)
NB_accuracy = sum(confusion_matrix[row(confusion_matrix)==col(confusion_matrix)]) / sum(confusion_matrix)
NB_accuracy
f1_fun = function(pre,y){
  class = sort(unique(y))
  tp=NA
  fp=NA
  fn=NA
  tn=NA
  for(i in 1:length(class)){
    tp[i] = sum(pre==class[i] & y==class[i])
    tn[i] = sum(pre!=class[i] & y!=class[i])
    fp[i] = sum(pre==class[i] & y!=class[i])
    fn[i] = sum(pre!=class[i] & y==class[i])
  }
  precision = tp/(tp + fp)
  recall = tp/(tp + fn)
  f1 = (2*precision*recall)/(precision+recall)
  
  names(f1) = class
  names(precision) = class
  names(recall) = class
  
  print(table(pre,y))
  print('-------------precision--------------------')
  print(precision)
  print('-------------recall--------------------')
  print(recall)
  print('-------------f1--------------------')
  print(f1)
  print('--------------mean(f1)-------------------')
  print(mean(f1))
}

f1_fun(actual,prediction)

#############################training and testing Decision Tree Classifier###################################
library("rpart")
library("rpart.plot")
library(party)
k <- 10
datasize <- nrow(decision_raw_set)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 666)
validation <- decision_raw_set[cvlist[[1]],]
validation <- subset(validation,select = -c(id,original_title))
validation$vote_average <- as.factor(validation$vote_average)
train <- decision_raw_set[-cvlist[[1]],]
train <- subset(train,select = -c(id,original_title))
train$vote_average <- as.factor(train$vote_average)

control<-ctree_control(maxdepth=8)
output.tree <- ctree(vote_average ~ runtime + budget + revenue + popularity + language_number + dir_experience_mean + pro_experience_mean + act_experience_mean, train)

plot(output.tree)
predicted <- predict((output.tree),validation)
predicted
validation$vote_average
table(predicted,validation$vote_average)
actual <- validation$vote_average

confusion_matrix <- table(predicted,actual)
DT_accuracy = sum(confusion_matrix[row(confusion_matrix)==col(confusion_matrix)]) / sum(confusion_matrix)
DT_accuracy
