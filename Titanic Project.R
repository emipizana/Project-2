library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(dtplyr)
library(tidyr)
library(MASS)
library(randomForest)
library(purrr)
library(caret)

#Read Data
train_df <- read_csv("train_titanic.csv")
test_df <- read_csv("test_titanic.csv")


#Data info training data
summary(train_df)
str(train_df)
head(train_df)


ggplot(train_df, aes(x=factor(Survived)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() +  labs(title = "Survived",
                          x = "Survivors", y = "Frequency (Count)")

#Data info test data
summary(test_df)
str(test_df)
head(test_df)


#Cleaning Data
missing_values <- train_df  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing_data") %>%
  count(variables, missing_data) 

ggplot(missing_values, aes(y = variables, x = n, fill = missing_data)) + 
  geom_col() +  labs(title = "Train Data")


null_cols <- which(colSums(is.na(train_df))>0)

no_missingval <- sum(is.na(train_df$Age))

train_df$Age <- replace_na(train_df$Age, mean(train_df$Age, na.rm = TRUE))
train_df$Age <- round(train_df$Age)


for(i in 1:length(train_df$Cabin)){
  if(is.na(train_df$Cabin[i])){
    train_df$Cabin[i] <- "No cabin recorded"
  }
}


sprintf("We have left %1.0f missing values" ,sum(is.na(train_df)))

#Checking test data
missing_values_test <- test_df  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing_data") %>%
  count(variables, missing_data) 

ggplot(missing_values_test, aes(y = variables, x = n, fill = missing_data)) + 
  geom_col() +  labs(title = "Test Data")

null_cols_test <- which(colSums(is.na(test_df))>0)

no_missingval_test <- sum(is.na(test_df$Age))

test_df$Age <- replace_na(test_df$Age, mean(test_df$Age, na.rm = TRUE))
test_df$Age <- round(test_df$Age)


for(i in 1:length(test_df$Cabin)){
  if(is.na(test_df$Cabin[i])){
    test_df$Cabin[i] <- "No cabin recorded"
  }
}


sprintf("We have left %1.0f missing values" ,sum(is.na(test_df)))
sum(is.na(test_df$Fare))

test_df$Fare <- replace_na(test_df$Fare, mean(test_df$Fare, na.rm = TRUE))
sprintf("We have left %1.0f missing values" ,sum(is.na(test_df)))

#Name Variable train data

train_df <- train_df %>%
  mutate(Title_1 = str_split(train_df$Name, ","))

vec <- c()
for (i in 1:length(train_df$Title_1)){
  vec <- rbind(vec, train_df$Title_1[[i]][2])
}

vec_1 <- c()
for (i in 1:length(vec)){
  vec_1 <- rbind(vec_1, word(vec[i], start = 1, end = 2, sep = fixed(" ")))
}

train_df <- train_df %>%
  mutate(Title_name = vec_1)
  

Title_freq_graph <- ggplot(train_df, aes(x = Title_name)) + geom_bar()
Title_freq_graph

Title_freq <- train_df %>% 
  count(Title_name)
Title_freq

for(i in 1:length(train_df$Title_name)){
  if(train_df$Title_name[i] %in% c(" Mr.", " Mrs.", " Miss.", " Mrs.", " Master."))
    train_df$Title_name[i] <- str_replace_all(train_df$Title_name[i]," ","")
  else
    train_df$Title_name[i] <- "Other"
}

Title_freq_graph <- ggplot(train_df, aes(x = Title_name)) + geom_bar()
Title_freq_graph

Title_freq <- train_df %>% 
  count(Title_name)
Title_freq

#Name Variable test data
test_df <- test_df %>%
  mutate(Splitname = str_split(test_df$Name, ","))

vectest <- c()
for (i in 1:length(test_df$Splitname)){
  vectest <- rbind(vectest, test_df$Splitname[[i]][2])
}

vectest_1 <- c()
for (i in 1:length(vectest)){
  vectest_1 <- rbind(vectest_1, word(vectest[i], start = 1, end = 2, sep = fixed(" ")))
}

test_df <- test_df %>%
  mutate(Title_name = vectest_1)


Title_freq_graph_test <- ggplot(test_df, aes(x = Title_name)) + geom_bar()
Title_freq_graph_test

Title_freq_test <- test_df %>% 
  count(Title_name)
Title_freq_test

for(i in 1:length(test_df$Title_name)){
  if(test_df$Title_name[i] %in% c(" Mr.", " Mrs.", " Miss.", " Mrs.", " Master."))
    test_df$Title_name[i] <- str_replace_all(test_df$Title_name[i]," ","")
  else
    test_df$Title_name[i] <- "Other"
}

Title_freq_graph_test <- ggplot(test_df, aes(x = Title_name)) + geom_bar() +  
  labs(title = "Test Data")
Title_freq_graph

Title_freq_test <- test_df %>% 
  count(Title_name)
Title_freq_test


#Ticket variable train data

for(i in 1:length(train_df$Ticket)){
  if(grepl(" ", train_df$Ticket[i])){
    train_df$Ticket[i] <- word(train_df$Ticket[i], start = 1, end = 1, sep = fixed(" "))
  }
  else
    train_df$Ticket[i] <- "Normal Ticket"
}

Ticket_freq_graph <- ggplot(train_df, aes(x = Ticket)) + geom_bar()
Ticket_freq_graph
train_df %>% 
  count(Ticket) %>%
  arrange(desc(n))

#Ticket variable test data
for(i in 1:length(test_df$Ticket)){
  if(grepl(" ", test_df$Ticket[i])){
    test_df$Ticket[i] <- word(test_df$Ticket[i], start = 1, end = 1, sep = fixed(" "))
  }
  else
    test_df$Ticket[i] <- "Normal Ticket"
}
Ticket_freq_graph_test <- ggplot(test_df, aes(x = Ticket)) + geom_bar()
Ticket_freq_graph_test
test_df %>% 
  count(Ticket) %>%
  arrange(desc(n))

#Embarked train variable

train_df <- train_df %>% 
  mutate(Embarked = replace(Embarked, Embarked == "", NA)) %>% 
  na.omit()



#Variables Dummification train data

cat_variables <- c("Survived", "Pclass", "Sex", "SibSp", "Embarked",
                   "Title_name")

train_df[cat_variables] <- lapply(train_df[cat_variables], factor)
train_df[cat_variables] %>% head()

str(train_df[cat_variables])

#Variables Dummification test data

cat_variables_test <- c("Pclass", "Sex", "SibSp", "Embarked",
                   "Title_name")

test_df[cat_variables_test] <- lapply(test_df[cat_variables_test], factor)
test_df[cat_variables_test] %>% head()

str(test_df[cat_variables_test])

#Fare Variable (numerical variable) train data

fare_graph <- ggplot(train_df, aes(x = Fare)) + geom_boxplot(outlier.colour="red", outlier.shape=16,
                                              outlier.size=2)


train_df <- train_df %>%
  filter(Fare < 400)

ggplot(train_df, aes(x = Fare)) + geom_boxplot(outlier.colour="red", outlier.shape=16,
                                               outlier.size=2)

train_df$Fare <- scale(train_df$Fare, center = TRUE, scale=TRUE)
head(train_df$Fare)

#Fare Variable test data
test_df$Fare <- scale(test_df$Fare, center = TRUE, scale=TRUE)


#Age variable (numerical)

age_graph <- ggplot(train_df, aes(Age)) + geom_boxplot(outlier.colour="red", outlier.shape=16,
                                                       outlier.size=2)
age_graph
min(train_df$Age)
train_df <- train_df %>% 
  filter(Age > 0)
train_df$Age <- scale(train_df$Age, center = TRUE, scale = TRUE)

#Age variable Test data
test_df$Age <- scale(test_df$Age, center = TRUE, scale = TRUE)

#Parch variable train df(numerical)
train_df$Parch <- scale(train_df$Parch, center = TRUE, scale = TRUE)

#Parch variable test df(numerical)
test_df$Parch <- scale(test_df$Parch, center = TRUE, scale = TRUE)

#Logistical Regresion train data
fmla <- as.formula(Survived ~ Pclass + Sex + SibSp + Parch + Embarked + Title_name
                   + Age + Fare)
log_model <- glm(fmla, train_df, family = "binomial")
summary(log_model)

log_model_prob <- predict(log_model, train_df, type = "response")
head(log_model_prob)
log_model_pred <- as.factor(round(log_model_prob))
head(log_model_pred)


#Table
log_model_table <- caret::confusionMatrix(log_model_pred, train_df$Survived)
log_model_table


#Random Forest train data
randomforest_model <- randomForest(fmla, train_df)
randomforest_pred <- predict(randomforest_model,train_df)
head(randomforest_pred)

#Table
randomforest_table <- caret::confusionMatrix(randomforest_pred, train_df$Survived)
randomforest_table

plot(randomforest_model)

#Prediction, Random Forest

test_prediction <- predict(randomforest_model, test_df)

final_prediction <- test_df %>% 
  dplyr::select(PassengerId) %>% 
  mutate(Survived = test_prediction)

head(final_prediction)

write.csv(final_prediction, "Titanic Prediction", row.names = FALSE)

