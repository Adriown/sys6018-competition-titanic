# Loading in libraries
library(tidyverse)
library(ggplot2)

# Global variable
tolerance <- .5

# Loading in datasets
submission_file_ex <- read_csv('gender_submission.csv')
titanic.train <- read_csv('train.csv')
titanic.test <- read_csv('test.csv')

# Now exploring the data somewhat
summary(titanic.train)
glimpse(titanic.train)

# Going to make a column or two into factors
# Make the Ticket Class a factor as this info is not a quantitative variable
titanic.train$Pclass <- factor(titanic.train$Pclass)
titanic.test$Pclass <- factor(titanic.test$Pclass)

# Let's subset to cross-validate
set.seed(20)
sub <- sample(1:NROW(titanic.train),size=round(.7 * NROW(titanic.train))) # Subset 70% of the training data we have to be in our training set
titanic.train.train <- titanic.train[sub,]     # Select subset for cross-validation
titanic.train.test <- titanic.train[-sub,]

titanic.train.train.use.this  <- titanic.train.train[c(2,3,5,7,8,10)] # I needed to only pass some columns to the glm function as I was losing rows with NAs 
# This means that we are only planning on using Pclass, Sex, SibSp, Parch, and Fare to predice the Survival

# We are going to use 'glm' because this is a logistic regression to predict survival
# titanic.lg <- glm(Survived~.-PassengerId-Name-Ticket-Cabin-Age, data=titanic.train.train, family = "binomial")
titanic.train.train.lg <- glm(Survived~., data=titanic.train.train.use.this, family = "binomial")
summary(titanic.train.train.lg)
# It appears that Pclass and Sex are most likely to have a non-trivial relationship with Survival
anova(titanic.train.train.lg, test = "Chisq")

# Let's test the full model on the training set
# to see how we do predicting.
probs <- as.vector(predict(titanic.train.train.lg, type="response"))
preds <- rep(0,length(probs))  # Initialize prediction vector
preds[probs>tolerance] <- 1 # p>0.5 -> 1
train_results <- table(preds,titanic.train.train$Survived)
(train_results[1]+train_results[4])/sum(train_results)  # Proportion of predictions that are correct
# 79% successful prediction rate

# Let's try the full model on the testing set
probs<-as.vector(predict(titanic.train.train.lg,newdata=titanic.train.test, type="response"))
preds <- rep(0,length(probs))  # Initialize prediction vector
preds[probs>tolerance] <- 1 # p>0.5 -> 1
test_results <- table(preds,titanic.train.test$Survived)
(test_results[1]+test_results[4])/sum(test_results)  # Proportion of predictions that are correct
# 82% successful prediction rate

# It doesn't seem like there's any significant issues with overfitting going on. We're getting pretty good accuracy
# I'm going to run the model on the full set of training data and look again
# Cut out certain columns again
titanic.train.use.this  <- titanic.train[c(2,3,5,7,8,10)] # I needed to only pass some columns to the glm function as I was losing rows with NAs 
titanic.train.lg <- glm(Survived~., data=titanic.train.use.this, family = "binomial")
summary(titanic.train.lg)

# Let's test the full model on the training set
# to see how we do predicting.
probs <- as.vector(predict(titanic.train.lg, type="response"))
preds <- rep(0,length(probs))  # Initialize prediction vector
preds[probs>tolerance] <- 1 # p>0.5 -> 1
train_results <- table(preds,titanic.train$Survived)
(train_results[1]+train_results[4])/sum(train_results)  # Proportion of predictions that are correct
# 80% successful prediction rate

# Now make predictions on the Test data
probs<-as.vector(predict(titanic.train.lg,newdata=titanic.test, type="response"))
preds <- rep(0,length(probs))  # Initialize prediction vector
preds[probs>tolerance] <- 1 # p>0.5 -> 1
test_results <- titanic.test[1]
test_results$Survived <- as.integer(preds)

# And write this out to a csv which we can submit
write_csv(test_results, "titanic_submission.csv")
