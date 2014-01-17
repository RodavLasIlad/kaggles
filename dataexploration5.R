# A first real (documented) attempt to apply random forests to the Titanic training set (The code is built from scratch, minus the randomForests function
# The following are the variable names and a quick summary of what each one is
# Survived: a binary variable describing whether someone lives or dies (0 or 1). This is the variable I am solving for in the test set.
# Pclass: Passenger class, either 1, 2, or 3, 1 being the "upper class"
# Name: Name of the passenger
# Sex: gender, either male or female
# Age: the age, in years, of the passenger (several NA values exist here)
# SibSp: Number of siblings/Spouses onboard
# ParCh: Number of parents/children onboard
# Ticket: A variable containing letters and numbers
# Fare: the cost of the ticket, from $0 to $512.33
# Cabin: the Cabin the passenger was in, A-G + T
# Embarked: Port of embarkation  (C = Cherbourg; Q = Queenstown; S = Southampton)

#First I set my working directory
#setwd("~/Media/Dropbox/Programming/kaggle/titanic/csv/")

#Load in my CSVs
original.train <- read.csv("train.csv")
original.test <- read.csv("test.csv")

#Set the survived status for the test group to 3, this creates the column for later, and allows me to easily pull it out after I've munged the data
original.test$Survived <- 3

# I combine the training and test sets into one variable for munging
data <- rbind(original.test, original.train)

# Some of the variables are pretty clean, and only need to be set as factors
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)

# Taking just the first letter of those that are available (disregarding the number)
data$Cabin <- as.factor(substring(data$Cabin, 1, 1))

# Using fares to guess at missing cabins (after binning them to find out the mean for each group)
for (i in 1:length(data$Cabin)) {
	if (data$Cabin[i] == "") {
# To fix the one na value that was messing me up, I determined it's likely class 'G'
		if (is.na(data$Fare[i])) {data$Cabin[i] <- "G"}
		else if (data$Fare[i] < 7.79) {data$Cabin[i] <- "G"}
		else if (data$Fare[i] < 8.68) {data$Cabin[i] <- "F"}
		else if (data$Fare[i] < 14.46) {data$Cabin[i] <- "E"}
		else if (data$Fare[i] < 26.25) {data$Cabin[i] <- "D"}
		else if (data$Fare[i] < 53.10) {data$Cabin[i] <- "C"}
		else {data$Cabin[i] <- "B"}
	}
}

# Cleaning up embarked, which has a lot of missing values
data$Embarked <- as.factor(data$Embarked)
# If there is no value, then it is set to S, which is the most common onloading point
for (i in 1:length(data$Embarked)) {
	if (data$Embarked[i] == "") {
		data$Embarked[i] <- "S"
	}
}

# I had used cut2 to determine equal size groups, then found the average of each of those buckets, and pushed any empty values into that average age (judging by how much their ticket cost, which seemed to be a reliable estimate of age)
data$Survived <- as.factor(data$Survived)
for (i in 1:length(data$Age)) { 
	if (is.na(data$Age[i])) {
		if (data$Pclass[i] == 1) {
			data$Age[i] <- 11.96
		}
		else if (data$Pclass[i] == 2) {
			data$Age[i] <- 29
		}
		else {
			data$Age[i] <- 57.25
		}
	}
}

# Counting family together, and making it so that3+ family members are all binned together
family <- data$Parch + data$SibSp
for (i in 1:length(family)) {
	if (family[i] >= 3) {
		family[i] = 3
	}
}


# ----------------------------------------------------------------------
# Now that the munging is finished I can seperate the groups again
test <- droplevels(subset(data, data$Survived == 3))
train <- droplevels(subset(data, data$Survived != 3))

## bootstrapping?
# library(boot)
## function to obtain regression weights
# bs <- function(formula, data, indices) {
#  d <- data[indices,] # allows boot to select sample
#  fit <- lm(formula, data=d)
#  return(coef(fit))
# } 
# results <- boot(data=data, statistic=bs, R=1000, formula=Survived ~ Pclass + Sex + Age + Fare + Embarked + family)


# Making the forests
library(randomForest)
first.forest <- randomForest(Survived ~ Pclass + Sex + Age + Fare + Embarked + family, data=train, prox=T)

# Predicting based on the model
vector.of.answers <- predict(first.forest, test)
vector.of.answers[is.na(vector.of.answers)] <- 0

# Writing it all, putting it in the correct order to submit, and then saving the output
original.test$Survived <- vector.of.answers
original.test <- original.test[c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked")]
write.table(original.test[c(1,2)], file="submission2.csv", row.names=FALSE, col.names=TRUE, sep=",")

