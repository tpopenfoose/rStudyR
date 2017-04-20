# ## 2016-08-03
# 0. 
rm(list = ls())
# 1. https://www.kaggle.com/c/titanic/data
# 2. download 'train.csv'
# 3. reading file
train.data <- read.csv('./Chapter02/train.csv', na.strings = c('NA', ''))
str(train.data)
# 4. converting types
train.data <- within(train.data, {
  Survived <- factor(Survived)
  Pclass <- factor(Pclass)
})
# 5. detecting missing values
is.na(train.data$Age)
sum(is.na(train.data$Age))
sum(is.na(train.data$Age)) / length(train.data$Age)

sapply(train.data, function(df) {
  sum(is.na(df)) / length(df)
})
# 6. install.packages('Amelia')
require(Amelia)
missmap(train.data, main = 'Missing Map')
# AmeliaView()
# 7. imputing missing values
table(train.data$Embarked, useNA = 'always')
train.data$Embarked[which(is.na(train.data$Embarked))] <- 'S'
table(train.data$Embarked, useNA = "always")

table_words <- table(unlist(strsplit(as.character(train.data$Name), "\\s+")))
sort(table_words[grep('\\.', names(table_words))], decreasing=TRUE)

require(stringr)
tb <- cbind(train.data$Age, str_match(train.data$Name, '[a-zA-Z]+\\.'))
table(tb[is.na(tb[, 1]), 2]) # title for unknow ages

mean.mr <- mean(train.data$Age[grepl(" Mr\\.", train.data$Name) & !is.na(train.data$Age)]) # knew ages mean dead ages
mean.mrs <- mean(train.data$Age[grepl(" Mrs\\.", train.data$Name) & !is.na(train.data$Age)])
mean.dr <- mean(train.data$Age[grepl(" Dr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.miss <- mean(train.data$Age[grepl(" Miss\\.", train.data$Name) & !is.na(train.data$Age)])
mean.master <- mean(train.data$Age[grepl(" Master\\.", train.data$Name) & !is.na(train.data$Age)])

train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.mr # assign mean values to unknow ages
train.data$Age[grepl(" Mrs\\.", train.data$Name) & is.na(train.data$Age)] <- mean.mrs
train.data$Age[grepl(" Dr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.dr
train.data$Age[grepl(" Miss\\.", train.data$Name) & is.na(train.data$Age)] <- mean.miss
train.data$Age[grepl(" Master\\.", train.data$Name) & is.na(train.data$Age)] <- mean.master
# 8. exploring and visualizing data
barplot(table(train.data$Survived), main="Passenger Survival", names= c("Perished", "Survived"))
barplot(table(train.data$Pclass), main="Passenger Class", names= c("first", "second", "third"))
barplot(table(train.data$Sex), main="Passenger Gender")
hist(train.data$Age, main="Passenger Age", xlab = "Age")
barplot(table(train.data$SibSp), main="Passenger Siblings")
barplot(table(train.data$Parch), main="Passenger Parch")
hist(train.data$Fare, main="Passenger Fare", xlab = "Fare")
barplot(table(train.data$Embarked), main="Port of Embarkation")

counts <- table(train.data$Survived, train.data$Sex)
barplot(counts, col=c("darkblue","red"), legend = c("Perished", "Survived"), main = "Passenger Survival by Sex")

counts <- table( train.data$Survived, train.data$Pclass)
barplot(counts, col=c("darkblue","red"), legend =c("Perished", "Survived"), main= "Titanic Class Bar Plot" )

counts <- table( train.data$Sex, train.data$Pclass)
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main= "Passenger Gender by Class")

hist(train.data$Age[which(train.data$Survived == "0")], main= "Passenger Age Histogram", xlab="Age", ylab="Count", col ="blue",
     breaks=seq(0,80,by=2))
hist(train.data$Age[which(train.data$Survived == "1")], col ="red", add = T, breaks=seq(0,80,by=2))

boxplot(train.data$Age ~ train.data$Survived, main="Passenger Survival by Age", xlab="Survived", ylab="Age")






