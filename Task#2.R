#importing data
data<- read.csv("Hours and Scores.csv")
View(data)
summary(data)

#Splitting the dataset into traing and testing set
library(caTools)
set.seed(123)
split<- sample.split(data$Scores, SplitRatio = 0.7)
training_set<- subset(data, split==TRUE)
View(training_set)
test_set<- subset(data, split==FALSE)
View(test_set)

#Fitting the Simple Linear Regression to the training set
regressor<- lm(formula = Scores~Hours, data = training_set)
summary(regressor)

#Predicting for test set
pred<- predict(regressor, newdata = test_set)
pred

#PREDICT SCORE IF A STUDENT STUDY FOR 9.25hrs A DAY
pred_score<- predict(regressor, data.frame(Hours=9.25))
pred_score

#VISUALIZATION
library(ggplot2)

#visualising the dataset provided
ggplot()+
  geom_point(aes(x=data$Hours, y=data$Scores),
             colour="red", size=2.5)+
  geom_line(aes(x=data$Hours, y=predict(regressor, newdata = data)),
            colour="black", size=1.3)+
  xlab("HOURS STUDIED")+
  ylab("SCORE")+
  ggtitle("HOURS STUDIED vs SCORES ACHIEVED for the DATASET")

#visualising training set
ggplot()+
  geom_point(aes(x=training_set$Hours, y=training_set$Scores),
                colour="red", size=2.5)+
  geom_line(aes(x=training_set$Hours, y=predict(regressor, newdata = training_set)),
             color="blue", size=1.3)+
  xlab("HOURS")+
  ylab("SCORE")+
  ggtitle("HOURS STUDIED PER DAY vs SCORE ACHIEVED for TRAINING SET")

#visualising for test set
ggplot()+
  geom_point(aes(x=test_set$Hours, y=test_set$Scores),
            colour="red", size=2.5)+
  geom_line(aes(x=training_set$Hours, y=predict(regressor, newdata = training_set)),
             colour="dark green", size=1.3)+
  xlab("HOURS STUDIED")+
  ylab("SCORE")+
  ggtitle("HOURS STUDIED vs SCORES ACHIEVED for TEST SET")
