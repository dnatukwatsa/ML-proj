
pacman::p_load(shiny, flextable, rio, sjmisc, sjlabelled, rpart.plot, caret)

setwd("D:/Proposal writing/Data folder")
the_data <- read.csv("clean data.csv", 
                     header=TRUE, 
                     sep=",",
                     stringsAsFactors = TRUE)

Hydrodata <- within(the_data, rm(snum, csection, hydro_etiology, place_of_delivery, time_infection, hydro_gp))

names(Hydrodata)
View(Hydrodata)


##classification and regression tree (CART)
# For decision tree model
library(rpart)
#install.packages("rlang", dependencies = TRUE)
library(rlang)
#install.packages(pkgs = "caret", 
#dependencies = c("Depends", "Imports"))
library(ggplot2)
library(lattice)
library(caret)
library(tidyverse)
# For data visualization
library(rpart.plot)

#pulling out data for an outcome variable from the whole dataset
outcome <- Hydrodata$sx_outcome
View(outcome)

#splitting/sampling of data (training and testing sets)
set.seed(234)
train = sample(1:nrow(Hydrodata), 280)
hydro.train = Hydrodata[train,]
hydro.test = Hydrodata[-train,]

#outcome data for the testing set
outcome.test = outcome[-train]


#building a classification tree
model1 = rpart(sx_outcome ~., data = hydro.train, method = "class", cp=0.008)

# Visualizing the unpruned tree
#rpart.plot(model1)
plot(model1, uniform=TRUE,
     main="Classification Tree for outcomes after surgical intervation")
text(model1, use.n=TRUE, all=TRUE, cex=.9)
# create attractive plot of tree
par(xpd = NA)
plot(model1)
text(model1, digits = 2)

model1$variable.importance

#making predictions
pred.tree = predict(model1, hydro.test, type="class")

#cross validation of predicted with actual values to see the performance
table(pred.tree, outcome.test) #there is a 23% misclassification and a 77% accuracy score

#confusionMatrix
confusionMatrix(pred.tree, outcome.test)

#getting a dataframe
data.frame(pred.tree, outcome.test)


#Here we consider whether pruning the tree might lead to improved results;
#cp(complex parameter) Pruning selects the cp (complexity parameter) value associated with a shorter tree that minimizes the cross-validated error rate (xerror)
plotcp(model1)
printcp(model1)

# Plot model accuracy vs different values of
# cp (complexity parameter)
plotcp(model1)

# Explicitly request the lowest cp value
model1$cptable[which.min(model1$cptable[,"xerror"]),"CP"]

bestcp <-model1$cptable[which.min(model1$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(model1, cp = bestcp) #bestcp had 0 splits and so used the second best cp with 5 splits
rpart.plot(pruned.tree)

#plot(pruned.tree)
#text(pruned.tree, cex = 0.9, xpd = TRUE)

# prediction 
pred.prune = predict(pruned.tree, hydro.test, type="class")
table(pred.prune, outcome.test) #Both the train and testing set had the same misclassification of 22.5%

#Tree prunning using caret train method
#Pruning can be easily performed in the caret package workflow, which invokes the rpart method for automatically testing different possible values of cp, then choose the optimal cp that maximize the cross-validation (“cv”) accuracy, and fit the final best CART model that explains the best our data.
#You can use the following arguments in the function train() [from caret package]:
#trControl, to set up 10-fold cross validation
#tuneLength, to specify the number of possible cp values to evaluate. Default value is 3, here we’ll use 10

#or 
set.seed(123)

model2 <- train(sx_outcome ~., data = hydro.train, method = "rpart",
                trControl = trainControl(method = "cv", number = 10),
                tuneLength = 10)

print(model2)

# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)

# Print the best tuning parameter cp that
# maximizes the model accuracy
model2$bestTune


# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
rpart.plot(model2$finalModel)
#text(model2$finalModel,  digits = 3)

# Make predictions on the test data
predicted.classes <- model2 %>% predict(hydro.test)
data.frame(hydro.test, predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == hydro.test$sx_outcome)

confusionMatrix(hydro.test$sx_outcome, predicted.classes)






