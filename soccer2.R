# Soccer Analysis 2 for shiny app. meant to be a quick one to publish over the weekend.
library(randomForest)
library(sqldf)
library(caret)
library(pROC)
# mac
setwd("~/Google Drive/Mikee/TrueLearn/kaggle/soccer 2")
# pc 
#setwd("C:/Users/Mike Lam/Google Drive/TrueLearn/kaggle/soccer 2")
source("soccer2Fun.R")
##############################################################################################################################################
# start
events = read.csv("events.csv",header=T,colClasses = c("factor","factor","integer","integer","character","factor","factor","factor",
                                                       "factor","factor","factor","factor","factor","factor","factor",
                                                       "factor","factor","factor","factor","factor","factor","factor"))
game_info = read.csv("ginf.csv",header=T,colClasses = c(id_odsp = "factor", link_odsp = "factor",date = "Date",season="factor"))

# pick arsenal
team = "Arsenal"
events.ars = events[events$event_team == team,]
game_info.ars = game_info[which(game_info$id_odsp %in% unique(events.ars$id_odsp)),]

# get win/lose, get main player
game_info.ars$result = as.factor(apply(game_info.ars,1,getWinLose,team= team))
game_info.ars$home = as.factor(apply(game_info.ars,1,getHorA, team = team))
summary(game_info.ars)

attempts.ars = events.ars[events.ars$event_type == 1,]
df.attempts.ars = merge(attempts.ars[, !names(attempts.ars) %in% c("event_team","event_type")], 
                        game_info.ars[c("id_odsp","date","season","result","home")], by = "id_odsp") 
summary(df.attempts.ars)

# get rid of outlier
df.attempts.ars = df.attempts.ars[-which(df.attempts.ars$event_type2 ==15),] # 1. remove own goal
df.attempts.ars = df.attempts.ars[-which(df.attempts.ars$location == 19),] # 2. location not recorded
df.attempts.ars$event_type2 = factor(df.attempts.ars$event_type2)


# EDA & feature engineering
df.attempts.ars$time = as.factor(cut(df.attempts.ars$time, c(-1,15,30,45,60,75,90,120)))
levels(df.attempts.ars$location) = c("0","5","4","5","8","7","6","2","2","0","0","0","1","0","0","2","2","2","3")
df.attempts.ars$location = factor(df.attempts.ars$location)
df.attempts.ars$event_type2 = addNA(df.attempts.ars$event_type2) # add another level to the factor

  ### add player attribute function

# start train & test
X = sqldf("Select time, event_type2, location, bodypart, assist_method, situation, fast_break, home, is_goal from 'df.attempts.ars'")
X$time = as.factor(X$time)  # weird change of format after sqldf
X$event_type2 = addNA(X$event_type2)  # weird change of format after sqldf
levels(X$is_goal) = make.names(levels(X$is_goal))

# train and test set
seed = 1992
train.ind = createDataPartition(X$is_goal, p = 0.8, list = F)
X.train = X[train.ind,]
X.test = X[-train.ind,]

# hyperparameters
control = trainControl(method="repeatedcv", number=5, repeats=3, classProbs=T, summaryFunction = twoClassSummary)
metric = "ROC" #"Accuracy"
mtry = 1:9 #sqrt(ncol(X))
tunegrid = expand.grid(.mtry=mtry)

# model and prediction
rf_default = train(is_goal~., data=X.train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
print(rf_default$finalModel)
pred.prob = predict.train(rf_default, newdata=X.test, type="prob")
qplot(pred.prob$X1, X.test$is_goal)
pred = factor(ifelse(pred.prob$X1>=0.05,"X1","X0"))

# metrics
cMatrix = confusionMatrix(X.test$is_goal, pred, positive = "X1")
kappa = cMatrix$overall[2]
precision = cMatrix$byClass[5]
recall = cMatrix$byClass[6]
F1 = cMatrix$byClass[7]
importance = varImp(rf_default, scale=F)

results = list(c(kappa,precision,recall,F1,importance))
results

# to get a sense
aggregate(as.numeric(as.character(is_goal))~location, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~location, data = df.attempts.ars, length)

aggregate(as.numeric(as.character(is_goal))~time, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~time, data = df.attempts.ars, length)

aggregate(as.numeric(as.character(is_goal))~home, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~home, data = df.attempts.ars, length)

aggregate(as.numeric(as.character(is_goal))~bodypart, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~bodypart, data = df.attempts.ars, length)

aggregate(as.numeric(as.character(is_goal))~assist_method, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~assist_method, data = df.attempts.ars, length)

aggregate(as.numeric(as.character(is_goal))~season, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~season, data = df.attempts.ars, length)

aggregate(as.numeric(as.character(is_goal))~fast_break, data = df.attempts.ars, mean)
aggregate(as.numeric(as.character(is_goal))~fast_break, data = df.attempts.ars, length)

