rm(list=ls())

#install.packages("ROCR")
library(party)
library(rpart)

library(RColorBrewer)
library(rattle)
library(pROC)
library(rpart.plot)
library(caret)
#download.file("http://www.football-data.co.uk/mmz4281/1819/SP1.csv", destfile = "D:/data_football/laliga/18-19.csv")

df  <- read.csv(".../seriaa_final_log.csv", row.names=NULL)
df <- df[df$Sezon > 10,]
df <- df[,-1]
sapply(df, function(x) sum(is.na(x)))
df$Sezon <- as.numeric(df$Sezon)
df$HomeTeam <- as.character(df$HomeTeam)
df$AwayTeam <- as.character(df$AwayTeam)
df <- df[df$HM1 != "M",]
df <- df[df$AM1 != "M",]
df <- df[df$HM2 != "M",]
df <- df[df$AM2 != "M",]
df <- df[df$HM3 != "M",]
df <- df[df$AM3 != "M",]

final <- read.csv(".../budget_seriaa_final.csv")
final <- final[,-1]
final$Club <- as.character(final$Club)
names(final) <- c("Club", "Age",  "Total_value", "Sezon" )
final$Sezon <- as.numeric(substr(final$Sezon,3,4))
final$Sezon <- final$Sezon+1



for (i in 1:nrow(df)){
  
  df[i,"Total_market_h"] <- unique(final[final$Club == df[i, "HomeTeam"] & final$Sezon == df[i, "Sezon"], "Total_value"])
  df[i,"Total_market_a"] <- unique(final[final$Club == df[i, "AwayTeam"] & final$Sezon == df[i, "Sezon"], "Total_value"])
  
  df[i,"Age_h"] <- unique(final[final$Club == df[i, "HomeTeam"] & final$Sezon == df[i, "Sezon"], "Age"])
  df[i,"Age_a"] <- unique(final[final$Club == df[i, "AwayTeam"] & final$Sezon == df[i, "Sezon"], "Age"])
  
  
  
}

tail(df)
#sum(is.na(df))
#names(df)
#str(df)
df$H2H_Diff <- df$H2H_Home_pts-df$H2H_Away_pts

df$Total_Diff <- df$Total_market_h - df$Total_market_a
df$Age_diff <- df$Age_h  - df$Age_a
#table(df$HM1)
#table(df$AM1)

df$HM1 <- factor(ifelse(df$HM1 == "W", 1,0))
df$AM1 <- factor(ifelse(df$AM1 == "W", 1,0))
df$HM2 <- factor(ifelse(df$HM2 == "W", 1,0))
df$AM2 <- factor(ifelse(df$AM2 == "W", 1,0))

df$H2H_Diff <- as.numeric(df$H2H_Diff)
cols <- c("FTR", "HTWinStreak3", "HTWinStreak5", "HTLossStreak3", 
          "HTLossStreak5", "ATWinStreak3", "ATWinStreak5", "ATLossStreak3", 
          "ATLossStreak5", "HM1", "AM1", "HM2", "AM2")

df[cols] <- lapply(df[cols], factor)
df$FTR <- ifelse(df$FTR =="H", 1, 0)
df$FTR <- factor(df$FTR, levels = c(0,1))
table(df$FTR)
round(prop.table(table(df$FTR)) * 100, digits = 1)

str(df)
table(df$H2H_Home)
df <- df[complete.cases(df),]

cols_to_corr <- c("HTFormPts", "ATFormPts",  "WHH", "WHD", "WHA",
                  "H2H_Home_pts", "H2H_Away_pts", "DiffLP", "DiffPts", "DiffFormPts", "H2H_Diff",
                  'HTGD','ATGD','DiffPts','DiffFormPts','HTP','ATP','Total_Diff', 'Age_diff')

A <- cor(df[,cols_to_corr])  

A[A>0.75 & A<1 ]

cols1 <- c("FTR", "HTWinStreak3", "HTWinStreak5", "HTLossStreak3", 
           "HTLossStreak5", "ATWinStreak3", "ATWinStreak5", "ATLossStreak3", 
           "ATLossStreak5","HM1", "AM1",  "HM2", "AM2",
           "HTGD", "ATGD", "DiffFormPts", "DiffLP", "H2H_Diff", "WHH", "WHA", 'Total_Diff')
#cor(df[,cols1])  

df <- df[,cols1]


sapply(df,function(x) sum(is.na(x)))

str(df)
tail(df)

set.seed(1)
smp_size <- floor(0.8 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

table(train$FTR)
round(prop.table(table(train$FTR)) * 100, digits = 1)
table(test$FTR)
round(prop.table(table(test$FTR)) * 100, digits = 1)

predictions <- prediction.object <- roc <- model <- list()
legend.label <- auc <- NULL

df_split <- data.frame(minsplit = numeric(),  acc = double())
 
for (i in 1:350){
  
  rpart <- rpart(FTR~., method="class", data= train,  cp=0.000001, minsplit=i)
  t_pred = predict(rpart,test,type="class")
  confMat <- table(test$FTR,t_pred)
  accuracy <- sum(diag(confMat))/sum(confMat)
  df_new <- data.frame(minsplit = i,  acc = accuracy)
  df_split <- rbind(df_split, df_new)
  
  
}




df_split %>% ggplot(aes(x=minsplit, y=acc)) + geom_line() + geom_point(aes(x=df_split$minsplit[df_split$acc == max(df_split$acc)][1],
                                                                           y=df_split$acc[df_split$acc == max(df_split$acc)][1]), col = "red", size=2)
df_split[df_split$acc == max(df_split$acc),]
rpart <- rpart(FTR~., method="class", data= train, cp=0.00001, minsplit=206)
summary(rpart)
plot(rpart)
text(rpart)
plotcp(rpart)
rpart.plot(rpart)
pred <- predict(object=rpart,test[-1],type="class")
t <- table(test$FTR,pred)

library(gmodels)

CrossTable(t)
confusionMatrix(t)$


minimum.error <-  which.min(rpart$cptable[,"xerror"])# w pracy wartosc = 2
optimal.complexity <- rpart$cptable[minimum.error, "CP"] 
points(minimum.error, rpart$cptable[minimum.error, "xerror"], pch=19)
prunded.tree <- prune(rpart, cp=optimal.complexity)
rpart.plot(prunded.tree)
pred_pru <- predict(object=prunded.tree,test[-1],type="class")
tt <- table(test$FTR,pred_pru)
confusionMatrix(tt)
CrossTable(tt)

plot(c.tree, tnex=2, type="simple")



##penetly table
for (i in seq(1,2,0.05)){
  penalty.matrix <- matrix(c(0,i,1,0), byrow=TRUE, nrow=2)
  rpart <- rpart(FTR~., method="class", data= train, parms = list(loss = penalty.matrix),  cp=0.00001, minsplit=206)
  rpart.plot(rpart)
  pred <- predict(object=rpart,test[-1],type="class")
  t <- table(test$FTR,pred)
  x <- sum(diag(t))/sum(t)
  print(x)
}
###




cp.optim <- rpart$cptable[which.min(rpart$cptable[,"xerror"]),"CP"]

tree <- prune(rpart, cp=cp.optim)
rpart.plot(rpart)
pred <- predict(object=rpart,test[-1],type="class")
t <- table(test$FTR,pred)
confusionMatrix(t)
printcp(rpart)

summary(rpart)
plot(rpart)
text(rpart)

#### Cross validation
library(plyr)

folds <- split(df, cut(sample(1:nrow(df)),10))
acc <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)[-1]
  train <- ldply(folds[-i], data.frame)[-1]
  tmp.model <- rpart(FTR~. , train, method = "class")
  tmp.predict <- predict(tmp.model, newdata = test, type = "class")
  conf.mat <- table(test$FTR, tmp.predict)
  acc[i] <- sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average acc using k-fold cross-validation: %.3f percent", 100*mean(acc)))
####

#full <- rpart(FTR~., method="class", data= train)
#null <- rpart(FTR~1, method="class", data= train)

c.tree  <- ctree(FTR ~ ., data = train,controls = tree_control(maxsurrogate = 3))


c.tree <- ctree(FTR ~ ., data=train,
                controls = ctree_control(mincriterion = 0.95,
                                         minsplit = 206))

plot(c.tree, tnex=2, type="extended")
t_pred = predict(c.tree,test,type="response")
confMat <- table(test$FTR,t_pred)
sum(diag(confMat))/sum(confMat)



pred <- read.csv(".../predict.csv", row.names=NULL)
for (i in 1:nrow(pred)){
  
  pred[i,"Total_market_h"] <- unique(final[final$Club == pred[i, "HomeTeam"] & final$Sezon == pred[i, "Sezon"], "Total_value"])
  pred[i,"Total_market_a"] <- unique(final[final$Club == pred[i, "AwayTeam"] & final$Sezon == pred[i, "Sezon"], "Total_value"])
  
  pred[i,"Age_h"] <- unique(final[final$Club == pred[i, "HomeTeam"] & final$Sezon == pred[i, "Sezon"], "Age"])
  pred[i,"Age_a"] <- unique(final[final$Club == pred[i, "AwayTeam"] & final$Sezon == pred[i, "Sezon"], "Age"])
  
  
  
}
pred$H2H_Diff <- pred$H2H_Home_pts-pred$H2H_Away_pts

pred$Total_Diff <- pred$Total_market_h - pred$Total_market_a
pred$Age_diff <- pred$Age_h  - pred$Age_a

pred$HM1 <- factor(ifelse(pred$HM1 == "W", 1,0))
pred$AM1 <- factor(ifelse(pred$AM1 == "W", 1,0))
pred$HM2 <- factor(ifelse(pred$HM2 == "W", 1,0))
pred$AM2 <- factor(ifelse(pred$AM2 == "W", 1,0))

pred$H2H_Diff <- as.numeric(pred$H2H_Diff)
cols <- c("FTR", "HTWinStreak3", "HTWinStreak5", "HTLossStreak3", 
          "HTLossStreak5", "ATWinStreak3", "ATWinStreak5", "ATLossStreak3", 
          "ATLossStreak5", "HM1", "AM1", "HM2", "AM2")

pred[cols] <- lapply(pred[cols], factor)

pred <- pred[,cols1]
pred1 <- pred[,-1]

pred_Value <- predict(rpart, newdata=pred1, type="class")

pred_names <- read.csv("C:/Users/pszydlik/Desktop/p/predict.csv", row.names=NULL)[,c("HomeTeam", "AwayTeam")]

out <- cbind(pred_names, pred1)
out$prediction <- pred_Value
str(out)
write.csv2(out, paste0(".../predict_tree_",format(Sys.Date(), "%Y%m%d"),".csv" ))








