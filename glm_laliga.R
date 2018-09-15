rm(list=ls())

#install.packages("ROCR")
library(ROCR)
library(caret)

#download.file("http://www.football-data.co.uk/mmz4281/1819/SP1.csv", destfile = "D:/data_football/laliga/18-19.csv")

df  <- read.csv("F:/data_football/laliga/laliga_final_log.csv", row.names=NULL)
df <- df[df$Sezon > 10,]
df <- df[,-1]
sapply(df, function(x) sum(is.na(x)))
df$Sezon <- as.numeric(df$Sezon)
df$HomeTeam <- as.character(df$HomeTeam)
df$AwayTeam <- as.character(df$AwayTeam)

final <- read.csv("F:/data_football/laliga/budget_la_liga_final.csv")
final <- final[,-1]
final$Club <- as.character(final$Club)
names(final) <- c("Club", "Age", "Foreign","Sezon",  "Total_value", "Market_value")
final$Sezon <- as.numeric(substr(final$Sezon,3,4))
final$Sezon <- final$Sezon+1


for (i in 1:nrow(df)){
  
  df[i,"Total_market_h"] <- unique(final[final$Club == df[i, "HomeTeam"] & final$Sezon == df[i, "Sezon"], "Total_value"])
  df[i,"Total_market_a"] <- unique(final[final$Club == df[i, "AwayTeam"] & final$Sezon == df[i, "Sezon"], "Total_value"])
  
  df[i,"Market_h"] <- unique(final[final$Club == df[i, "HomeTeam"] & final$Sezon == df[i, "Sezon"], "Market_value"])
  df[i,"Market_a"] <- unique(final[final$Club == df[i, "AwayTeam"] & final$Sezon == df[i, "Sezon"], "Market_value"])
  
  df[i,"Age_h"] <- unique(final[final$Club == df[i, "HomeTeam"] & final$Sezon == df[i, "Sezon"], "Age"])
  df[i,"Age_a"] <- unique(final[final$Club == df[i, "AwayTeam"] & final$Sezon == df[i, "Sezon"], "Age"])
  
  
  
}

tail(df)
#sum(is.na(df))
#names(df)
#str(df)
df$H2H_Diff <- df$H2H_Home_pts-df$H2H_Away_pts
df$Market_Diff <- df$Market_h - df$Market_a
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

#df$FTR <- factor(df$FTR, levels = c(0,1),
#                 labels = c("Remis_lub_gosc", "Gospodarz"))
table(df$FTR)
round(prop.table(table(df$FTR)) * 100, digits = 1)

str(df)

cols_to_corr <- c("HTFormPts", "ATFormPts",  "WHH", "WHD", "WHA",
              "H2H_Home_pts", "H2H_Away_pts", "DiffLP", "DiffPts", "DiffFormPts", "H2H_Diff",
              'HTGD','ATGD','DiffPts','DiffFormPts','HTP','ATP', 'Market_Diff','Total_Diff', 'Age_diff')

A <- cor(df[,cols_to_corr])  

cols1 <- c("FTR", "HTWinStreak3", "HTWinStreak5", "HTLossStreak3", 
           "HTLossStreak5", "ATWinStreak3", "ATWinStreak5", "ATLossStreak3", 
           "ATLossStreak5","HM1", "AM1",  "HM2", "AM2",
            "HTGD", "ATGD", "DiffFormPts", "DiffLP", "H2H_Diff", "WHH", "WHD", "WHA",  "Market_Diff", "Age_diff" )
#cor(df[,cols1])  

df <- df[,cols1]


sapply(df,function(x) sum(is.na(x)))


str(df)
tail(df)

          
#LABELS <- rep(rep(c("training", "validation"), c(3,2)), len=nrow(df))
#observation.type <- factor((sample(LABELS, nrow(df))))
#splitted.data.set <- split(df, observation.type)

set.seed(1)
smp_size <- floor(0.6 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]
#train_labels <- df[train_ind, 1]
#test_labels <- df[-train_ind, 1]




predictions <- prediction.object <- roc <- model <- list()
legend.label <- auc <- NULL
NAMES <- c("pełny", "wybrane zmienne")
model[[1]] <- glm(FTR~., family="binomial"(link="logit"), data= train)

full <- glm(FTR~., family="binomial"(link="logit"), data= train)
null <- glm(FTR~1, family="binomial"(link="logit"), data= train)

step(null, scope=list(lower=null, upper=full), direction="forward")
step(null, scope = list(upper=full), data=df, direction="both")

model[[2]] <- glm(FTR~   WHA + WHH + ATGD + ATWinStreak5 + HM1 + DiffLP + 
                    HM2 + HTLossStreak3,
                  family="binomial"(link="logit"), data= train)

for (i in 1:length(model)) {
  predictions[[i]] <- predict(model[[i]], new = test)
  prediction.object[[i]] <- prediction(predictions[[i]],
                                       test$FTR)
  roc[[i]] <- performance(prediction.object[[i]], "tpr", "fpr")
  auc[i] <- attr(performance(prediction.object[[i]], "auc"), "y.values")
  legend.label[i] <- paste(NAMES[i], " (AUC=", format(auc[i], digits = 4), ")",
                           sep = "")
  plot(roc[[i]], add = (i != 1), lwd=2, lty = i, main="Krzywa ROC dla dwóch modeli regresji logistycznej", 
       col=gray((i-1)/4), cex=.9)
}

legend("bottomright", legend.label, title="Modele", lty=1:length(model))
summary(df)

summary(model[[1]])
summary(model[[2]])
exp(coef(model[[1]]))
exp(coef(model[[2]]))


pred <- read.csv("F:/data_football/laliga/predict.csv", row.names=NULL)
for (i in 1:nrow(pred)){
  
  pred[i,"Total_market_h"] <- unique(final[final$Club == pred[i, "HomeTeam"] & final$Sezon == pred[i, "Sezon"], "Total_value"])
  pred[i,"Total_market_a"] <- unique(final[final$Club == pred[i, "AwayTeam"] & final$Sezon == pred[i, "Sezon"], "Total_value"])
  
  pred[i,"Market_h"] <- unique(final[final$Club == pred[i, "HomeTeam"] & final$Sezon == pred[i, "Sezon"], "Market_value"])
  pred[i,"Market_a"] <- unique(final[final$Club == pred[i, "AwayTeam"] & final$Sezon == pred[i, "Sezon"], "Market_value"])
  
  pred[i,"Age_h"] <- unique(final[final$Club == pred[i, "HomeTeam"] & final$Sezon == pred[i, "Sezon"], "Age"])
  pred[i,"Age_a"] <- unique(final[final$Club == pred[i, "AwayTeam"] & final$Sezon == pred[i, "Sezon"], "Age"])
  
  
  
}
pred$H2H_Diff <- pred$H2H_Home_pts-pred$H2H_Away_pts
pred$Market_Diff <- pred$Market_h - pred$Market_a
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

pred_Value <- predict(model[[2]], newdata=pred1, type="response")

pred_names <- read.csv("F:/data_football/laliga/predict.csv", row.names=NULL)[,c("HomeTeam", "AwayTeam")]

out <- cbind(pred_names, pred1)
out$prediction <- pred_Value

out$FTR_05 <- ifelse(out$prediction > 0.5, "Gospodarz", "Remis_lub_gosc")
out$FTR_045 <- ifelse(out$prediction > 0.45, "Gospodarz", "Remis_lub_gosc")

write.csv(out, paste0("F:/data_football/laliga/predict_",format(Sys.Date(), "%Y%m%d"),".csv" ))







names(df)




install.packages("survey")
library(survey)
varImp(model[[1]])

anova(model[[1]], test="Chisq")
anova(model[[2]], test="Chisq")
anova(model[[2]], model[[1]], test="Chisq")


ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(FTR ~ WHA + WHH + HM1 + AM1 + HTLossStreak3 + AM2 + 
                   H2H_Diff + DiffLP, 
                 data=train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
#mod_fit2 <- train(FTR ~ ., 
#                 data=train, method="glm", family="binomial",
#                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=test)
prediction <- ifelse(pred > 0.5, 1, 0)
confusionMatrix(data=pred, test$FTR)

#pred1 = predict(mod_fit2, newdata=test)
#prediction <- ifelse(pred1 > 0.5, 1, 0)
#confusionMatrix(data=pred, test$FTR)


roc.glm1 <- roc[[1]] 
Roc.glm2 <- roc[[2]]
auc.glm1 <- auc[[1]]
auc.glm2 <- auc[[2]]
