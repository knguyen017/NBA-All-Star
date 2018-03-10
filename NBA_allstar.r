nbaAllStars <- read.csv("nba_stats_2014.csv")
summary(nbaAllStars)
nbaAllStars_clean <- nbaAllStars[-c(1:5)]
nbaAllStars_clean[is.na(nbaAllStars_clean] <- 0
summary(nbaAllStars_clean)
plot(nbaAllStars_clean$all_star, nbaAllStars_clean$PTS)
boxplot(nbaAllStars_clean$all_star, nbaAllStars_clean$X3P)
nbaAllStars_norm <- as.data.frame(lapply(nbaAllStars_clean,normalize))
nbaAllStars_norm$all_star <- as.factor(nbaAllStars_norm$all_star)
sample <- createDataPartition(nbaAllStars_norm$all_star,p=0.7,list=FALSE)
trainNBA <- nbaAllStars_norm[sample,]
testNBA <- nbaAllStars_norm[-sample,]
nn_nba <- nnet(all_star ~ . , data=trainNBA, size=2, rang=0.1, maxit=200)
nn_predict <- predict(nn_nba,testNBA,type="class")
nn.table <- table(testNBA$all_star,nn_predict)
confusionMatrix(nn.table)
plot(nn_nba, .3)
model <- randomForest(all_star ~ ., data=trainNBA)
model
predictNBA <- predict(model,newdata=testNBA)
table(predictNBA,testNBA$all_star)
(174+4)/nrow(nba_test)
savehistory("~/Desktop/NBA_allstar.r")
