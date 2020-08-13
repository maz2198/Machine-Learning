#### model GROUP B ####

dt_sol <- readRDS(file.choose())
dt_sol <- setDT(dt_sol)
dt_loc <- fread(file.choose())
dt_loc <- setDT(dt_loc)
library(e1071)
library(data.table)
library(rpart)
library(rpart.plot)

dt_sol$Date <- as.Date(dt_sol$Date,format = "%Y%m%d")
#### Rpart - Recursive Partitioning for classification ####

# We try for one variable GUTH
set.seed(12420352)
index <- sample(nrow(dt_sol[1:5113,c("GUTH","PC1","PC2","PC3","PC9")]),nrow(dt_sol[1:5113,c("GUTH","PC1","PC2","PC3","PC9")]))
new_sol <- dt_sol[1:5113,c("GUTH","PC1","PC2","PC3","PC9")]
new_sol.train <- new_sol[index,]
new_sol.test <- new_sol[5114:6909,c("GUTH","PC1","PC2","PC3","PC9")]
head(new_sol.test,4)
Solar.tree <- rpart(GUTH ~ ., data = new_sol.train,  cp = 0.001)
rpart.plot(Solar.tree, type = 1, fallen.leaves = FALSE)
plotcp(Solar.tree)
printcp(Solar.tree)

prune.tree1 <- prune(Solar.tree, cp = 0.0012885)
prune.tree1

rpart.plot(prune.tree1, type =1, fallen.leaves = FALSE)
new_sol.train.pred.tree = predict(prune.tree1)
Solar.test.pred.tree = predict(prune.tree1, new_sol.test)
mean((Solar.test.pred.tree - new_sol.test$GUTH)^2)
Solar.test.pred.tree

#### RandomForest ####
library(randomForest)
library(ggplot2)
rfNews()
oob.err<- data.frame()

for(i in 3:5){
  fit<- randomForest(GUTH~., data = dt_sol[1:5113,c("GUTH","PC1","PC2","PC3","PC9")], mtry=i)
  oob.err <- rbind(oob.err,data.frame(cbind(mtry=i, ntree=seq(1,500), oob_error=fit$mse)))
  cat(i, " ")
}

ggplot(data=oob.err, aes(x=ntree, y=oob_error, color=mtry)) + geom_point() + geom_line()
# therefore 5 and 350 trees

#### Model ####
Soll <- as.formula("GUTH ~ .")
rf.Solar <- randomForest(Soll,data=new_sol.train, mtry=5, ntree=350, importance =TRUE)
rf.Solar
varImpPlot(rf.Solar) # Check variable importance
#see 73. 56 of variance is explained

solar1.train.pred.tree = predict(rf.Solar)
solar1.test.pred.tree = predict(rf.Solar, new_sol.test)
mean((solar1.test.pred.tree - new_sol.test$GUTH)^2)
