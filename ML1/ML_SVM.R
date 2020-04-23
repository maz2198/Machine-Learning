##### ML MODEL GROUP B #####

#### [Load Data] ####
dt_sol <- readRDS(file.choose())
dt_sol <- setDT(dt_sol)
dt_loc <- fread(file.choose())
dt_loc <- setDT(dt_loc)
library(e1071)
library(data.table)
####
col <- c("ACME","PC1","PC2","PC3");
head(train,3)
head(dt_sol[,col])

# train SVM model with a particular set of hyperparamets
model <- svm(ACME ~ ., data = train);  
model2 <- svm(x = train[, -"ACME", with = F], y = train$ACME); # ease of use, but drawbacks -> optimize the model

test <- dt_sol[5114:6909,c("ACME","PC1","PC2","PC3")];
head(test,3)
predictions_test <- predict(model2, newdata = test);


errors_test <- predictions_test - test$ACME;


head(errors_test)
head(predictions_test)



model <- svm(ytrain ~ ., data = xtrain
             );

# Get model predictions
predictions_train <- predict(model, newdata = train);
head(predictions_train,4)
predictions_val <- predict(model, newdata = val);
### Define grid
c_values <- 10^seq(from = -1, to = 1, by = 1);
gamma_values <- 10^seq(from = -1, to = 1, by = 1);
eps_values <- 10^seq(from = -3, to = 0, by = 1);
# epsilon parameter only for regression
# eps_values <- 10^seq(from = -3, to = 3, by = 1);

### Compute grid search
library(data.table)
grid_results <- data.table();

for (c in c_values){
  for (gamma in gamma_values){
    
    print(sprintf("Start of c = %s - gamma = %s", c,gamma));
    
    # train SVM model with a particular set of hyperparamets
    model <- svm(ACME ~ ., data = train,
                 cost = c, gamma = gamma);
    
    # Get model predictions
    predictions_train <- predict(model, newdata = train);
    #predictions_val <- predict(model, newdata = val);
    
    # Accuracy
    acc_train <- 100*sum(predictions_train == train)/nrow(train);
    #acc_val <- 100*sum(predictions_val == val$am)/nrow(val);
    
    
    # Build comparison table
    grid_results <- rbind(grid_results,
                          data.table(c = c, gamma = gamma, 
                                     acc_train = acc_train));
  }
}


# Order results by decreasing accuracy
grid_results <- grid_results[order(-acc_train)];

# Check results
View(grid_results);

# Get optimized hyperparameters
best <- grid_results[1];
best;


### Train final model



