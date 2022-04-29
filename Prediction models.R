# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 5) # Remove scientific notation for numbers
# Prepare needed libraries
packages <- c("haven" # To import *.dta files
              , "ggplot2" # Best plotting
              , "stargazer" # Nice output tables
              , "car" # For doing F-tests
              , "sandwich" # For robust standard errors
              , "lmtest" # For robust standard errors
              , "boot"
              , "leaps"
              , "MASS" # For stepwise selection
              , "ggrepel"   # For labels in scatter plots
              , "ISLR"
              , "glmnet"
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only = TRUE)
}
rm(packages)


load("nba.full.RData")


nba.data <- subset(nba, select = -c(playerid, teamid, season, teamfullsal, 
                                    team, salary.lead, fg, fga, trb, salary.team.total, 
                                    salary.team.mean, salary.team.median, position))
nba.data$fg3.pct[nba.data$fg3a==0] <- 0
nba.data$ft.pct[nba.data$fta==0] <- 0
nba.data <- na.omit(nba.data)

View(nba.data)

##################################

View(nba.data)
nba.data$player <- as.factor(nba.data$player)
numplayerstrain <- length(levels(nba.data$player))
set.seed(1500)
nba.data$row <- c(1:nrow(nba.data))
players.train <- sample(numplayerstrain, 
                        0.8*numplayerstrain,
                        replace = FALSE)
playernames.train <- levels(nba.data$player)[players.train]
nba.trainrows <- c()
for (i in 1:nrow(nba.data)){
  if(nba.data$player[nba.data$row==i]%in%playernames.train){
    nba.trainrows <- c(nba.trainrows, i)
  }
}
nba.data <- nba.data[,!(names(nba.data)%in%c("row", "player"))]
data.train <- nba.data[nba.trainrows,]
data.test <- nba.data[-nba.trainrows,]

#################################

data.full.train <- model.matrix(salary ~ .^2, data.train)
View(data.full.train)
data.full.train <- as.data.frame(data.full.train[,-1]) # -1 to remove the intercept 
data.full.train$salary <- data.train$salary

# forward selection
nba.forward <- regsubsets(salary~., data.full.train, 
                          nvmax=NULL, method="forward"
                          )
forward.summary <- summary(nba.forward)
plot(summary(nba.forward)$adjr2)
which.max(summary(nba.forward)$adjr2)
round(coef(nba.forward,which.max(forward.summary$adjr2)),5)

# backward selection
nba.backward <- regsubsets(salary~., data.full.train, 
                           nvmax=NULL, method="backward"
                           )

backward.summary <- summary(nba.backward)
plot(summary(nba.backward)$adjr2)
which.max(summary(nba.backward)$adjr2)
round(coef(nba.backward, which.max(backward.summary$adjr2)),5)


get.model.formula <- function(id, reg, outcome){
  # Identify all variables
  vars <- summary(reg)$which[id,-1]
  # Get model predictors used in model with id
  predictors <- names(which(vars == TRUE))
  predictors <- paste(predictors, collapse = " + ")
  # Build model formula
  formula <- as.formula(paste0(outcome, " ~ ", predictors)) 
  return(formula)
} 

get.model.formula(which.max(backward.summary$adjr2)
                  , nba.backward 
                  , "salary"
                  )



# Now let's compare forward vs backward vs standard OLS

reg.best.forward <- lm(get.model.formula(which.max(forward.summary$adjr2)
                                         , nba.forward
                                         , "salary"
                                        )
                     , data.full.train
                     )

summary(reg.best.forward)

reg.best.backward <- lm(get.model.formula(which.max(backward.summary$adjr2)
                                          , nba.backward
                                          , "salary"
                                          )
                     , data.full.train
                     )

summary(reg.best.backward)


# Standard OLS model with all variables
reg.best.OLS <- lm(salary ~ ., data.full.train)
summary(reg.best.OLS)

# Compare models
stargazer(reg.best.OLS, reg.best.forward, reg.best.backward
          , column.labels = c("OLS", "Forward", "Backward")
          , dep.var.labels.include = FALSE
          , dep.var.caption  = ""
          , type = "html", style = "default", digits = 2
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "regs.html"
)


# Let's save all coefficient values into a table
# We will use it later to compare with ridge and lasso regressions
results.ols <- data.frame(feature = names(reg.best.OLS$coefficients)[-1] # -1 to exclude intercept
                          , ols = round(coef(reg.best.OLS)[-1], 5) # -1 to exclude intercept
                         )

results.best.f <- data.frame(feature = names(reg.best.forward$coefficients)[-1]
                             , best.f = round(coef(reg.best.forward)[-1], 5)
                            )

results.best.b <- data.frame(feature = names(reg.best.backward$coefficients)[-1]
                             , best.b = round(coef(reg.best.backward)[-1], 5)
                            )

# How can we combine these together?
# We could create a loop to run through one variable at a time
# So that we can account for different sets of variables between models
# But a much better way is to do a merge
results <- merge(results.ols, results.best.f
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE
)

# And another merge, this time with backward
results <- merge(results, results.best.b
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE
)

# Need to remove unnecessary single quotes from feature names
results$feature <- gsub("`", "", results$feature)



# Ridge
nba.ridge <- model.matrix(salary ~ ., data.full.train)[,-1]
str(nba.ridge)
View(nba.ridge)

nba.reg.ridge <- cv.glmnet(x = nba.ridge
                       ,y = data.full.train$salary
                       ,alpha = 0
                       )

coef(nba.reg.ridge)

plot(nba.reg.ridge,xvar="lambda",label= TRUE)
nba.reg.ridge$lambda.min
coef(nba.reg.ridge, s = "lambda.min")

results.best.ridge <- data.frame(feature = names(coef(nba.reg.ridge, s = "lambda.min")[- 1,1])
                                 , ridge = round(coef(nba.reg.ridge, s ="lambda.min")[-1,1], 5)
                                )
results.best.ridge$feature <- gsub("`", "", results.best.ridge$feature)

results <- merge(results, results.best.ridge
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE )

# Lasso
nba.lasso <- model.matrix(salary ~ ., data.full.train)[,-1]
str(nba.lasso)

nba.reg.lasso<-cv.glmnet(x = nba.lasso
                          ,y = data.train$salary
                          ,alpha = 1
                          )

coef(nba.reg.lasso)

plot(nba.reg.lasso,xvar="lambda",label= TRUE)
nba.reg.lasso$lambda.min
coef(nba.reg.lasso, s = "lambda.min")

results.best.lasso <- data.frame(feature = names(coef(nba.reg.lasso, s = "lambda.min")[- 1,1])
                                 , lasso = coef(nba.reg.lasso, s ="lambda.min")[-1,1])

results.best.lasso$feature <- gsub("`", "", results.best.lasso$feature)

results.best.lasso$lasso[results.best.lasso$lasso == 0] <- NA 
results.best.lasso$lasso <- round(results.best.lasso$lasso, 5)

results <- merge(results, results.best.lasso
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE )

stargazer(results, summary = FALSE
          , type = "html", style = "default", digits = 5
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "results.html" )



## ACCURACY PART

#create a model matrix object with test data for each regression type
data.full.test <- model.matrix(salary ~ .^2, data.test)[,-1]
# create a predictions results dataframe
prediction<- data.frame(salary = data.test$salary)

# Calculate predictions
# OLS:
prediction$salary.OLS <- predict(reg.best.OLS, newdata = as.data.frame(data.full.test))
# Best backward
prediction$salary.b <- predict(reg.best.backward, newdata = as.data.frame(data.full.test))
# Best forward
prediction$salary.f <- predict(reg.best.forward, newdata = as.data.frame(data.full.test))

# 1. Ridge
prediction$salary.ridge <- as.numeric(predict(nba.reg.ridge
                                              , newx = data.full.test 
                                              , s = "lambda.min"
                                              , type = "response"
                                              )
                                      )

# 2. Lasso
prediction$salary.lasso <- as.numeric(predict(nba.reg.lasso
                                                , newx = data.full.test
                                                , s = "lambda.min"
                                                , type = "response"
                                              )
                                      )
# Calculate MSE:
accuracy <- data.frame(model = c("OLS"
                                 , "Backward"
                                 , "Forward"
                                 , "Ridge"
                                 , "Lasso"
                                )
                      , MSE = NA
                      )

accuracy$MSE[accuracy$model == "OLS"] <- mean((prediction$salary - prediction$salary.OLS)^2)
accuracy$MSE[accuracy$model == "Backward"] <- mean((prediction$salary - prediction$salary.b)^2)
accuracy$MSE[accuracy$model == "Forward"] <- mean((prediction$salary - prediction$salary.f)^2)
accuracy$MSE[accuracy$model == "Ridge"] <- mean((prediction$salary - prediction$salary.ridge)^2)
accuracy$MSE[accuracy$model == "Lasso"] <- mean((prediction$salary - prediction$salary.lasso)^2)

# And the winner is....
accuracy

# Relative MSE of everybody with OLS as a baseline
accuracy$MSE.relative <- accuracy$MSE/accuracy$MSE[accuracy$model == "OLS"]
accuracy