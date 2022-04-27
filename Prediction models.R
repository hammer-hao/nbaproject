# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 5) # Remove scientific notation for numbers
# Prepare needed libraries
packages <- c("ggplot2"     # Best plotting
              , "gridExtra" # Arrange multiple plots in a grid
              , "leaps"     # Best subset selection
              , "ISLR"      # Textbook datasets
              , "stargazer" # Nice output tables
              , "glmnet"    # Ridge and Lasso
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


load("/Users/wangyanting/Desktop/Project/nba.full.RData")


nba.data <- subset(nba, select = -c(playerid, teamid, season, teamfullsal, player, team, salary.lead, fg, fga, trb, salary.team.total, salary.team.mean, salary.team.median))
nba.data$position <- as.factor(nba.data$position)
nba.data$fg3.pct[nba.data$fg3a==0] <- 0
nba.data$ft.pct[nba.data$fta==0] <- 0
nba.data <- na.omit(nba.data)

View(nba.data)


nba.train <- sample(nrow(nba.data), 0.8*nrow(nba.data))
data.train <- nba.data[nba.train, ]
data.test <- nba.data[-nba.train, ]

# Ridge
nba.ridge1 <- model.matrix(salary ~ ., data.train)[,-1]
str(nba.ridge1)
View(nba.ridge1)

nba.reg.ridge2<-cv.glmnet(x = nba.ridge1
                       ,y = data.train$salary
                       ,alpha = 0
                       )

coef(nba.reg.ridge2)

plot(nba.reg.ridge2,xvar="lambda",label= TRUE)
nba.reg.ridge2$lambda.min
coef(nba.reg.ridge2, s = "lambda.min")

# Ridge & interactions
nba.ridge2 <- model.matrix(salary ~ .^2, data.train)[,-1]
View(nba.ridge2)
reg.best.ridge <- cv.glmnet(x = nba.ridge2
                            , y = data.train$salary
                            , alpha = 0
)
plot(reg.best.ridge)
reg.best.ridge$lambda.min
coef(reg.best.ridge, s = "lambda.min")

results.best.ridge <- data.frame(feature = names(coef(reg.best.ridge, s = "lambda.min")[- 1,1])
                                 , ridge = round(coef(reg.best.ridge, s ="lambda.min")[-1,1], 5)
                                )

View(results.best.ridge)

results <- merge(results, results.best.ridge
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE )

# Lasso
nba.lasso1 <- model.matrix(salary ~ ., data.train)[,-1]
str(nba.lasso1)
View(nba.lasso1)

nba.reg.lasso2<-cv.glmnet(x = nba.lasso1
                          ,y = data.train$salary
                          ,alpha = 1
                          )

coef(nba.reg.lasso2)

plot(nba.reg.lasso2,xvar="lambda",label= TRUE)
nba.reg.lasso2$lambda.min
coef(nba.reg.lasso2, s = "lambda.min")

# Lasso & interaction
nba.lasso2 <- model.matrix(salary ~ .^2, data.train)[,-1]
View(nba.lasso2)
reg.best.lasso <- cv.glmnet(x = nba.lasso2
                            , y = data.train$salary
                            , alpha = 1
                            )
plot(reg.best.lasso)
reg.best.lasso$lambda.min
coef(reg.best.lasso, s = "lambda.min")

results.best.lasso <- data.frame(feature = names(coef(reg.best.lasso, s = "lambda.min")[- 1,1])
                                 , lasso = coef(reg.best.ridge, s ="lambda.min")[-1,1])
                                )

View(results.best.lasso)

results <- merge(results, results.best.lasso
                 , by = "feature"
                 , all.x = TRUE, all.y = TRUE )



# accuracy
data.full.test <- model.matrix(salary ~ .^2, data.test)[,-1]

prediction<- data.frame( salary = data.test$salary)
# 1. Ridge
prediction$salary.ridge <- as.numeric(predict(reg.best.ridge
                                                , newx = data.full.test 
                                                , s = "lambda.min"
                                                , type = "response"
                                              )
                                        )

