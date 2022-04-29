# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 9) # Remove scientific notation for numbers with 9 decimals or less

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
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only=TRUE)
}
rm(packages)
#loading the dataset
load("nba.full.RData")
#getting rid of variables we dont need and those with too many NAs
nba.data <- subset(nba, select = -c(playerid, teamid, season, teamfullsal, 
                                    player, team, salary.lead, fg, fga, trb, 
                                    salary.team.total, salary.team.mean, 
                                    salary.team.median, position))
#getting rid of some NA's
nba.data$fg3.pct[nba.data$fg3a==0] <- 0
nba.data$ft.pct[nba.data$fta==0] <- 0
#now we remove all observations with missing values
nba.data <- na.omit(nba.data)

#From here we develop our inference model, fist we start with OLS on all the relevant veriables
nba.best.ols <- lm(salary~., nba.data)
summary(nba.best.ols)

#There are way too many coefficients and the result is hard to interpret, there are also some 
#very high t values as well. We use stepwise selection to get rid of some of those stressors that
#are too insignificant

nba.backward <- regsubsets(salary~., nba.data, nvmax=NULL, method="backward")

#plotting the r2 value.
plot(summary(nba.backward)$bic)
which.min(summary(nba.backward)$bic)
#although R^2 is highest when there are 24 variables, we can see from the graph that
# n=10 is good enough for an inference model, and the economic interpretation should be much clearer.
#now we try if forward selection method gives us better rsquared:
nba.forward <- regsubsets(salary~., nba.data, nvmax=NULL, method="forward")
#plotting the r2 value.
plot(summary(nba.forward)$bic)
which.min(summary(nba.forward)$bic)
#seeing the difference
summary(nba.backward)$bic[16]-summary(nba.forward)$bic[17]
#backward selection does a tiny bit better job at explaining the variation in salary when n=10.
#now I've copied the formula generating function from the lecture codes:
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
#now we compare the coefficients in those regressions to see if the interpretation gets easier.
reg.best.backward <- lm(get.model.formula(which.min(summary(nba.backward)$bic), nba.backward, "salary"), 
                        nba.data)
reg.best.forward <- lm(get.model.formula(which.min(summary(nba.forward)$bic), nba.forward, "salary"), 
                        nba.data)

stargazer(nba.best.ols, reg.best.forward, reg.best.backward
          , column.labels = c("OLS", "Forward", "Backward")
          , dep.var.labels.include = FALSE
          , dep.var.caption  = ""
          , type = "html", style = "default", digits = 2
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "regs.html"
)

#Now we've adopted the model from backwards regression, we want to add some
#polynomial terms to age since we believe that to be mattering in the data
formula <- get.model.formula(10, nba.backward, "salary")
#lets see first if what the residuals look like in the model:
residualPlots(reg.best.backward)
#Just as we expected, age is the only variable that needs a polynomial term,
#we find the best polynomial term to use here using CV, but we don't necessarily
#want the best degree, just a balance between accuracy and difficulty of interpretation.

#Using CV to determine best polynomial
nba.cv.fit <- data.frame(poly = seq.int(from = 1, to = 5, by = 1)
                         , mse = NA
)
formula <- "salary ~ yrend + age + gs + mp + fg.pct + fg2a + efg.pct + drb + ast + pts"
model.NBA <- lm(as.formula(formula), data = nba.data)
for (i in 1:5) { # The inner loop cycles through polynomials
  # Adjust the formula
  formula.cv <- paste(formula, " + poly(age, degree = ", i, ", raw = TRUE)", sep="")
  # Estimate the model
  model.cv <- glm(as.formula(formula.cv), data = nba.data)
  #  Calculate MSE from CV
  mse <- cv.glm(data = nba.data, glmfit = model.cv, K = 10)$delta[1]
  # Put MSE back into main dataframe
  nba.cv.fit$mse[nba.cv.fit$poly == i] <- mse
}
nba.cv.fit$mse <- nba.cv.fit$mse/1000000000000
#how are the errors behaving as we add more polynimials?
p<-ggplot(data=nba.cv.fit, aes(x=poly, y=mse)) +
  geom_bar(stat="identity", color="blue", fill="lightblue")+
  theme_bw()+
  coord_cartesian(ylim=c(11.5,12.5))
p
#although 4th degree polynomial makes it quite hard to interpret, but it does significantly
#lower our errors so we are going to add it in.
formula <- "salary ~ yrend + age + gs + mp + fg.pct + fg2a + efg.pct + drb + ast + pts + 
            poly(age, degree = 4, raw = TRUE)"
formula <- gsub("\n|  ", "", formula)
reg.best.backward.withpoly <- lm(as.formula(formula), nba.data)
reg.best.backward.withpoly2 <- lm(as.formula("salary ~ yrend + age + gs + mp + fg.pct + fg2a + efg.pct + drb + ast + pts + 
                                             poly(age, degree = 2, raw = TRUE)"), nba.data)
stargazer(reg.best.backward, reg.best.backward.withpoly, reg.best.backward.withpoly2
          , column.labels = c("Backward", "Backward+AgePolynomials", "Backward+AgePolynomials(n=2)")
          , dep.var.labels.include = FALSE
          , dep.var.caption  = ""
          , type = "html", style = "default", digits = 2
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "regs2.html"
)
#And that's it for our inference model, now on to the prediction model.
