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
load("nba.full.RData")
nba.data <- subset(nba, select = -c(playerid, teamid, season, teamfullsal, player, team, salary.lead, fg, fga, trb, salary.team.total, salary.team.mean, salary.team.median))
nba.data$position <- as.factor(nba.data$position)
nba.data$fg3.pct[nba.data$fg3a==0] <- 0
nba.data$ft.pct[nba.data$fta==0] <- 0
nba.data <- na.omit(nba.data)
nba.data.full <- model.matrix(salary ~ .^2, nba.data)
View(nba.data.full)
nba.data.full <- as.data.frame(nba.data.full[,-1]) 
nba.data.full$salary <- nba.data$salary
#running OLS with all interaction terms
nba.reg <- lm(salary ~ .^2, data=nba.data)

#Using CV to determine best polynomial
nba.cv.fit <- data.frame(poly = seq.int(from = 1, to = 5, by = 1)
                             , mse = NA
)

formula <- "salary ~ fg.pct + orb + ast + stl + blk + orb + drb + fg2 + fg2a + fg3 + 
            fg3a + fg2.pct + fg3.pct + ft + fta + pf + g + gs + tov + pts"
formula <- gsub("\n|  ", "", formula)
model.NBA <- lm(as.formula(formula), data = nba.data)
for (i in 1:5) { # The inner loop cycles through polynomials
  # Adjust the formula
  formula.cv <- paste0(formula
                       , " + poly(mp, degree = ", i, ", raw = TRUE)"
                       , " + poly(age, degree = ", i, ", raw = TRUE)"
  )
  model.cv <- glm(as.formula(formula.cv), data = nba.data)
  #  Calculate MSE from CV
  mse <- cv.glm(data = nba.data, glmfit = model.cv, K = 10)$delta[1]
  # Put MSE back into main dataframe
  nba.cv.fit$mse[nba.cv.fit$poly == i] <- mse
}
formula <- "salary ~ fg.pct + orb + ast + stl + blk + orb + drb + 
            fg2 + fg2a + fg3 + fg3a + fg2.pct + fg3.pct + ft + fta + 
            pf + g + gs + tov + pts + poly(mp, degree = 4, raw = TRUE) + 
            poly(age, degree = 4, raw = TRUE)"
formula <- gsub("\n|  ", "", formula) 
nba.inference.model <- lm(as.formula(formula), data=nba.data)
nba.inference.model2 <- regsubsets(as.formula(formula), nba.data, nvmax=NULL, method="backward")
summary(nba.inference.model2)$adjr2
plot(summary(nba.inference.model2)$adjr2)
numofcoeffs2 <- which.max(summary(nba.inference.model2)$adjr2)
round(coef(nba.inference.model2, numofcoeffs2),5)
nba.inference.model3 <- regsubsets(as.formula(formula), nba.data, nvmax=NULL, method="forward")
numofcoeffs3 <- which.max(summary(nba.inference.model3)$adjr2)
plot(summary(nba.inference.model3)$adjr2)
round(coef(nba.inference.model3, numofcoeffs3),5)
#making a formula generating function
get.model.formula <- function(id, reg, outcome){
  # Identify all variables 
  vars <- summary(reg)$which[id,-1]
  # Get model predictors used in model with id
  predictors <- names(which(vars == TRUE))[1:(id-8)]
  predictors <- paste(predictors, collapse = " + ")
  # Build model formula
  formula <- as.formula(paste0(outcome, " ~ ", predictors, " + ", "poly(mp, degree = 4, raw = TRUE) + 
            poly(age, degree = 4, raw = TRUE)"))
  return(formula)
}
reg.best.backward <- lm(get.model.formula(numofcoeffs2, nba.inference.model2, "salary"), 
                        nba.data)
reg.best.forward <- lm(get.model.formula(numofcoeffs3, nba.inference.model3, "salary"), 
                        nba.data)
reg.best.ols <- nba.inference.model
stargazer(reg.best.ols, reg.best.forward, reg.best.backward
          , column.labels = c("OLS", "Forward", "Backward")
          , dep.var.labels.include = FALSE
          , dep.var.caption  = ""
          , type = "html", style = "default", digits = 2
          , no.space = TRUE, report = "vc*", omit.stat = c("ser","f", "rsq")
          , align = TRUE, single.row = TRUE, multicolumn = TRUE
          , out = "regs.html"
)