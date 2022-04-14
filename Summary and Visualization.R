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
              , "MASS" # For stepwise selection
              , "ggrepel"   # For labels in scatter plots
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
setwd("C:/Users/hammerhao/Downloads")
load("nba.full.RData")
summary(nba)
summary(nba$salary)
summary(nba$salary[nba$yrend==2018])[4]
for (i in 2001:2019){
  x <- summary(nba$salary[nba$yrend==i])
  x
}
meansalary <- rep(NA, times=19)
for (i in 2001:2019){
  meansalary[i-2000] = mean(nba$salary[nba$yrend==i])
}
#Histogram of salary
ggplot(nba, aes(x=salary))+
  geom_histogram(bins = 50
                 , color = "darkblue"
                 , fill = "lightblue"
  ) +
  # Adjust y-axis ticks
  scale_y_continuous(breaks = seq(from = 0        
                                  , to = 2000
                                  , by = 400
  )
  ) +
  # Adjust x-axis ticks
  scale_x_continuous(breaks = seq(from = 0        
                                  , to = 50000000
                                  , by = 10000000
  )
  ) +
  labs(title = "Distribution of Yearly Salaries"
       , subtitle = ""
       , x = "Yearly salary"
       , y = "Number of players"
  ) +
  # Apply black-and-white theme
  theme_bw()
#Salaries by year
ggplot(nba, aes(x = season # Make two boxplots next to each other
                , y = salary
                , fill = season # Use different fill color
)
) +
  geom_boxplot() +
  # Flip axis around, can be used in any ggplot object 
  # (note that you can't switch x and y in aes())
  coord_flip() +
  theme_bw() +
  labs(title = "Distribution of salaries by season"
       , subtitle = ""
       , x = "Season"
       , y = "Player salary"
  )
nbayear <- split(nba, nba$yrend)
nba.2010 <- nbayear$"2010"
#2010points and salary in 2011
ggplot(nba.2010, aes(x = pts
                     , y = salary.lead  
)
) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , size = 3
             , shape = 21         
             , alpha = 1          
  ) +
  labs(title = "points per game in 2010 and salary in 2011"
       , subtitle = ""
       , x = "points per game in 2010"
       , y = "Salary in 2011"
  ) +
  theme_bw()
#Agegroup and salary
nba$agegroup <- as.integer(nba$age==23 | nba$age==24 | nba$age==25 | nba$age==26 | nba$age==27) + 
  2*as.integer(nba$age==28 | nba$age==29 | nba$age==30 | nba$age==31 | nba$age==32) +
  3*as.integer(nba$age==33 | nba$age==34 | nba$age==35 | nba$age==36 | nba$age==37) +
  4*as.integer(nba$age==38 | nba$age==39 | nba$age==40 | nba$age==41 | nba$age==42)
nba$agegroup[nba$agegroup==0] <- "18-22"
nba$agegroup[nba$agegroup==1] <- "23-27"
nba$agegroup[nba$agegroup==2] <- "28-32"
nba$agegroup[nba$agegroup==3] <- "33-37"
nba$agegroup[nba$agegroup==4] <- "38-42"
nba$agegroup <- as.factor(nba$agegroup)
ggplot(nba, aes(x = agegroup
                , y = salary
                , fill = agegroup
)
) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  labs(title = "Distribution of salary for different agegroups"
       , subtitle = ""
       , x = ""
       , y = "salary"
  ) 
#pts distribution
ggplot(nba, aes(x=pts))+
  geom_histogram(bins = 20
                 , color = "darkblue"
                 , fill = "lightblue"
                 
  ) +
  labs(title = "Distribution of points per game in 2009-2010"
       , subtitle = ""
       , x = "points per game"
       , y = "Number of players"
  ) +
  theme_bw()
#Minutes vs. Pts
ggplot(nba.2010, aes(x = mp
                     , y = pts  
)
) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , size = 3
             , shape = 21         
             , alpha = 1         
  ) +
  labs(title = ""
       , subtitle = ""
       , x = "minutes played"
       , y = "points per game"
  ) +
  theme_bw()