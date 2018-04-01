library(BradleyTerry2)
library(readxl)
library(plyr)

df <- read_excel("~/Desktop/Thesis/data/BT_data/WL_data_2003_full.xlsx")
yrs = 2004:2017
yrs = yrs[-c(3,4)]
for (i in yrs) {
  new_df = read_excel(paste("~/Desktop/Thesis/data/BT_data/WL_data_", i, "_full.xlsx", sep=''))
  df = join_all(list(df, new_df), type = 'full')
}
nrow(df)


covariates = df[!duplicated(df$team_full),c(1,5:7)]
covariates$ones = 1
games  = df[,c(2:3,9:10)]
row.names(covariates) = covariates$team_full



nba.df = list(games=games,covariates=covariates)
names(nba.df$games) = c("L", "W", "team2", "team1")
levels(nba.df$games$team1)=levels(nba.df$games$team2)
nba.bt3 <- BTm(cbind(W,L), as.factor(team1), as.factor(team2),
               ~ ones[..] + gini[..] + I(gini[..]^2) + (1|..),
               data = nba.df)

nba.bt2 <- BTm(cbind(W,L), as.factor(team1), as.factor(team2),
               ~ ones[..] + gini[..] + (1|..),
               data = nba.df)


anova(nba.bt2, nba.bt3)

fit <- BTm(cbind(W,L), as.factor(team1), as.factor(team2), 
           data=nba.df, id = 'team')

nba.bt1 <- BTm(cbind(W,L), as.factor(team1), as.factor(team2),
               ~ ones[..] + (1|..),
               data = nba.df)

#nba.bt1 <- BTm(cbind(W,L), as.factor(team1), as.factor(team2),
#               ~ ..,
#               data = nba.df)
abilities <- data.frame(BTabilities(nba.bt1))$ability
n = length(df$team_full)
probs = matrix(data = NA, nrow = n, ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    probs[i, j] = exp(abilities[i] - abilities[j])/(1 + exp(abilities[i] - abilities[j]))
  }
}
rownames(probs) = levels(df$team_full)
colnames(probs) = levels(df$team_full)


