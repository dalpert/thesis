library(BradleyTerry2)

df <- read_excel("~/Desktop/Thesis/data/BT_data/WL_data_2017_full.xlsx")
typeof(df)
df <- as.data.frame(df)
typeof(df)
df$team = as.factor(df$team)
df$opp = as.factor(df$opp)
df2 = df

### Fit model 1--just W and L data
outcome = cbind(df$W, df$L)
fit <- BTm(outcome, player1 = team, player2 = opp, 
           data=df2, id = 'team')

abilities <- data.frame(BTabilities(fit))$ability

probs = matrix(data = NA, nrow = 30, ncol = 30)
for (i in 1:30) {
  for (j in 1:30) {
    probs[i, j] = exp(abilities[i] - abilities[j])/(1 + exp(abilities[i] - abilities[j]))
  }
}

rownames(probs) = levels(df$team)
colnames(probs) = levels(df$team)

# Prob. that team1 beats team2
probs['GSW', 'CLE']


###
# Update general model --> tuned model
df2$team <- data.frame(team = df2$team, gini = as.numeric(as.character(df2$gini))) #adjem1 instead of team1.home
df2$opp <- data.frame(team = df2$opp, gini = as.numeric(as.character(df2$opp_gini))) #adjem2 instead of team2.home
#df2$team = as.factor(df2$team)
#df2$opp = as.factor(df2$opp)

fit2 <- update(fit, formula = ~ team + gini)


### Take TWO
df <- read_excel("~/Desktop/Thesis/data/BT_data/WL_data_2017_full.xlsx")
df2 = df
#dimnames(kp_full2)[[2]][c(4,7)] = c('Team1.Seed', 'Team2.Seed')

outcome = cbind(df2$W, df2$L)

df2$team=as.factor(df2$team)
df$opp=as.factor(df2$opp)
levels(df2$team)=levels(df2$opp)

# Fit general model
fit <- BTm(outcome, player1 = team, player2 = opp, 
           data=df2, id = 'Seed')

# Update general model --> tuned model
df2$team <- data.frame(Seed = df2$team, gini = as.numeric(as.character(df2$gini))) #adjem1 instead of team1.home
df2$opp <- data.frame(Seed = df2$opp, gini = as.numeric(as.character(df2$opp_gini))) #adjem2 instead of team2.home
fit2 <- update(fit, formula = ~ Seed + gini) # team + adjem1 + adjem2

#  Compare the fit of these two models:
anova(fit, fit2)



#### Copying last years code
 
# Read in/format data
df <- read_excel("~/Desktop/Thesis/data/BT_data/WL_data_2017_full.xlsx")
df2 = df
#dimnames(kp_full2)[[2]][c(4,7)] = c('Team1.Seed', 'Team2.Seed')

wins = df2$W
losses = df2$L
df2$ones = 1
outcome = cbind(wins, losses)

df2$team=as.factor(df2$team)
df2$opp=as.factor(df2$opp)
levels(df2$team)=levels(df2$opp)

# Fit general model
fit <- BTm(outcome, player1 = team, player2 = opp, 
           data=df2, id = 'team')

# Update general model --> tuned model
df2$team <- data.frame(team = df2$team, gini = df2[[7]]) #adjem1 instead of team1.home
df2$opp <- data.frame(team = df2$opp, gini = df2[[8]]) #adjem2 instead of team2.home
#fit2 <- update(fit, formula = ~ team + gini) # team + adjem1 + adjem2
fit2 <- BTm(outcome, player1 = team, player2 = opp, formula = ~ team + gini,
            data=df2, id = 'team')


#  Compare the fit of these two models:
anova(fit, fit2)


## Rader's code
nba.bt3 <- BTm(outcome, as.factor(team), as.factor(opp),
               ~ ones[..] + gini[..] + I(gini[..]^2) + (1|..),
               data = df2)

nba.bt2 <- BTm(outcome, as.factor(team), as.factor(opp),
               ~ ones[..] + gini[..] + (1|..),
               data = df2)

nba.bt1 <- BTm(outcome, as.factor(team), as.factor(opp),
               ~ ones[..] + (1|..),
               data = df2)

anova(nba.bt1, nba.bt2)
