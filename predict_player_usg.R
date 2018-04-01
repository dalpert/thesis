library(readxl)
library(xlsx)

### Predict next year's workload
df <- read_excel("~/Desktop/Thesis/data/season_data/season_data_2003.xlsx")
yrs = 2004:2017
for (yr in yrs) {
  season_df <- read_excel(paste("~/Desktop/Thesis/data/season_data/season_data_", yr, ".xlsx", sep=''))
  df = join_all(list(df, season_df), type = 'full')
}

nrow(df)
df <- df[complete.cases(df),]
nrow(df)
df = df[df$team != 'TOT',]
nrow(df)
df$age2 = df$age^2
df$z.age = scale(df$age)
df$z.age2 = scale(df$age)^2

sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

fit = lm(weighted_usg ~ prev_weighted_usg, data = train)
fit2 = lm(weighted_usg ~ prev_weighted_usg + age, data = train)
fit3 = lm(weighted_usg ~ prev_weighted_usg + age + age2, data = train)

AIC(fit)
AIC(fit2)
AIC(fit3)

anova(fit, fit2)
anova(fit, fit3)
anova(fit2, fit3)

preds = predict(fit3, newdata = test)

cor(test$weighted_usg, test$prev_weighted_usg)
cor(test$weighted_usg, preds)

plot(test$weighted_usg, test$prev_weighted_usg)


plot(test$weighted_usg, preds)

full_preds = predict(fit3, df)

df$full_preds = full_preds
head(df)

write_xlsx(df, "~/Desktop/Thesis/data/season_data/season_data_full.xlsx")

cor(df$weighted_usg, df$prev_weighted_usg)
cor(df$weighted_usg, df$full_preds)


df$diffs = df$weighted_usg - df$full_preds

### Predict next year's Gini
nrow(df)

sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
train <- dat[sample, ]
test  <- dat[-sample, ]

fit = lm(gini_true ~ gini_pred + l_gini, data = train)
summary(fit)
fit2 = lm(gini_true ~ l_gini, data = train)
summary(fit2)
AIC(fit)
AIC(fit2)

preds = predict(fit, test)
cor(preds, test$gini_true)

