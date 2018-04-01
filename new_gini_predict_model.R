library(readxl)

dat <- read_excel("~/Desktop/Thesis/data/gini_preds_df_new_model.xlsx")
nrow(dat)
dat = dat[complete.cases(dat), ]
nrow(dat)

sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
train <- dat[sample, ]
test  <- dat[-sample, ]

fit = lm(gini_true ~ l_gini, data = train)
fit2 = lm(gini_true ~ gini_pred, data = train)
fit3 = lm(gini_true ~ l_gini + gini_pred, data = train)

summary(fit)
summary(fit2)
summary(fit3)

anova(fit, fit3)
anova(fit2, fit3)

AIC(fit)
AIC(fit2)
AIC(fit3)

preds = predict(fit3, test)

cor(test$gini_true, test$l_gini)
cor(test$gini_true, preds)

full_preds = predict()

