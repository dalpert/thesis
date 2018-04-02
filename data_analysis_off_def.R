library(readxl)
library(MASS)

df <- read_excel("~/Desktop/Thesis/data/nba_large_regression_all_data.xlsx")
df$f_playoffs = factor(df$playoffs)
df$season_gini_2 = df$season_gini^2

fitp1 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg, data = df, Hess=TRUE)
fitp2 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + season_gini, data = df, Hess=TRUE)
fitp3 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + season_gini + season_gini_2, data = df, Hess=TRUE)

AIC(fitp1)
AIC(fitp2)
AIC(fitp3)

anova(fitp1, fitp2, test="Chisq")
anova(fitp1, fitp3, test="Chisq")
anova(fitp2, fitp3, test="Chisq")

anova(fitp1, fitp2, fitp3)

fitp4 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + season_gini + opp_dist_off, data = df, Hess=TRUE)

AIC(fitp4)
anova(fitp1, fitp2, fitp4)


fitp5 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + opp_dist_off, data = df, Hess=TRUE)
AIC(fitp5)
anova(fitp2, fitp5, test="Chisq")

# Best model
summary(fitp2)

# Prediction
# Celtics 2017
predict(fitp2,newdata = data.frame(Z_off_rtg=0.742813938, Z_def_rtg=-0.16108018, season_gini=0.261485843),type="p")
predict(fitp2,newdata = data.frame(Z_off_rtg=1.0, Z_def_rtg=-0.16108018, season_gini=0.35),type="p")
# Cavs 2017
predict(fitp2,newdata = data.frame(Z_off_rtg=1.48863522, Z_def_rtg=0.642955633, season_gini=0.288649719),type="p")
predict(fitp2,newdata = data.frame(Z_off_rtg=1.48863522, Z_def_rtg=0.642955633, season_gini=0.35),type="p")

predict(fitp2)
