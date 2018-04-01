df <- read_excel("~/Desktop/Thesis/data/nba_results_gini_new.xlsx")

#tier0 = df[df$playoffs == 0,]
#tier1 = df[df$playoffs == 1,]
#tier2 = df[df$playoffs == 2,]
#tier3 = df[df$playoffs == 3,]
#tier4 = df[df$playoffs == 4,]
#tier5 = df[df$playoffs == 5,]

df1 = df[df$gini < 0.15,]
df2 = df[(df$gini > 0.15) & (df$gini < 0.2),]
df3 = df[(df$gini > 0.2) & (df$gini < 0.25),]
df4 = df[(df$gini > 0.25) & (df$gini < 0.3),]
df5 = df[df$gini > 0.3,]

#avg_series_win = function(df) {
#  tot = 0
#  for (i in 1:5) {
#    tot = tot + (i * sum(df$playoffs == i))
#  }
#  return(tot/nrow(df))
#}
#avg_series_win(df1)
#avg_series_win(df2)
#avg_series_win(df3)
#avg_series_win(df4)
#avg_series_win(df5)

mean(df1$playoffs)
mean(df2$playoffs)
mean(df3$playoffs)
mean(df4$playoffs)
mean(df5$playoffs)

t.test(df4$playoffs, df5$playoffs)


## Regression
reg_df <- read_excel("~/Desktop/Thesis/data/regression_data_playoffs_new.xlsx")
reg_df$new_gini_2 = reg_df$gini^2
reg_df$f_playoffs = factor(reg_df$playoffs)

fit1 = lm(playoffs ~ Z_off_rtg + Z_def_rtg, data = reg_df)
fit2 = lm(playoffs ~ Z_off_rtg + Z_def_rtg + new_gini + new_gini_2, data = reg_df)
summary(fit2)
# remove new_gini
fit3 = lm(playoffs ~ Z_off_rtg + Z_def_rtg + new_gini_2, data = reg_df)
summary(fit3)

# test fits
anova(fit1,fit2,test="Chisq")



fitp1 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg, data = reg_df, Hess=TRUE)
fitp2 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + new_gini, data = reg_df, Hess=TRUE)
fitp3 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + new_gini + new_gini_2, data = reg_df, Hess=TRUE)

anova(fitp1, fitp2, test="Chisq")
anova(fitp1, fitp3, test="Chisq")
anova(fitp2, fitp3, test="Chisq")

fito1 = oglmx(playoffs ~ Z_off_rtg + Z_def_rtg, data = reg_df)
fito2 = oglmx(playoffs ~ Z_off_rtg + Z_def_rtg + new_gini, data = reg_df)
fito3 = oglmx(playoffs ~ Z_off_rtg + Z_def_rtg + new_gini + new_gini_2, data = reg_df)

anova(fito1, fito2, test="Chisq")
