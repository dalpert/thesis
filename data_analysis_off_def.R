df <- read_excel("~/Desktop/Thesis/data/nba_large_regression_all_data.xlsx")
df$f_playoffs = factor(df$playoffs)
df$season_gini_2 = df$season_gini^2

fitp1 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg, data = df, Hess=TRUE)
fitp2 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + season_gini, data = df, Hess=TRUE)
fitp3 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + season_gini + season_gini_2, data = df, Hess=TRUE)

anova(fitp1, fitp2, test="Chisq")
anova(fitp1, fitp3, test="Chisq")
anova(fitp2, fitp3, test="Chisq")



fitp4 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + season_gini + opp_dist_off, data = df, Hess=TRUE)

anova(fitp2, fitp4, test="Chisq")

fitp5 = polr(f_playoffs ~ Z_off_rtg + Z_def_rtg + opp_dist_off, data = df, Hess=TRUE)

anova(fitp1, fitp5, test="Chisq")
