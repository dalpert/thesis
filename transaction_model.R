library(readxl)
trans_df <- read_excel("~/Desktop/Thesis/data/transaction_model_data.xlsx")
fit = lm(delta_gini ~ coming_usg_1 + coming_usg_2 + coming_usg_3 + leaving_usg_1 + leaving_usg_2 + leaving_usg_3, 
         data = trans_df)
summary(fit)

fit = lm(delta_gini ~ coming_usg_1 + leaving_usg_1, data = trans_df)
summary(fit)

#trans_df[is.na(trans_df)] <- 0
fit = lm(delta_gini ~ (I1*coming_usg_1) + (I2*coming_usg_2) + (I3*coming_usg_3) + 
           (I4*leaving_usg_1) + (I5*leaving_usg_2) + (I6*leaving_usg_3), 
         data = trans_df)
summary(fit)

trans_df[is.na(trans_df)] <- 0
fit = lm(delta_gini ~ (I1*coming_usg_1) + (I2*coming_usg_2) + 
           (I4*leaving_usg_1) + (I5*leaving_usg_2), 
         data = trans_df)
summary(fit)

trans_df[is.na(trans_df)] <- 0
fit = lm(delta_gini ~ coming_usg_1 + coming_usg_2 + 
           leaving_usg_1 + leaving_usg_2, 
         data = trans_df)
summary(fit)

fit2 = lm(trans_df$delta_gini ~ (trans_df$coming_usg_1 - trans_df$leaving_usg_1) +
          (trans_df$coming_usg_2 - trans_df$leaving_usg_2) + 
          (trans_df$coming_usg_3 - trans_df$leaving_usg_3))



fas1 <- read_excel("~/Desktop/Thesis/data/free_agent_signings_2015.xlsx")
fas2 <- read_excel("~/Desktop/Thesis/data/free_agent_signings_2016.xlsx")
fas3 <- read_excel("~/Desktop/Thesis/data/free_agent_signings_2017.xlsx")
df <- join_all(list(fas1,fas2,fas3), by = 'cn', type = 'full')
df_clean = data.frame(df$team_prev_gini_before, df$team_prev_gini_after)
df_clean = df_clean[!duplicated(df_clean), ]
nrow(df_clean)

plot(df_clean$df.team_prev_gini_before, df_clean$df.team_prev_gini_after)
cor(df_clean$df.team_prev_gini_before, df_clean$df.team_prev_gini_after)
