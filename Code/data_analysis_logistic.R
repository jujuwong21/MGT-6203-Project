#################################################### MODELING ####################################################

###################  Modeling for question of who wins

library(pROC)

set.seed(1)

# Logistic regression
fight_data_reg <- fight_data %>% inner_join(df_records, by=c('date'='date', 'Winner'='fighter'))
fight_data_reg <- subset(fight_data_reg, select=-c(Loser,date))
fight_data_reg <- mutate(fight_data_reg, Winner=ifelse(fight_data_reg$Winner==fight_data_reg$R_fighter,1,0))
fight_data_reg <- subset(fight_data_reg, select=-c(R_fighter,B_fighter,last_round,last_round_time))
fight_data_reg <- fight_data_reg %>% select("Winner",everything())
names(fight_data_reg) <- gsub(" ","_",names(fight_data_reg))
names(fight_data_reg) <- gsub("-","",names(fight_data_reg))
names(fight_data_reg) <- gsub("/","",names(fight_data_reg))
names(fight_data_reg) <- gsub("'","",names(fight_data_reg))
names(fight_data_reg) <- gsub("\\(","",names(fight_data_reg))
names(fight_data_reg) <- gsub("\\)","",names(fight_data_reg))
names(fight_data_reg) <- gsub("\\+","",names(fight_data_reg))

m <- nrow(fight_data_reg)
trn <- sample(1:m, size=round(m*0.7), replace=FALSE)
d.learn <- fight_data_reg[trn,]
d.valid <- fight_data_reg[-trn,]

# all variables
reg <- glm(Winner~., family=binomial(link="logit"), data=d.learn)
summary(reg)

# significant variables from 1st run
reg <- glm(Winner~R_KD+B_KD+B_SIG_STR_pct+R_SIG_STR_pct+B_SIG_STR_ATT+B_SIG_STR_LND+R_SIG_STR_ATT+B_TOT_STR_LND+B_TOT_STR_ATT+R_TD_LND+R_TD_ATT+R_TD_LND+R_SUB_ATT+
             B_SUB_ATT+R_REV+B_REV+B_CTRL+R_HEAD_LND+R_DIST_ATT+B_DIST_LND+B_DIST_ATT+B_CLINCH_LND+win_by_Decision__Majority+win_by_Decision__Split+
             win_by_Decision__Unanimous+win_by_KOTKO+win_by_Submission+Format_3_Rnd_555+Format_3_Rnd__OT_5555+Fight_type_Catch_Weight_Bout+
             Fight_type_Womens_Featherweight_Bout+total_wins+total_draws, family=binomial(link="logit"), data=d.learn)
summary(reg)

# significant variables from 2nd run
reg <- glm(Winner~R_KD+B_KD+R_SIG_STR_pct+B_SIG_STR_ATT+B_SIG_STR_LND+R_SIG_STR_ATT+B_TOT_STR_LND+B_TOT_STR_ATT+R_TD_LND+R_TD_ATT+R_SUB_ATT+B_SUB_ATT+
             B_CTRL+R_HEAD_LND+R_DIST_ATT+B_DIST_LND+B_DIST_ATT+B_CLINCH_LND+win_by_KOTKO+win_by_Submission+Fight_type_Catch_Weight_Bout+
             Fight_type_Womens_Featherweight_Bout+total_wins+total_draws, family=binomial(link="logit"), data=d.learn)
summary(reg)

# significant variables from 3nd run w/ p-value <= 0.05
reg <- glm(Winner~R_KD+B_KD+R_SIG_STR_pct+B_SIG_STR_ATT+B_SIG_STR_LND+R_SIG_STR_ATT+B_TOT_STR_LND+B_TOT_STR_ATT+R_TD_LND+R_TD_ATT+R_SUB_ATT+B_SUB_ATT+B_CTRL+R_HEAD_LND+
             R_DIST_ATT+B_DIST_LND+B_DIST_ATT+B_CLINCH_LND+win_by_KOTKO+win_by_Submission+Fight_type_Catch_Weight_Bout+total_wins+total_draws, 
           family=binomial(link="logit"), data=d.learn)
summary(reg)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -0.1537809  0.2484769  -0.619 0.535986    
# R_KD                          1.8765360  0.1589347  11.807  < 2e-16 ***
#   B_KD                         -1.7800987  0.1324432 -13.440  < 2e-16 ***
#   R_SIG_STR_pct                 2.5954630  0.3668791   7.074 1.50e-12 ***
#   B_SIG_STR_ATT                 0.1324144  0.0236594   5.597 2.18e-08 ***
#   B_SIG_STR_LND                -0.2679008  0.0318113  -8.422  < 2e-16 ***
#   R_SIG_STR_ATT                 0.0444080  0.0054365   8.168 3.12e-16 ***
#   B_TOT_STR_LND                 0.0713406  0.0192825   3.700 0.000216 ***
#   B_TOT_STR_ATT                -0.0605283  0.0172160  -3.516 0.000438 ***
#   R_TD_LND                      0.3826507  0.0492734   7.766 8.11e-15 ***
#   R_TD_ATT                     -0.0804117  0.0201073  -3.999 6.36e-05 ***
#   R_SUB_ATT                     0.9856471  0.0807146  12.212  < 2e-16 ***
#   B_SUB_ATT                    -0.5988681  0.0656379  -9.124  < 2e-16 ***
#   B_CTRL                       -0.0051813  0.0005038 -10.284  < 2e-16 ***
#   R_HEAD_LND                    0.0460331  0.0056030   8.216  < 2e-16 ***
#   R_DIST_ATT                   -0.0320385  0.0053495  -5.989 2.11e-09 ***
#   B_DIST_LND                    0.1337571  0.0237236   5.638 1.72e-08 ***
#   B_DIST_ATT                   -0.0761323  0.0147547  -5.160 2.47e-07 ***
#   B_CLINCH_LND                  0.0498665  0.0126107   3.954 7.68e-05 ***
#   win_by_KOTKO                 -0.9292582  0.1758813  -5.283 1.27e-07 ***
#   win_by_Submission            -0.5435228  0.1733371  -3.136 0.001715 ** 
#   Fight_type_Catch_Weight_Bout -1.4808364  0.5474271  -2.705 0.006829 ** 
#   total_wins                    0.1111439  0.0145349   7.647 2.06e-14 ***
#   total_draws                  -0.4251715  0.1536535  -2.767 0.005656 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 6324.0  on 4879  degrees of freedom
# Residual deviance: 2806.1  on 4856  degrees of freedom
# (229 observations deleted due to missingness)
# AIC: 2854.1
# 
# Number of Fisher Scoring iterations: 6


# Validation
y_hat <- predict(reg,d.valid,type="response")

# evaluate different threshold's auc and accuracy
acc <- c()
auc <- c()

for (i in 1:9) {
  y_hat_round <- as.integer(y_hat > i/10)
  t <- table(y_hat_round, d.valid$Winner)
  acc <- cbind(acc,(t[1,1]+t[2,2]) / sum(t))
  r <- roc(d.valid$Winner,y_hat_round)
  auc <- cbind(auc,r$auc)
}

acc
#           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]
# [1,] 0.7884892 0.8282974 0.8527578 0.8681055 0.8772182 0.8724221 0.8580336 0.8378897 0.7573141
auc
#           [,1]      [,2]      [,3]     [,4]      [,5]      [,6]      [,7]      [,8]      [,9]
# [1,] 0.7065057 0.7678497 0.8087633 0.837737 0.8597458 0.8639642 0.8641095 0.8579664 0.8028821

# threshold of 0.5 provides the highest accuracy and threshold of 0.7 provides the highest auc

r <- roc(d.valid$Winner,as.integer(y_hat > 0.7))
plot(r, main="ROC Curve")
r

# Depending on how much a bettor can stand to lose, we should look at minimizing losses
loss <- c()

for(i in 1:100) {
  y_hat_round <- as.integer(y_hat > (i/100)) # calculate threshold predictions
  
  tm <-as.matrix(table(y_hat_round,d.valid$Winner))
  
  if(nrow(tm)>1) { c1 <- tm[2,1] } else { c1 <- 0 }
  if(ncol(tm)>1) { c2 <- tm[1,2] } else { c2 <- 0 }
  loss <- c(loss, c2*5 + c1)
}

plot(c(1:100)/100,loss,xlab = "Threshold",ylab = "Loss",main = "Loss vs Threshold")
loss
which.min(loss) 
# 0.13 is the threshold to expect minimum loss (481 out of 2190 validation points).

y_hat_round <- as.integer(y_hat > (which.min(loss)/100)) # 0/1 preds
t <- table(y_hat_round, d.valid$Winner)
acc <- (t[1,1]+t[2,2])/sum(t)
r <- roc(d.valid$Winner,y_hat_round)
auc <- r$auc

acc # 0.8057554
auc # 0.7318
plot(r, main="ROC Curve")

# run for specific matchups based on aggregated stats. this would be useful for future matchups where you can plug in #'s
fighter_agg <- fighter_agg %>% drop_na()
fighter_agg <- fighter_agg %>% inner_join(df_records, by=c('date'='date','fighter'='fighter'))

# red (1)= damon jackson, blue(0)= bobby green
matchup <- data.frame(r_figher='Damon Jackson',b_fighter='Bobby Green',R_KD=0,B_KD=0.012035303557100828,R_SIG_STR_pct=0.426,B_SIG_STR_ATT=11.70633859320674,
                      B_SIG_STR_LND=6.053757689221717,R_SIG_STR_ATT=6.807004470938897,B_TOT_STR_LND=6.936346616742444,B_TOT_STR_ATT=12.717304092003209,
                      R_TD_LND=0.15648286140089418,R_TD_ATT=0.40238450074515647,R_SUB_ATT=0.12295081967213115,B_SUB_ATT=0.016047071409467772,B_CTRL=8.829901043,
                      R_HEAD_LND=1.8889716840536512,R_DIST_ATT=5.2533532041728765,B_DIST_LND=4.906392083444771,B_DIST_ATT=10.306231612730675,
                      B_CLINCH_LND=0.6579299277881786,win_by_KOTKO=0.2,win_by_Submission=0.4,Fight_type_Catch_Weight_Bout=0,total_wins=5,total_draws=2)

matchup$r_win <- predict(reg, newdata=matchup, type="response")
matchup

# red has a 52.6% chance of winning
