##################################
########### ECON 494 ############# 
# PRESCRIPTIVE ANALYTICS PROJECT #
#######@ NOELLE FREEBURG #########

# LOADING DATA AND PACKAGES #
library(ggplot2)
library(tseries) 
data(omitted_data)
df <- omitted_data
View(df)

# RENAMING VARIABLES #
names(df)[2] <- "gini"
names(df)[3] <- "gdp"
names(df)[4] <- "poorest"
names(df)[5] <- "richest"

##################################
####### PARTITIONING DATA ########
##################################

# CREATING NEW BINARY VARIABLE #
df$status <- NA 
for (i in 1:length(df$gdp)) 
  if (df$gdp[i]>=15000) { 
    df$status[i] <- "Developed" 
  } else {
    df$status[i] <- "Developing"
  }

# INCORPORATING NONLINEAR TRANSFORMATIONS # 
df$gini2 <- df$gini^2 #QUADRATIC TRANSFORMATION (SQUARED)
df$gini3 <- df$gini^3 #CUBIC TRANSFORMATION (CUBED)

# INCORPORATING LOGARITHMIC TRANSFORMATION # 
df$lngini <- log(df$gini)

# PARTITIONING CRITERIA # 
p <- 0.7 # 70% OF THE SAMPLE WILL BE USED FOR TRAINING

# CHECKING DIMENSIONS OF PARTITIONED DATA # 
obs_count <- dim(df)[1] # 153 OBSERVATIONS IN THE DATAFRAME

# SETTING NUMBER OF OBSERVATIONS TO BE USED IN TRAINING # 
training_size <- floor(p * obs_count) # RETURNS 70% OF TOTAL OBSERVATIONS ROUNDED DOWN TO THE NEAREST INTEGER
# training_size = 107 OBSERVATIONS

# PARTITIONING DATA # 
set.seed(1234) # MAKES PARTITION REPRODUCIBLE 
training_df <- sample(obs_count, size = training_size) 
Training <- df[training_df, ] # pulls 107 random rows for training (70%)
dim(Training) # CHECK TRAINING DIMENSIONS
Testing <- df[-training_df, ] # pulls 46 random rows for testing (30%)
dim(Testing) # CHECK TESTING DIMENSIONS
View(Training)
View(Testing)

# PLOTTING PARTITIONED DATA # 
plot(df$gdp ~ df$gini, df, xlim=c(25,60), ylim=c(0,60000)) #PLOT ENTIRE DATASET
plot(Training$gdp ~ Training$gini, Training, xlim=c(25,60), ylim=c(0,60000), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(Testing$gdp ~ Testing$gini, Testing, xlim=c(25,60), ylim=c(0,60000), col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

#BUILDING THE GINI, GDP MODEL AND GENERATING PREDICTIONS FROM TRAINING DATA (M1)
M1_A <- lm(Training$gdp ~ Training$gini, Training)
summary(M1_A) 
IN_predict1A <- predict(M1_A, Training) #GENERATE PREDICTIONS FOR IN-SAMPLE (TRAINING) DATA 
View(IN_predict1)
OUT_predict1A <- predict(M1_A, Testing) #GENERATE PREDICTIONS FOR OUT-OF-SAMPLE (TESTING) DATA
View(OUT_predict1A)

#TESTING FOR NORMALITY (M1_A)
hist(M1_A$residuals) #PLOT RESIDUALS
jarque.bera.test(M1_A$residuals) #TEST FOR NORMLAITY 
# p-value < 2.2e-16 -> reject normality assumption for Model 1A

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR (M1-A)
RMSE_1A_IN<-sqrt(sum((IN_predict1A-Training$gdp)^2)/length(IN_predict1A))  #IN-SAMPLE ERROR
# RMSE_1A_IN = 16866.87
RMSE_1A_OUT<-sqrt(sum((OUT_predict1A-Testing$gdp)^2)/length(OUT_predict1A)) #OUT-OF-SAMPLE ERROR
# RMSE_1A_OUT = 19709.11

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS (M1-A)
x_grid <- seq(0, 60, by = 1) #CREATES GRID OF X-AXIS VALUES
predictions_1A <- predict(M1_A, list(gini=x_grid))
plot(Training$gdp ~ Training$gini, col='blue', main="Model 1A: Training vs Testing", xlab= "Gini Index (%)", ylab = "GDP per Capita ($)")
lines(predictions_1A, col='green', lwd=3)
points(Testing$gdp ~ Testing$gini, col='red', pch=3)

#BUILDING THE GDP ~ GINI, RICHEST, STATUS MODEL AND GENERATING PREDICTIONS FROM TRAINING DATA (M1-B)
M1_B <- lm(Training$gdp ~ Training$gini + Training$richest + Training$status, Training)
summary(M1_B) 
IN_predict1B <- predict(M1_B, Training) #GENERATE PREDICTIONS FOR IN-SAMPLE (TRAINING) DATA 
View(IN_predict1B)
OUT_predict1B <- predict(M1_B, Testing) #GENERATE PREDICTIONS FOR OUT-OF-SAMPLE (TESTING) DATA
View(OUT_predict1B)

#TESTING FOR NORMALITY (M1_B)
hist(M1_B$residuals) #PLOT RESIDUALS
jarque.bera.test(M1_B$residuals) #TEST FOR NORMLAITY 
# p-value < 2.2e-16 -> reject normality assumption for Model 1B

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR (M1-B)
RMSE_1B_IN<-sqrt(sum((IN_predict1B-Training$gdp)^2)/length(IN_predict1B))  #IN-SAMPLE ERROR
# RMSE_1B_IN = 8242.439
RMSE_1B_OUT<-sqrt(sum((OUT_predict1B-Testing$gdp)^2)/length(OUT_predict1B)) #OUT-OF-SAMPLE ERROR
# RMSE_1B_OUT = 20355.03

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS (M1-B)
x_grid <- seq(0, 60, by = 1) #CREATES GRID OF X-AXIS VALUES
predictions_1B <- predict(M1_B, list(gini=x_grid))
plot(Training$gdp ~ Training$gini, col='blue', main="Model 1B: Training vs Testing", xlab= "Gini Index (%)", ylab = "GDP per Capita ($)")
lines(predictions_1B, col='green', lwd=3)
points(Testing$gdp ~ Testing$gini, col='red', pch=3)

#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA (M2)
M2 <- lm(Training$gdp ~ Training$gini + Training$gini2, Training)
summary(M2)
IN_predict2 <- predict(M2, Training) #GENERATE PREDICTIONS FOR IN-SAMPLE (TRAINING) DATA 
View(IN_predict2)
OUT_predict2 <- predict(M2, Testing) #GENERATE PREDICTIONS FOR OUT-OF-SAMPLE (TESTING) DATA
View(OUT_predict2)

#TESTING FOR NORMALITY (M2)
hist(M2$residuals) #PLOT RESIDUALS
jarque.bera.test(M2$residuals) #TEST FOR NORMLAITY 
# p-value < 2.2e-16 -> reject normality assumption for Model 2

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR (M2)
RMSE_2_IN<-sqrt(sum((IN_predict2-Training$gdp)^2)/length(IN_predict2))  #IN-SAMPLE ERROR
# RMSE_2_IN = 16651.94
RMSE_2_OUT<-sqrt(sum((OUT_predict2-Testing$gdp)^2)/length(OUT_predict2)) #OUT-OF-SAMPLE ERROR
# RMSE_2_OUT = 20074.78

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS (M2)
x_grid <- seq(0, 60, by = 1) #CREATES GRID OF X-AXIS VALUES
predictions_2 <- predict(M2, list(gini=x_grid))
plot(Training$gdp ~ Training$gini, col='blue', main="Model 2: Training vs Testing", xlab= "Gini Index (%)", ylab = "GDP per Capita ($)")
lines(predictions_2, col='green', lwd=3)
points(Testing$gdp ~ Testing$gini, col='red', pch=3)

#BUILDING THE CUBIC MODEL FROM THE TRAINING DATA (M3)
M3 <- lm(Training$gdp ~ Training$gini + Training$gini2 + Training$gini3, Training)
summary(M3)
IN_predict3 <- predict(M3, Training) #GENERATE PREDICTIONS FOR IN-SAMPLE (TRAINING) DATA 
View(IN_predict3)
OUT_predict3 <- predict(M3, Testing) #GENERATE PREDICTIONS FOR OUT-OF-SAMPLE (TESTING) DATA
View(OUT_predict3)

#TESTING FOR NORMALITY (M3)
hist(M3$residuals) #PLOT RESIDUALS
jarque.bera.test(M3$residuals) #TEST FOR NORMLAITY 
# p-value < 2.2e-16 -> reject normality assumption for Model 3

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR (M3)
RMSE_3_IN<-sqrt(sum((IN_predict3-Training$gdp)^2)/length(IN_predict3))  #IN-SAMPLE ERROR
# RMSE_3_IN = 16601.18
RMSE_3_OUT<-sqrt(sum((OUT_predict3-Testing$gdp)^2)/length(OUT_predict3)) #OUT-OF-SAMPLE ERROR
# RMSE_3_OUT = 20248.81

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS (M3)
x_grid <- seq(0, 60, by = 1) #CREATES GRID OF X-AXIS VALUES
predictions_3 <- predict(M3, list(gini=x_grid))
plot(Training$gdp ~ Training$gini, col='blue', main="Model 3: Training vs Testing", xlab= "Gini Index (%)", ylab = "GDP per Capita ($)")
lines(predictions_3, col='green', lwd=3)
points(Testing$gdp ~ Testing$gini, col='red', pch=3)

#BUILDING THE LOGARITHMIC MODEL FROM THE TRAINING DATA (M4)
M4 <- lm(Training$gdp ~ Training$lngini, Training)
summary(M4)
IN_predict4 <- predict(M4, Training) #GENERATE PREDICTIONS FOR IN-SAMPLE (TRAINING) DATA 
View(IN_predict4)
OUT_predict4 <- predict(M4, Testing) #GENERATE PREDICTIONS FOR OUT-OF-SAMPLE (TESTING) DATA
View(OUT_predict4)

#TESTING FOR NORMALITY (M4)
hist(M4$residuals) #PLOT RESIDUALS
jarque.bera.test(M4$residuals) #TEST FOR NORMLAITY 
# p-value < 2.2e-16 -> reject normality assumption for Model 4

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR (M4)
RMSE_4_IN<-sqrt(sum((IN_predict4-Training$gdp)^2)/length(IN_predict4))  #IN-SAMPLE ERROR
# RMSE_4_IN = 16880.53
RMSE_4_OUT<-sqrt(sum((OUT_predict4-Testing$gdp)^2)/length(OUT_predict4)) #OUT-OF-SAMPLE ERROR
# RMSE_4_OUT = 19747.22

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS (M4)
x_grid <- seq(0, 60, by = 1) #CREATES GRID OF X-AXIS VALUES
predictions_4 <- predict(M4, list(gini=x_grid))
plot(Training$gdp ~ Training$gini, col='blue', main="Model 4: Training vs Testing", xlab= "Gini Index (%)", ylab = "GDP per Capita ($)")
lines(predictions_4, col='green', lwd=3)
points(Testing$gdp ~ Testing$gini, col='red', pch=3)

################################
####### MODEL COMPARISON #######
################################

# IN-SAMPLE RANK BY RMSE: 

RMSE_1A_IN # RMSE_1A_IN = 16866.87
#MODEL WITH ONLY LINEAR TERM
RMSE_1B_IN # RMSE_1B_IN = 8242.439
#MODEL WITH LINEAR TERM AND BINARY VARIABLE
RMSE_2_IN # RMSE_2_IN = 16651.94
#MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN # RMSE_3_IN = 16601.18
#MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN # RMSE_4_IN = 16880.53
#LOGARITHMIC MODEL

# OUT-OF-SAMPLE RANK BY RMSE: 

RMSE_1_OUT # RMSE_1A_OUT = 27157.64
#MODEL WITH ONLY LINEAR TERM
RMSE_1B_OUT # RMSE_1B_OUT = 20355.03
#MODEL WITH LINEAR TERM AND BINARY VARIABLE
RMSE_2_OUT # RMSE_2_OUT = 20074.78
#MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT # RMSE_3_OUT = 20248.81
#MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT # RMSE_4_OUT = 19747.22
#LOGARITHMIC MODEL

########################################################
## PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER ##
########################################################

x_grid <- seq(0,60, by = 1) #CREATES GRID OF X-AXIS VALUES
plot(Training$gdp ~ Training$gini, col='blue', main="Complete Regression Model Analysis", xlab= "Gini Index (%)", ylab = "GDP per Capita ($)")
predictions_1A <- predict(M1_A, list(`Training$gini`=x_grid))
predictions_1B <- predict(M1_B, list(`Training$gini`=x_grid))
predictions_2 <- predict(M2, list(`Training$gini`=x_grid, `Training$gini2`=x_grid^2))
predictions_3 <- predict(M3, list(`Training$gini`=x_grid, `Training$gini2`=x_grid^2, `Training$gini3`=x_grid^3))
predictions_4 <- predict(M4, list(`Training$lngini`=log(x_grid)))
lines(predictions_1A, col='darkgreen', lwd=3) #PLOTS M1A
lines(predictions_1B, col='blue', lwd=3) #PLOTS M1B
lines(predictions_2, col='green', lwd=3) #PLOTS M2
lines(predictions_3, col='purple', lwd=3) #PLOTS M3
lines(predictions_4, col='orange', lwd=3) #PLOTS M4
points(Testing$gdp ~ Testing$gini, col='red', pch=3)


