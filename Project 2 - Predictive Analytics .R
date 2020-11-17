

library(ggplot2)

df<-read.csv("/Volumes/GoogleDrive/My Drive/ECON 494 – Intro to Business Analytics/economic_freedom_index2019_data.csv")

#INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF Property Rights
df$Property.Rights2<-df$Property.Rights^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$Property.Rights3<-df$Property.Rights^3 #CUBIC TRANSFORMATION (3rd ORDER)

#A LOGARITHMIC TRANSFORMATION OF Property Rights 
df$ln_Property.Rights<-log(df$Property.Rights)

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(df)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- df[train_ind, ] #pulls random rows for training
Testing <- df[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#PLOTTING THE TRAINING AND TESTING PARTITIONS
plot(Government.Integrity ~ Property.Rights, df, xlim=c(1.5,7), ylim=c(10,45)) #PLOT ENTIRE DATASET
plot(Government.Integrity ~ Property.Rights, Training, xlim=c(1.5,7), ylim=c(10,45), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(Government.Integrity ~ Property.Rights, Testing, xlim=c(1.5,7), ylim=c(10,45),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$Property.Rights, Training$Government.Integrity, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$Property.Rights, Testing$Government.Integrity, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

#BUILDING THE MODEL FROM THE TRAINING DATA
M1 <- lm(Government.Integrity ~ Property.Rights, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Government.Integrity)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Government.Integrity)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,100,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(Property.Rights=x_grid))
plot(Training$Government.Integrity ~ Training$Property.Rights, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Government.Integrity ~ Testing$Property.Rights, col='red', pch=3)

#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
M2 <- lm(Government.Integrity ~ Property.Rights + Property.Rights2, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Government.Integrity)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Government.Integrity)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,100,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(Property.Rights=x_grid, Property.Rights2=x_grid^2))
plot(Training$Government.Integrity ~ Training$Property.Rights, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Government.Integrity ~ Testing$Property.Rights, col='red', pch=3)

#BUILDING THE CUBIC MODEL FROM THE TRAINING DATA
M3 <- lm(Government.Integrity ~ Property.Rights + Property.Rights2 + Property.Rights3, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$Government.Integrity)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$Government.Integrity)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,100,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(Property.Rights=x_grid, Property.Rights2=x_grid^2, Property.Rights3=x_grid^3))
plot(Training$Government.Integrity ~ Training$Property.Rights, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Government.Integrity ~ Testing$Property.Rights, col='red', pch=3)


#BUILDING THE LOGARITHMIC MODEL FROM THE TRAINING DATA
M4 <- lm(Government.Integrity ~ ln_Property.Rights, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$Government.Integrity)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$Government.Integrity)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,100,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M4, list(ln_Property.Rights=log(x_grid)))
plot(Training$Government.Integrity ~ Training$Property.Rights, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Government.Integrity ~ Testing$Property.Rights, col='red', pch=3)

######################################
###########MODEL COMPARISON###########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #LOGARITHMIC MODEL

########################################################
###PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER###
########################################################

x_grid <- seq(0,100,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$Government.Integrity ~ Training$Property.Rights, col='blue')
predictions_1 <- predict(M1, list(Property.Rights=x_grid))
predictions_2 <- predict(M2, list(Property.Rights=x_grid, Property.Rights2=x_grid^2))
predictions_3 <- predict(M3, list(Property.Rights=x_grid, Property.Rights2=x_grid^2, Property.Rights3=x_grid^3))
predictions_4 <- predict(M4, list(ln_Property.Rights=log(x_grid)))
lines(x_grid, predictions_1, col='darkgreen', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='green', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='lightgreen', lwd=3) #PLOTS M3
lines(x_grid, predictions_4, col='orange', lwd=3) #PLOTS M4
points(Testing$Government.Integrity ~ Testing$Property.Rights, col='red', pch=3)

