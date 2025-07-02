#Load data
life_exp = read.csv('/Users/amankumarjha/Downloads/Project/Life-Expectancy-Data-Updated.csv')
life_exp$Country = as.factor(life_exp$Country)
life_exp$Region = as.factor(life_exp$Region)

#Exploratory data analysis

#boxplot
scaled.life_exp = scale(life_exp[,3:21])
boxplot(scaled.life_exp)

#histograms
hist(life_exp$Incidents_HIV)
hist(life_exp$Population_mln)
hist(life_exp$GDP_per_capita)

#correlation matrix
library(ggcorrplot)
corr_mat <- round(cor(life_exp[,3:21]),2)
ggcorrplot(corr_mat,type = "lower",outline.col = "white")

#scatter plots
ggplot(life_exp, aes(x=GDP_per_capita, y=Life_expectancy, color=Region)) + geom_point()
ggplot(life_exp, aes(x=Schooling, y=Life_expectancy, color=Region)) + geom_point()

#########################    MODEL 1
#Randomly sampled 80% data for training, no country/region info
life_exp_numeric <- life_exp[-c(1,2)] 
index=sample(nrow(life_exp_numeric),size=nrow(life_exp_numeric)*0.8)
initial_train = life_exp_numeric[index,]
initial_test = life_exp_numeric[-index,]
fit = lm(Life_expectancy ~ . , data = initial_train)

#In-sample performance
summary(fit)$adj.r.squared
train_pred = predict(fit)
mean((train_pred - initial_train$Life_expectancy)^2)

#Out-of-sample performance
test_pred = predict(fit,initial_test)
mean((test_pred - initial_test$Life_expectancy)^2)


##########################.   MODEL 2
#Predicting life expectancy in each country for the year 2015. Training data will contain 15 data points per country = 2685 points.
#and 179 test points for 179 countries. No country/region info.

by_year_test_index = life_exp_numeric$Year == 2015
by_year_test = life_exp_numeric[by_year_test_index,]
by_year_train = life_exp_numeric[!by_year_test_index,]
fit_by_year_numeric_only = lm(Life_expectancy ~ . , data = by_year_train)

#In-sample performance
summary(fit_by_year_numeric_only)$adj.r.squared
train_pred = predict(fit_by_year_numeric_only)
mean((train_pred - by_year_train$Life_expectancy)^2)

#Out-of-sample performance
test_pred = predict(fit_by_year_numeric_only,by_year_test)
mean((test_pred - by_year_test$Life_expectancy)^2)

#Plotting MSE by region
by_year_test_for_plotting = life_exp[by_year_test_index,]
by_year_test_for_plotting$pred=test_pred
by_year_test_for_plotting$MSE= (by_year_test_for_plotting$Life_expectancy - by_year_test_for_plotting$pred)^2 
MSE_by_region = aggregate(x= by_year_test_for_plotting$MSE,by = list(by_year_test_for_plotting$Region),FUN = mean)
barplot(with(MSE_by_region, setNames(x, Group.1)), main = "MSE by Region", xlab = "Regions", ylab = "MSE",cex.names = 0.8)


###########################   MODEL3
#By year, region is expanded using dummy variables

dummy <- model.matrix(~ ., data = life_exp[2:21])
life_exp_expanded = data.frame(dummy[,-1])
dim(life_exp_expanded)

by_year_test_index = life_exp_expanded$Year == 2015
by_year_test = life_exp_expanded[by_year_test_index,]
by_year_train = life_exp_expanded[!by_year_test_index,]
fit_by_year_expanded = lm(Life_expectancy ~ . , data = by_year_train)

#In-sample performance
summary(fit_by_year_expanded)$adj.r.squared
train_pred = predict(fit_by_year_expanded)
mean((train_pred - by_year_train$Life_expectancy)^2)

#Out-of-sample performance
test_pred = predict(fit_by_year_expanded,by_year_test)
mean((test_pred - by_year_test$Life_expectancy)^2)

#Plotting MSE by region
by_year_test_for_plotting = life_exp[by_year_test_index,]
by_year_test_for_plotting$pred=test_pred
by_year_test_for_plotting$MSE= (by_year_test_for_plotting$Life_expectancy - by_year_test_for_plotting$pred)^2 
MSE_by_region = aggregate(x= by_year_test_for_plotting$MSE,by = list(by_year_test_for_plotting$Region),FUN = mean)
barplot(with(MSE_by_region_with_dummy_region_variables, setNames(x, Group.1)), main = "MSE by Region", xlab = "Regions", ylab = "MSE")


###########################   MODEL4
#By year, region is expanded using dummy variables and perform variable selection

dummy <- model.matrix(~ ., data = life_exp[2:21])
life_exp_expanded = data.frame(dummy[,-1])

by_year_test_index = life_exp_expanded$Year == 2015
by_year_test = life_exp_expanded[by_year_test_index,]
by_year_train = life_exp_expanded[!by_year_test_index,]
fit_by_year_expanded0 = lm(Life_expectancy ~ . , data = by_year_train)

#Backward selection using AIC
fit_by_year_expanded_back <- step(fit_by_year_expanded0)
summary(fit_by_year_expanded_back)

#Backward selection using BIC
fit_by_year_expanded_back_BIC <- step(fit_by_year_expanded0,k=log(nrow(life_exp_expanded)))
summary(fit_by_year_expanded_back_BIC)


#In-sample performance for min AIC model
summary(fit_by_year_expanded_back)$adj.r.squared
train_pred = predict(fit_by_year_expanded_back)
mean((train_pred - by_year_train$Life_expectancy)^2)

#Out-of-sample performance for min AIC model
test_pred = predict(fit_by_year_expanded_back,by_year_test)
mean((test_pred - by_year_test$Life_expectancy)^2)

#In-sample performance for min BIC model
summary(fit_by_year_expanded_back_BIC)$adj.r.squared
train_pred = predict(fit_by_year_expanded_back_BIC)
mean((train_pred - by_year_train$Life_expectancy)^2)

#Out-of-sample performance for min BIC model
test_pred = predict(fit_by_year_expanded_back_BIC,by_year_test)
mean((test_pred - by_year_test$Life_expectancy)^2)


###########################   MODEL5
#regression tree, By year, region is expanded using dummy variables,

dummy <- model.matrix(~ ., data = life_exp[2:21])
life_exp_expanded = data.frame(dummy[,-1])

by_year_test_index = life_exp_expanded$Year == 2015
by_year_test = life_exp_expanded[by_year_test_index,]
by_year_train = life_exp_expanded[!by_year_test_index,]

fit_by_year_expanded_rpart <- rpart(formula = Life_expectancy ~ ., data = by_year_train)
prp(fit_by_year_expanded_rpart,digits = 4, extra = 1)

#In-sample performance
train_pred = predict(fit_by_year_expanded_rpart)
mean((train_pred - by_year_train$Life_expectancy)^2)

#Out-of-sample performance
test_pred = predict(fit_by_year_expanded_rpart,by_year_test)
mean((test_pred - by_year_test$Life_expectancy)^2)


###########################   MODEL6
#Neural network, By year, region is expanded using dummy variables,

life_exp_nn = life_exp
by_year_test_index = life_exp_nn$Year == 2015
life_exp_nn$Year<- scale(life_exp_nn$Year)
by_year_test = life_exp_nn[by_year_test_index,]
by_year_train = life_exp_nn[!by_year_test_index,]

#Scale the data for everything other than country, region and Life expectancy. Divide life_expectancy by 100 
by_year_train[,-c(1,2,3,19,20,21)]<- as.data.frame(scale(by_year_train[,-c(1,2,3,19,20,21)]))
by_year_train$Life_expectancy = by_year_train$Life_expectancy/100

by_year_test[,-c(1,2,3,19,20,21)]<- as.data.frame(scale(by_year_test[,-c(1,2,3,19,20,21)]))

#now expand with dummy variables

dummy <- model.matrix(~ ., data = by_year_train[,2:21])
by_year_train_expanded = data.frame(dummy[,-1])

dummy <- model.matrix(~ ., data = by_year_test[,2:21])
by_year_test_expanded = data.frame(dummy[,-1])

#Fitting
life_exp.ann1<- neuralnet(Life_expectancy~., data = by_year_train_expanded, hidden = 5, linear.output = TRUE)

#In-sample performance
train_pred1 = compute(life_exp.ann1, by_year_train_expanded)
train_pred1 = train_pred1$net.result*100
mean((train_pred1-by_year_train_expanded$Life_expectancy*100)^2)

#Out-of-sample performance
test_pred1 = compute(life_exp.ann1, by_year_test_expanded)
test_pred1 = test_pred1$net.result*100
mean((test_pred1-by_year_test_expanded$Life_expectancy)^2)

#Fitting a smaller network
life_exp.ann2<- neuralnet(Life_expectancy~., data = by_year_train_expanded, hidden = 3, linear.output = TRUE)

#In-sample performance
train_pred1 = compute(life_exp.ann2, by_year_train_expanded)
train_pred1 = train_pred1$net.result*100
mean((train_pred1-by_year_train_expanded$Life_expectancy*100)^2)

#Out-of-sample performance
test_pred1 = compute(life_exp.ann2, by_year_test_expanded)
test_pred1 = test_pred1$net.result*100
mean((test_pred1-by_year_test_expanded$Life_expectancy)^2)



#####################MODEL 7
#XGBoost

dummy <- model.matrix(~ ., data = life_exp[2:21])
life_exp_expanded = data.frame(dummy[,-1])

by_year_test_index = life_exp_expanded$Year == 2015
by_year_test = life_exp_expanded[by_year_test_index,]
by_year_train = life_exp_expanded[!by_year_test_index,]

train_x = as.matrix(by_year_train[,-27])
train_y = by_year_train$Life_expectancy
test_x = as.matrix(by_year_test[,-27])
test_y = by_year_test$Life_expectancy

library(xgboost)
xgb_model = xgboost(data = train_x, label = train_y, nrounds = 100, objective = "reg:squarederror")

#In-sample performance
train_pred = predict(xgb_model, train_x)
mean((train_pred - train_y)^2)

#Out-of-sample performance
test_pred = predict(xgb_model, test_x)
mean((test_pred - test_y)^2)



