######################################
# Wine Sales Intro ---------
## The wine data set contains information on over 12,000 commercially
## available wines. Each record is a bottle of wine. Most variables are related
## chemical attributes but more notable variables are Cases (number of cases
## purchased), STARS (rated by experts) and the dichotomous variable
## Purchase Purchase will be the response variable.



# Load Libraries -------------
library(readxl)
library(data.table)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(fastDummies)
library(formattable)
library(DT)
library(caret)
library(MASS)
library(car)
library(formatR)
options(scipen=999)



# Load Data ------------
dat <- read_excel("wine.xlsx")
dat <- as.data.table(dat)
setkey(dat, NULL)
dat <- dat[, INDEX := NULL]


# Create Function ------------
summary_func <- function(x) {
  c(
    "Mean" = round(mean(x, na.rm = TRUE), digits = 2),
    "Median" = round(median(x, na.rm = TRUE), digits = 2),
    "Min" = round(min(x, na.rm = TRUE), digits = 2),
    "Max" = round(max(x, na.rm = TRUE), digits = 2),
    "Std" = round(sd(x, na.rm = TRUE), digits = 2),
    "NA Count" = sum(is.na(x)),
    "< 0 Count" = sum(ifelse(x < 0, 1, 0), na.rm = TRUE),
    "0 Count" = sum(x == 0, na.rm = TRUE)
  )
}



# EDA --------------------------------------------
names(dat)
print(original_columns)
str(dat)
head(dat)

# Create summary tables
dat_summary_total <- t(dat[, sapply(.SD, summary_func)])
dat_summary_purchase <- dat[Purchase == 1, sapply(.SD, summary_func)]
dat_summary_no_purchase <- dat[Purchase == 0, sapply(.SD, summary_func)]
dat_summary_difference <- t(
  round(dat_summary_purchase - dat_summary_no_purchase, 2)
  )

# Print summary statistics into tables. One table for the entire dataset and 
# another table for the difference between Purchase=1 and Purchase=0.
datatable(dat_summary_total, class = 'cell-border stripe')
datatable(dat_summary_difference, class = 'cell-border stripe')

# Print histogram for all variables, omitting NA
ggplot(gather(na.omit(dat)), aes(value)) +
  geom_histogram(bins = 20) + 
  facet_wrap(~key, scales = 'free_x')

# Print boxplot for all variables, omitting NA
ggplot(gather(na.omit(dat)), aes(x=key, y=value)) +
  geom_boxplot() + 
  facet_wrap(~key, scales = 'free')



# Correlation Prior to Cleaning ---------------------------
corrplot::corrplot(cor(na.omit(dat)))



# Data Cleaning -------------------------------------------
dat[, lapply(.SD, function(x) sum(is.na(x)))]

# Delete Cases as only Purchased wines have a case count
dat[, Cases:=NULL]

# Define missing columns
missing_cols <- which(dat[, lapply(.SD, function(x) sum(is.na(x)))] > 0)
missing_col_dt <- dat[, ..missing_cols]

# No row has all missing across columns
dat[, which(unlist(lapply(missing_col_dt, function(x)all(is.na(x))))), with = F]



# Data Imputation ----------------------------------
# The missing values may be indicators so NA values are given their own column.
# Most columns are imputed with the mean.

# STARS - replace missing STARS with 0
dat$STARS_na <- ifelse(is.na(dat$STARS), 1, 0)
dat$STARS[which(is.na(dat$STARS))] <- 0

# ResidualSugar - replace with mean
dat$ResidualSugar_na <- ifelse(is.na(dat$ResidualSugar), 1, 0)
dat$ResidualSugar[which(is.na(dat$ResidualSugar))] <- mean(dat$ResidualSugar,
                                                           na.rm = TRUE)
# Chlorides - replace with mean
dat$Chlorides_na <- ifelse(is.na(dat$Chlorides), 1, 0)
dat$Chlorides[which(is.na(dat$Chlorides))] <- mean(dat$Chlorides, 
                                                   na.rm = TRUE)

# FreeSulfurDioxide - replace with mean
dat$FreeSulfurDioxide_na <- ifelse(is.na(dat$FreeSulfurDioxide), 1, 0)
dat$FreeSulfurDioxide[which(is.na(
  dat$FreeSulfurDioxide))] <- mean(dat$FreeSulfurDioxide, 
                                   na.rm = TRUE)

# TotalSulfurDioxide - replace with mean
dat$TotalSulfurDioxide_na <- ifelse(is.na(dat$TotalSulfurDioxide), 1, 0)
dat$TotalSulfurDioxide[which(is.na(
  dat$TotalSulfurDioxide))] <- mean(dat$TotalSulfurDioxide, 
                                    na.rm = TRUE)

# pH - replace with mean
dat$pH_na <- ifelse(is.na(dat$pH), 1, 0)
dat$pH[which(is.na(dat$pH))] <- mean(dat$pH, 
                                     na.rm = TRUE)

# Sulphates - replace with mean
dat$Sulphates_na <- ifelse(is.na(dat$Sulphates), 1, 0)
dat$Sulphates[which(is.na(dat$Sulphates))] <- mean(dat$Sulphates,
                                                   na.rm = TRUE)
# Alcohol - replace with mean
dat$Alcohol_na <- ifelse(is.na(dat$Alcohol), 1, 0)
dat$Alcohol[which(is.na(dat$Alcohol))] <- mean(dat$Alcohol,
                                               na.rm = TRUE)

# Row count remains the same and NA count is now zero
print(paste0("There are still ", nrow(dat), " rows"))
print(paste0("There are ", sum(is.na(dat)), " NA values"))



# Correlation After Cleaning -------------------
# A new look at the corrplot reveals only correlation with the missing values
# of STARS. Feature engineering will be needed to create better predictors.
corrplot::corrplot(cor(dat))



# Feature Engineering ---------------
# Only 2 variables have significant correlation so feature engineering 
# will be very important for the success of this prediction model. 

# STARS by LabelAppeal reveals higher LabelAppeal indicates higher STARS
dat[, .(Avg_STARS_by_Label = mean(STARS)), by = LabelAppeal]
dat$High_Label_and_STARS <- ifelse(
  dat$STARS > 2 & dat$LabelAppeal == 2, 1, 0) # cor 0.07

# AcidIndex flagged for value higher than 8
dat[, .(c = mean(AcidIndex)), by = Purchase] # cut off looks to be 8
dat$AcidIndex_8plus <- ifelse(dat$AcidIndex > 8, 1, 0) #  cor -.24

# Average AcidIndex by STARS
dat[, Avg_AcidIndex_by_STARS := mean(AcidIndex), by = STARS] # cor -0.59

# Another look at the corrplot shows the new variables are promising
corrplot::corrplot(cor(dat))



# Collinearity Check ---------------------------------
dat_cor <- cor(dat)

# Find high collinearity and identify the lower mean to eliminate
high_cor <- colnames(dat)[findCorrelation(dat_cor, 
                                          cutoff = 0.9, 
                                          verbose = TRUE)]

# Eliminate Avg_AcidIndex_by_STARS to relieve multicollinearity
dat[, Avg_AcidIndex_by_STARS := NULL]


# Train Test Split 70/30 ----------------------
set.seed(123)
split_idx <- createDataPartition(dat$Purchase, p = 0.7, list = F)
train <- dat[split_idx,]
test <- dat[-split_idx,]



# Automative Feature Selection and Model ------------------------

# Prior to forward, backward, and stepwise selection, establish Full Model,
# Intercept Model, and Base Model

full_model <- glm(Purchase ~ ., data = train, family = binomial)
intercept_model <- glm(Purchase ~ 1, data = train, family = binomial)
base_model <- glm(Purchase ~ STARS, data = train, family = binomial)

# Run forward selection
forward <-
  stepAIC(
    object = intercept_model,
    scope = list(upper = formula(full_model), lower =  ~ 1),
    direction = c('forward')
    )
forward_summ <- summary(forward)
forward_vif <- sort(vif(forward), decreasing=TRUE)

# Run backward selection
backward <- 
  stepAIC(
    object = full_model, 
    direction = c('backward')
    )
backward_summ <- summary(backward)
backward_vif <- sort(vif(backward), decreasing=TRUE)

# Run stepwise selection
step <-
  stepAIC(
    object = base_model,
    scope = list(upper = formula(full_model), lower =  ~ 1),
    direction = c('both')
  )
step_summ <- summary(step)
step_vif <- sort(vif(step), decreasing=TRUE)



# Model Summaries --------------------------
# All three selection methods chose the same variables, but backward selection
# chose different coefficients. The biggest difference is in the STARS_na flag
# where the process chose a large coefficient for FreeSulfurDioxide in place of
# a large STARS_na coefficient. 
formattable(round(cbind.data.frame(
  "Forward Selection" = forward$coefficients,
  "Backward Selection" = backward$coefficients,
  "Stepwise Selection" = step$coefficients), 2))

# All three selection methods resulted in the same VIF and all values are
# below 6. Multicollinearity is not an issue.
formattable(round(cbind(forward_vif, backward_vif, step_vif), 2))

# All three selection methods have the same AIC and BIC. The BIC is lower
# expectedly because it penalizes models with more variables.
formattable(cbind.data.frame(
  "Selection" = c("Forward", "Backward", "Stepwise"),
  "AIC" = round(c(AIC(forward), AIC(backward), AIC(step)), 2), 
  "BIC" = round(c(BIC(forward), BIC(backward), BIC(step)), 2)))



# Stepwise Predictions and Results Prior to Adjustment --------------------------
# Training accuracy
train_prob <- step %>% predict(train, type = "response")
train_pred <- ifelse(train_prob > 0.5, 1, 0)
train_res <- table(train$Purchase, train_pred, dnn = c("Purchase", "Predict"))

training_accuracy <- sum(diag(train_res)) / sum(train_res) # 0.8621

# Test accuracy
test_prob <- step %>% predict(newdata = test, type = "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)
test_res <- table(test$Purchase, test_pred, dnn = c("Purchase", "Predict"))

test_accuracy <- sum(diag(test_res)) / sum(test_res) #  0.8575

# The ROC confirms a weak prediction performace
pred <- prediction(as.numeric(test_pred), as.numeric(test$Purchase))
perf <- performance(pred, "tpr","fpr")
auc <- performance(pred, 'auc')@y.values[[1]]
plot(perf, lwd = 3, main = "Stepwise Model ROC")
text(0.6, 0.4, round(auc, 3))



# Adjusting for Model Interpretability ----------
# More variables are removed to increase interpretability. The final model
# only uses variables that do not have NA values (except STARS) which avoid
# address data source issues for use in production. The final model has only 5
# variables but 84% accuracy.
final <- glm(Purchase ~ STARS + AcidIndex + LabelAppeal +
               STARS_na + VolatileAcidity, data = train)
final_summ <- summary(final)
final_vif <- sort(vif(final), decreasing=TRUE)

# Results of Final Model
test_prob_final <- final %>% predict(newdata = test, type = "response")
test_pred_final <- ifelse(test_prob_final > 0.5, 1, 0)
test_res_final <- table(test$Purchase, test_pred_final,
                        dnn = c("Purchase", "Predict"))

test_accuracy_final <- sum(diag(test_res_final)) / sum(test_res_final) # 0.8426

# The ROC confirms a weak prediction performace
pred_final <- prediction(as.numeric(test_pred_final), as.numeric(test$Purchase))
perf_final <- performance(pred_final, "tpr","fpr")
auc_final <- performance(pred_final, 'auc')@y.values[[1]]
plot(perf_final, lwd = 3, main = "Final Model ROC")
text(0.6, 0.4, round(auc_final, 3))



# Influential Points ------------------------------

# There are no concerning influential points. The three highest Cook's D are
# small values. 
plot(final, which = 4)

model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
resid <- model.data %>% 
  top_n(3, .cooksd)
resid$.cooksd


# Interpretation, Accuracy, ROC AUC -------------------

# The change in odds of a purchase 
formattable(round(exp(cbind.data.frame(coef(final), confint(final))), 2))

# The final model lowers false positive rate but loses true positive rate.
# The area under the curve improves by 0.018.
par(mfrow = c(1, 2))
plot(perf_final, lwd = 3, main = "Final Model ROC")
text(0.6, 0.4, round(auc_final, 3))
plot(perf, lwd = 3, main = "Stepwise Model ROC")
text(0.6, 0.4, round(auc, 3))

res <- cbind.data.frame(
  " " = c("Accuracy",
          "ROC AUC",
          "AIC",
          "Variable Count",
          "Highest VIF"),
  "Training" = c(round(training_accuracy, 3), 
                 "-", 
                 "-", 
                 "-", 
                 "-"),
  "Stepwise Model" = round(c(test_accuracy,
                             auc,
                             AIC(step), 
                             (length(step$coefficients) - 1),
                             max(vif(step))),
                           3),
  "Final Model" = round(c(test_accuracy_final,
                          auc_final,
                          AIC(final),
                          (length(final$coefficients) - 1),
                          max(vif(final))),
                        3))
formattable(res)

