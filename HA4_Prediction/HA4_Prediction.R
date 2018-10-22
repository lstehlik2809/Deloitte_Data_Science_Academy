# HA4_PREDICTION ------------------------------------------------------------------------------
# created by Ludek Stehlik

# Task description:
# Train your own unrest prediction model on p04.resampled dataset. Use observations 
# from 2015-01-01 on as testing dataset. Use binning and weight of evidence.Evaluate 
# performance of the model by calculating gini index, ROC curve and marginal 
# lift chart on testing dataset. Describe predictors in your model using binning plots.


# PREPARATORY WORK ----------------------------------------------------------------------------

# Basic set up of the working environment
rm(list=ls())
setwd("F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction")

# Uploading all necessary libraries
source("shared_functions.R")
library(tidyverse)
library(gridExtra)
library(skimr)
library(GGally)
library(recipes)
library(scorecard)
library(caret)
library(caretEnsemble)
library(gbm)
library(randomForest)
library(xgboost)
library(ROCR)
library(plotROC)

# Uploading data from the database
data <- sql_db("select * from p03.target_table")
# saveRDS(data, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/data/data.rds") 

# Reviewing available data
glimpse(data)
summary(data)

# Removing irrelevant variables and filtering out rows with missing target
mydata <- data %>%
  select(-c(ActionGeoCountryCode)) %>%
  filter(!(is.na(flag_target))) %>%
  mutate(observation_date = as.Date(observation_date))

# Creating training/testing dataset
train <- mydata %>%
  filter(observation_date < "2015-01-01") %>%
  select(-observation_date)

test <- mydata %>%
  filter(observation_date >= "2015-01-01") %>%
  select(-observation_date)

# Checking distribution of the target in training/testing dataset
train_stat <- train %>%
  summarise(n = n(), 
            n_target = sum(flag_target), 
            non_target = sum(flag_target == 0),
            target_rate = mean(flag_target))
train_stat
# write.table(train_stat, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/data/train_stat.txt", quote=F)

test_stat <- test %>%
  summarise(n = n(), 
            n_target = sum(flag_target),
            non_target = sum(flag_target == 0),
            target_rate = mean(flag_target))
test_stat
# write.table(test_stat, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/data/test_stat.txt", quote=F)

# EDA ------------------------------------------------------------------------------------------ 

# Data summarization
skim(train)

# Target visualization
train %>%
  group_by(as.factor(flag_target)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = `as.factor(flag_target)`, y = p)) +
  geom_col(aes(fill = `as.factor(flag_target)`)) +
  geom_label(aes(label = round(p,2))) +
  theme(legend.title = element_blank(),
        legend.position = "") +
  labs(x = "flag_target",
       y = "Proportion")

# Feature visualization - "ratio" variables
train %>%
  mutate(flag_target = as.factor(flag_target)) %>%
  select(flag_target, contains("ratio")) %>%
  ggpairs(aes(color = flag_target), lower = "blank", legend = 1, diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Feature visualization - "n" variables
train %>%
  mutate(flag_target = as.factor(flag_target)) %>%
  select(flag_target, contains("n_")) %>%
  ggpairs(aes(color = flag_target), lower = "blank", legend = 1, diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Feature visualization - "avg" variables
train %>%
  mutate(flag_target = as.factor(flag_target)) %>%
  select(flag_target, contains("avg")) %>%
  ggpairs(aes(color = flag_target), lower = "blank", legend = 1, diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Feature visualization - remaining variables
train %>%
  mutate(flag_target = as.factor(flag_target)) %>%
  select(flag_target, days_since_last_g8, days_since_last_unrest, sumgs_article_ActorName_7D_23D) %>%
  ggpairs(aes(color = flag_target), lower = "blank", legend = 1, diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Pre-modeling correlation analysis 
train %>%
  select_if(function(col) {is.numeric(col) | is.integer(col)}) %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  as.tibble() %>%
  mutate(feature = names(.)) %>%
  select(feature, flag_target) %>%
  filter(!(feature =="flag_target")) %>%
  arrange(desc(flag_target)) %>%
  rename(correlation = flag_target) %>%
  ggplot(aes(x = reorder(feature, correlation), y = correlation)) + 
  geom_col(aes(fill = factor(ifelse(correlation >=0, "positive", "negative")))) +
  geom_label(aes(label = round(correlation,2))) +
  coord_flip() +
  labs(y = "Spearman correlation with the target", 
       x = "Predictors") +
  theme(legend.position="bottom") + 
  theme(legend.title=element_blank())


# FEATURE ENGINEERING -----------------------------------------------------------------------

# Filtering out zero and near zero variance predictors
recipe_obj <- recipe(flag_target ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  prep()

train_reduced <- bake(recipe_obj, newdata = train)
test_reduced <- bake(recipe_obj, newdata = test)

# Finding optimal binning for selected predictors using tree method
bins <- train_reduced %>%
  woebin(y = "flag_target", min_perc_coarse_bin = 0.1, method = "tree", print_step = 0)

# Plotting IV of binned variables
n <- ncol(train_reduced)-1
iv <- rep(0,ncol(train_reduced)-1)
for (i in 1:n){
  iv[i] <- bins[[i]]$total_iv[1]
}

var <- rep(0,ncol(train_reduced)-1)
for (i in 1:n){
  var[i] <- bins[[i]]$variable[1]
}

varIV <- data.frame(Variable = var, IV = iv) 

varIV %>%
  ggplot(aes(x = reorder(Variable, IV), y = IV, fill = IV))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ylab("Information Value")+
  xlab("")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13))+
  theme(legend.position="")+
  labs(fill='Information Value')+
  scale_fill_gradient(low = "#86BC25", high = "#DA291C")

# Plotting binned predictors
woebin_plot(bins)

# Vector of rejected predictors - based on their low IV (<0.02) and/or hardly adjustable saw-shaped look
preds_rejected <- c("n_article_1D_29D", 
                    "n_article_g8_7D_23D", 
                    "ratio_event_ng_30D", 
                    "ratio_article_ng_30D", 
                    "n_article_g8_1D_29D",
                    "n_article_7D_23D",
                    "ratio_article_ng_7D",
                    "avgt_article_ng_30D",
                    "avgg_article_ng_30D",
                    "avgt_article_ng_7D",
                    "avgg_article_ng_7D")

# Manual binning adjustment
breaks_adj <- list(
  days_since_last_unrest = c(7, 14, Inf))

bins_adj <- train_reduced %>%
  select(-one_of(preds_rejected)) %>%
  woebin(y = "flag_target", min_perc_coarse_bin = 0.1, breaks_list=breaks_adj, print_step=0)
 
# Plotting final shape of binned predictors 
plotlist <- woebin_plot(bins_adj)
plotlist

# Saving binning plot
for (i in 1:length(plotlist)) {
  ggsave(
     paste0("F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/predictors/",
            names(plotlist[i]),
            ".png"),
     plotlist[[i]],
     width = 20, height = 15, units="cm" )
  }

# converting train and test into woe values
train_woe <- train_reduced %>%
  select(-one_of(preds_rejected)) %>%
  woebin_ply(bins, print_step=0) %>%
  mutate(flag_target = factor(flag_target, levels = c(1,0), labels = c("event", "non_event")))

test_woe <- test_reduced %>%
  select(-one_of(preds_rejected)) %>%
  woebin_ply(bins, print_step=0) %>%
  mutate(flag_target = factor(flag_target, levels = c(1,0), labels = c("event", "non_event")))

# MODELING -------------------------------------------------------------------------------------

# To prevent occurance of 'Error in summary.connection(connection) : invalid connection'
library("doParallel")
registerDoSEQ() 

fitControl <- trainControl(method = "cv", 
             number = 10, 
             summaryFunction = twoClassSummary,
             classProbs = TRUE, 
             verboseIter = TRUE)

# Train glmnet model
model_glmnet <- train(
  flag_target ~ .,
  data = train_woe, 
  tuneLength = 10,
  method = "glmnet",
  trControl = fitControl)
saveRDS(model_glmnet, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_glmnet.rds") 

# Train rpart model
model_rpart <- train(
  flag_target ~ .,
  data = train_woe, 
  tuneLength = 10,
  method = "rpart",
  trControl = fitControl)
saveRDS(model_rpart, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_rpart.rds") 

# Train rf model
model_rf <- train(
  flag_target ~ .,
  data = train_woe, 
  tuneLength = 10,
  method = "ranger",
  trControl = fitControl)
saveRDS(model_rf, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_rf.rds") 

# Train gbm model
model_gbm <- train(
  flag_target ~ .,
  data = train_woe, 
  tuneLength = 5,
  method = "gbm",
  trControl = fitControl)
saveRDS(model_gbm, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_gbm.rds") 

# Train xgb model
model_xgb <- train(
  flag_target ~ .,
  data = train_woe, 
  tuneLength = 5,
  method = "xgbTree",
  trControl = fitControl)
saveRDS(model_xgb, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_xgb.rds") 

# Train nnet model
model_nnet <- train(
  flag_target ~ .,
  data = train_woe, 
  tuneLength = 5,
  method = "nnet",
  trControl = fitControl)
saveRDS(model_nnet, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_nnet.rds") 

# Preparing new training dataset for stacking ensamble  
glmnet_preds_train <- predict(model_glmnet, train_woe, type = "prob") %>%
  select(event) %>%
  rename(glmnet_preds = event)

rpart_preds_train <- predict(model_rpart, train_woe, type = "prob") %>%
  select(event) %>%
  rename(rpart_preds = event)

rf_preds_train <- predict(model_rf, train_woe, type = "prob") %>%
  select(event) %>%
  rename(rf_preds = event)

gbm_preds_train <- predict(model_gbm, train_woe, type = "prob") %>%
  select(event) %>%
  rename(gbm_preds = event)

xgb_preds_train <- predict(model_xgb, train_woe, type = "prob") %>%
  select(event) %>%
  rename(xgb_preds = event)

nnet_preds_train <- predict(model_nnet, train_woe, type = "prob") %>%
  select(event) %>%
  rename(nnet_preds = event)

train_stack_data <- glmnet_preds_train %>%
  bind_cols(rpart_preds_train, rf_preds_train, gbm_preds_train, xgb_preds_train, nnet_preds_train, train_woe[1])
saveRDS(train_stack_data, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/data/train_stack_data.rds") 

# Preparing new testing dataset for stacking ensamble  
glmnet_preds_test <- predict(model_glmnet, test_woe, type = "prob") %>%
  select(event) %>%
  rename(glmnet_preds = event)

rpart_preds_test <- predict(model_rpart, test_woe, type = "prob") %>%
  select(event) %>%
  rename(rpart_preds = event)

rf_preds_test <- predict(model_rf, test_woe, type = "prob") %>%
  select(event) %>%
  rename(rf_preds = event)

gbm_preds_test <- predict(model_gbm, test_woe, type = "prob") %>%
  select(event) %>%
  rename(gbm_preds = event)

xgb_preds_test <- predict(model_xgb, test_woe, type = "prob") %>%
  select(event) %>%
  rename(xgb_preds = event)

nnet_preds_test <- predict(model_nnet, test_woe, type = "prob") %>%
  select(event) %>%
  rename(nnet_preds = event)

test_stack_data <- glmnet_preds_test %>%
  bind_cols(rpart_preds_test, rf_preds_test, gbm_preds_test, xgb_preds_test, nnet_preds_test, test_woe[1])
saveRDS(test_stack_data, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/data/test_stack_data.rds") 

# Training ensemble model 
model_ensemble <- train(
  flag_target ~ .,
  data = train_stack_data,
  method = "gbm",
  tuneLength = 5,
  trControl = fitControl)
saveRDS(model_ensemble, "F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/models/model_ensemble.rds") 


# MODEL'S PERFORMANCE EVALUATION -------------------------------------------------------------

# Generating model's predictions for training and testing data
train_preds <- predict(model_ensemble, newdata = train_stack_data, type = "prob")
test_preds <- predict(model_ensemble, newdata = test_stack_data, type = "prob")

# Plotting distribution of prediction scores grouped by known outcome for trainig and testing data
train_preds_df <- data.frame(target = train$flag_target, pred = train_preds$event)  
g1 <- ggplot(train_preds_df, aes(pred, fill = as.factor(target))) +
  geom_density(alpha = 0.4) +
  labs(title = "Training dataset",
       subtitle = "Distribution of predictions scores",
       x = "Probability of event",
       fill = "target") +
  theme(legend.position = c(0.85, 0.9))

test_preds_df <- data.frame(target = test$flag_target, pred = test_preds$event)  
g2 <- ggplot(test_preds_df, aes(pred, fill = as.factor(target))) +
  geom_density(alpha = 0.4) +
  labs(title = "Testing dataset",
       subtitle = "Distribution of predictions scores",
       x = "Probability of event",
       fill = "target") +
  theme(legend.position = c(0.85, 0.9))

grid.arrange(g1, g2, nrow = 1)

g <- arrangeGrob(g1, g2, nrow = 1) 
ggsave("F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/Scores_Distribution.png", g, width = 12, height = 8)

# Computing AUC for training and testing data
aucn <- performance(prediction(train_preds$event, train$flag_target),"auc")
aucn <- round(aucn@y.values[[1]],2)
aucn 

aucs <- performance(prediction(test_preds$event, test$flag_target),"auc")
aucs <- round(aucs@y.values[[1]],2)
aucs 

# Computing Gini for testing data
gn <- (aucn-0.5)/0.5
gn

gs <- (aucs-0.5)/0.5
gs

# Plotting ROC
ggplot() + 
  geom_roc(data = train_preds_df, aes(d = target, m = pred, color = "Train"), labels = F, pointsize = 0, alpha = 0.7) + 
  geom_roc(data = test_preds_df, aes(d = target, m = pred, color = "Test"), labels = F, pointsize = 0, alpha = 0.7) +
  geom_abline(aes(intercept = 0, slope = 1, color = "Random guess")) +
  labs(x = "False positive rate",
       y = "True positive rate",
       color = "",
       title = "ROC Curve") +
  theme(legend.position = "bottom") +
  annotate("label", label = paste0("Gini_test = ", round(gs,2),
                                   "\nGini_train = ", round(gn,2),
                                   "\nAUC_test = ", round(aucs,2),
                                   "\nAUC_train = ", round(aucn,2)),
           x = 0.9, y = 0.1, color = "black") +
  scale_color_manual(values=c("Test" = "#ED8B00", "Train" = "#012169", "Random guess" = "darkgrey"))

ggsave("F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/ROC.png", width = 6, height = 6)

# Plotting non/cumulative Lift charts for training and testing data
train_lift <- train_preds_df %>%
  mutate(quantile = ntile(pred, 10)) %>%
  group_by(quantile) %>%
  summarise(n = n(), t = sum(target)) %>%
  ungroup() %>%
  mutate(p = t/n, quantile = 11-quantile) %>%
  mutate(ncl = p/(sum(t)/sum(n))) %>%
  arrange(quantile) %>%
  mutate(n_cumsum = cumsum(n), t_cumsum = cumsum(t)) %>%
  mutate(cl = t_cumsum/n_cumsum/(sum(t)/sum(n)))

test_lift <- test_preds_df %>%
  mutate(quantile = ntile(pred, 10)) %>%
  group_by(quantile) %>%
  summarise(n = n(), t = sum(target)) %>%
  ungroup() %>%
  mutate(p = t/n, quantile = 11-quantile) %>%
  mutate(ncl = p/(sum(t)/sum(n))) %>%
  arrange(quantile) %>%
  mutate(n_cumsum = cumsum(n), t_cumsum = cumsum(t)) %>%
  mutate(cl = t_cumsum/n_cumsum/(sum(t)/sum(n)))

# Non-Cumulative Lift Charts
ggplot() +
  geom_point(data = train_lift, aes(x = quantile, y = ncl, color = "Train"), size = 3, alpha = 0.7) +
  geom_line(data = train_lift, aes(x = quantile, y = ncl, color = "Train"), linetype = "longdash", alpha = 0.7) +
 
  geom_point(data = test_lift, aes(x = quantile, y = ncl, color = "Test"), size = 3, alpha = 0.7) +
  geom_line(data = test_lift, aes(x = quantile, y = ncl, color = "Test"), linetype = "longdash", alpha = 0.7) +
  
  scale_x_continuous(limits = c(1,10), breaks = seq(1,10,1)) +
  scale_y_continuous(limits = c(0,max(max(train_lift$ncl), max(test_lift$ncl))+0.25), breaks = seq(0,max(max(train_lift$ncl), max(test_lift$ncl))+0.25,0.25)) +
  labs(x = "Deciles",
       y = "Non-cummulative lift",
       color = "",
       title = "Non-Cummulative Lift Chart") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  geom_abline(aes(intercept = 1, slope = 0, color = "Baseline")) +
  scale_color_manual(values=c("Test" = "#ED8B00", "Train" = "#012169", "Baseline" = "darkgrey"))

ggsave("F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/NC_Lift_Chart.png", width = 12, height = 8)

# Cumulative Lift Charts
ggplot() +
  geom_point(data = train_lift, aes(x = quantile, y = cl, color = "Train"), size = 3, alpha = 0.7) +
  geom_line(data = train_lift, aes(x = quantile, y = cl, color = "Train"), linetype = "longdash", alpha = 0.7) +
  
  geom_point(data = test_lift, aes(x = quantile, y = cl, color = "Test"), size = 3, alpha = 0.7) +
  geom_line(data = test_lift, aes(x = quantile, y = cl, color = "Test"), linetype = "longdash", alpha = 0.7) +
  
  scale_x_continuous(limits = c(1,10), breaks = seq(1,10,1)) +
  scale_y_continuous(limits = c(0.95,max(max(train_lift$cl), max(test_lift$cl))+0.25), breaks = seq(1,max(max(train_lift$cl), max(test_lift$cl))+0.25,0.25)) +
  labs(x = "Deciles",
       y = "Cummulative lift",
       color = "",
       title = "Cummulative Lift Chart") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  geom_abline(aes(intercept = 1, slope = 0, color = "Baseline")) +
  scale_color_manual(values=c("Test" = "#ED8B00", "Train" = "#012169", "Baseline" = "darkgrey"))

ggsave("F:/space_aa_420/01_projects/2018/06_DSA_GDELT/trainees/com-lstehlik/HA4_Prediction/outputs/C_Lift_Chart.png", width = 12, height = 8)



