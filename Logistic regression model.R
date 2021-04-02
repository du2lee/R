# install.packages(c("dplyr", "ggplot2", "ISLR", "MASS", "glmnet", "randomForest", "gbm", "rpart", "boot"))


binomial_deviance <-function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)  # 0 <= yhat <= 1
  yhat = ifelse(yhat < 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a+b))
}

adult <- read.csv("/Users/duhuimac/adult.data", header = FALSE, strip.white = TRUE)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'wage')

#adult[sapply(adult, is.character)] <- lapply(adult[sapply(adult, is.character)], as.factor) #char to factor

levels(adult$wage) # They have 1, 2 each, Numerically. So, they're changed 0, 1 each.

#about model matrix
levels(adult$wage)
adult$wage[1:5]
levels(adult$sex)
adult$sex[1:5]
x <- model.matrix(~ race + sex + age, adult) # race, sex, age
colnames(x) # 7, because race's level = 5 , sex' level = 2, age :수치형 and intercept
x <- model.matrix( ~ . - wage, adult) # all
dim(x) # 101

#about divide training, validation, test set
set.seed(1601)  # because of reproducible.
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n* .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n* .20)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- adult[training_idx,]
validation <- adult[validate_idx,]
test <- adult[test_idx,]

#visualzation
training %>% ggplot(aes(age, fill=wage)) + geom_density(alpha=.5)  # age and wage
# age, race, sex and wage
training %>% filter(race %in% c('Black','White')) %>% ggplot(aes(age, fill=wage)) + geom_density(alpha=.5) + ylim(0, 0.1) + facet_grid(race ~ sex, scales = 'free_y')
training %>% ggplot(aes(education_num, fill=wage)) + geom_bar()  # education and wage

#logistic use glm()
ad_glm_full <- glm(wage ~ ., data=training, family = binomial)  #Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred -> lots of x than amount of data
summary(ad_glm_full)  #2 not defined because of singularities -> because of education_num, occupation Transport-moving is collinear
alias(ad_glm_full) #about collinearity 
predict(ad_glm_full, newdata = adult[1:5,], type = "response") # predict
#x, y of validation set
y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_lm <- predict(ad_glm_full, newdata=validation, type='response')
library(gridExtra)  #visualzation
p1 <- ggplot(data.frame(y_obs, yhat_lm), aes(y_obs, yhat_lm, group=y_obs, fill=factor(y_obs))) + geom_boxplot()
p2 <- ggplot(data.frame(y_obs, yhat_lm), aes(yhat_lm, fill=factor(y_obs))) + geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)
binomial_deviance(y_obs, yhat_lm)  #calculate 이항편차(accuracy of predict)
#ROC Curve
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]
