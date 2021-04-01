# install.packages(c("dplyr", "ggplot2", "ISLR", "MASS", "glmnet", "randomForest", "gbm", "rpart", "boot"))


binamial_deviance <-function(y_obs, yhat){
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


