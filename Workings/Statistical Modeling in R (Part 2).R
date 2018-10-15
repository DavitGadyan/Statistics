# library(devtools)
# 
# devtools::install_github("dtkaplan/statisticalModeling")
library(mosaicData)
library(statisticalModeling)
data(CPS85)
# Train model
wage_model <- lm(wage ~ educ + sector + sex + exper,
                   data = CPS85)
# Effect size of education on wage: a slope
effect_size(wage_model, ~ educ)

# A model of the probability of being married
married_model <- glm(married == "Married" ~ educ * sector * sex + age,
                       data = CPS85, family = "binomial")
fmodel(married_model, ~ age + sex + sector + educ, data = CPS85,
         type = "response", educ = c(10, 16))

install.packages('NHANES')
library(NHANES)
library(rpart)

#### for response (dependent) variable being numercical we use effect_size function


# Build the model
mod <- lm(Pulse ~ Height + BMI + Gender, data = NHANES)

# Confirm by reconstructing the graphic provided
fmodel(mod, ~ Height + BMI + Gender) + ggplot2::ylab("Pulse")

# Find effect size ### formula takes for the rest on independent variables median and mode.. for BMI median and...?

effect_size(mod, ~ BMI) # for independent variable in effect_size function if it is numerical functions
## outputs slope (rate of change)

effect_size(mod, ~ Gender)# for independent variable in effect_size function if it is categorical functions
## outputs change in dependant variable (rate of change)

# Replot the model
fmodel(mod, ~ BMI+Height + Gender) + ggplot2::ylab("Pulse")


#### for response (dependent) variable being categorical we use evaluate_model function but we alos may use effect_size function

# Create model and set inputs
married_model <- rpart(married ~ educ + sex + age,
                         data = CPS85, cp = 0.005)
# Output as a category (i.e. class)
evaluate_model(married_model, type = "class", age = c(25, 30),
                 educ = 12, sex = "F")

# Output as a probability
evaluate_model(married_model, type = "prob", age = c(25, 30),
                 educ = 12, sex = "F")

data(Whickham)
str(Whickham)

# An rpart model
mod1 <- rpart(outcome ~ age + smoker, data = Whickham)

# Logistic regression
mod2 <- glm(outcome == "Alive" ~ age + smoker, 
            data = Whickham, family = "binomial")

summary(mod2)
# Visualize the models with fmodel()
fmodel(mod1)
fmodel(mod2)

# Find the effect size of smoker
effect_size(mod1,~smoker)
effect_size(mod2,~smoker)


Used_Fords<-read.csv('C:\\Users\\Gaya\\Desktop\\PYTHON\\Data\\Fords.csv')

# Train model_1
model_1 <- lm(Price ~ Age + Mileage, 
              data = Used_Fords)

# Train model_2
model_2 <- lm(Price ~ Age * Mileage, 
              data = Used_Fords)

# Plot both models
fmodel(model_1)
fmodel(model_2)


# Cross validate and compare prediction errors
res <- cv_pred_error(model_1, model_2)
t<-t.test(mse ~ model, data = res)
t$null.value # Null hypothesis is difference in means = 0 ... p_value less than 0.05% >>> reject null hyposthesis..

# The interaction makes the effect size of Mileage smaller in magnitude for older cars.
# The interaction makes the effect size of Age smaller in magnitude for higher-Mileage cars. (That is, once the car has a lot of mileage, Age doesn't matter much.)
# The cross-validated prediction error for model_1 is larger than model_2 in a statistically significant way.

## evaluate_model differs from effect_size that it also provides prediction for different scenarios

# By default, evaluate_model() gives a few levels for each explanatory variable not assigned a specific value. 
# So when you compare the different scenarios, you need to be careful to choose instances where those 
# variables are the same in the output for both scenarios! As it happens, because the model has no 
# interactions, it doesn't matter which of those levels you choose when calculating the price difference.


mod3<-lm(wage ~ age + sector + sex + married * exper, 
         data = CPS85)
summary(mod3)


# Model and effect size from the "real" data
model <- lm(wage ~ age + sector, data = CPS85)
effect_size(model, ~ age)

# Generate 10 resampling trials
my_trials <- ensemble(model, nreps = 10)

# Find the effect size for each trial
effect_size(my_trials, ~ age)

# Re-do with 100 trials
my_trials <- ensemble(model, nreps = 100)
trial_effect_sizes <- effect_size(my_trials, ~ age)

# Calculate the standard deviation of the 100 effect sizes
sd(trial_effect_sizes$slope)

# Model and effect size from the "real" data
model <- lm(wage ~ age + sector, data = CPS85)
effect_size(model, ~ age)

# Generate 10 resampling trials
my_trials <- ensemble(model, nreps = 10)

# Find the effect size for each trial
effect_size(my_trials, ~ age)

# Re-do with 100 trials
my_trials <- ensemble(model, nreps = 100)
trial_effect_sizes <- effect_size(my_trials, ~ age)

# Calculate the standard deviation of the 100 effect sizes
sd(trial_effect_sizes$slope)


# A model of logarithmically transformed price
model <- lm(log(Price) ~ Mileage + Age, data = Used_Fords)

# Create the bootstrap replications
bootstrap_reps <- ensemble(model, nreps = 100, data = Used_Fords)

# Find the effect size
age_effect <- effect_size(bootstrap_reps, ~ Age)

# Change the slope to a percent change
age_effect$percent_change <- 100 * (exp(age_effect$slope) - 1)

# Find confidence interval
with(age_effect, mean(percent_change) + c(-2, 2) * sd(percent_change))


# A model of wage
model_1 <- lm(wage ~ educ + sector + exper + age, data = CPS85)

# Effect size of educ on wage
effect_size(model_1, ~ educ)

# Examine confidence interval on effect size
ensemble_1 <- ensemble(model_1, nreps = 100)
effect_from_1 <- suppressWarnings(effect_size(ensemble_1, ~ educ))
with(effect_from_1, mean(slope) + c(-2, 2) * sd(slope))

# Collinearity inflation factor on standard error
collinearity( ~ educ + sector + exper + age, data = CPS85)

# Leave out covariates one at a time
collinearity( ~ educ + sector + exper, data = CPS85) # leave out age
collinearity( ~ educ + sector + age, data = CPS85) # leave out exper
collinearity( ~ educ + exper + age, data = CPS85) # leave out sector


# Improved model leaving out worst offending covariate
model_2 <- lm(wage ~ educ + sector + age, data = CPS85)

# Confidence interval of effect size of educ on wage
ensemble_2 <- ensemble(model_2, nreps = 100)
effect_from_2 <- effect_size(ensemble_2, ~ educ)
with(effect_from_2, mean(slope) + c(-2, 2) * sd(slope))


# Train a model Price ~ Age + Mileage
model_1 <- lm(Price ~ Age + Mileage, data = Used_Fords)

# Train a similar model including the interaction
model_2 <- lm(Price ~ Age * Mileage, data = Used_Fords)

# Compare cross-validated prediction error
cv_pred_error(model_1, model_2)

# Use bootstrapping to find conf. interval on effect size of Age  
ensemble_1 <- ensemble(model_1, nreps = 100)
ensemble_2 <- ensemble(model_2, nreps = 100)
effect_from_1 <- effect_size(ensemble_1, ~ Age)
effect_from_2 <- effect_size(ensemble_2, ~ Age)
with(effect_from_1, mean(slope) + c(-2, 2) * sd(slope))
with(effect_from_2, mean(slope) + c(-2, 2) * sd(slope))

# Compare inflation for the model with and without interaction
collinearity(~ Age + Mileage, data = Used_Fords)
collinearity(~ Age * Mileage, data = Used_Fords)