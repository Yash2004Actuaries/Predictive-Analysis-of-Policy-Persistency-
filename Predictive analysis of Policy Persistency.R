# Predictive analysis of Policy Persistency

# Install Package named "Tidyverse"
install.packages("tidyverse")
library(tidyverse)

# Setting a specific seed value, to generate same random variables
set.seed(123)
# Creating a Portfolio of 1000 policies in start
n <- 1000 

# Creating a data frame named insurance_data which includes Policy ID,
# Age, Annual Income, Premium Frequency and Policy Tenure
insurance_data <- data.frame(
  Policy_ID = 1:n,
  Age = sample(18:65, n, replace = TRUE),
  Annual_Income = round(runif(n, 300000, 2000000)),
  Premium_Frequency = sample(c("Annual", "Monthly", "Quarterly"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  Policy_Tenure = sample(1:10, n, replace = TRUE)
)

# Generating first 5 rows of insurance data
head(insurance_data , 5)

# Designing Lapse rate based on certain assumptions
insurance_data$Lapse_rate <- 0.1 + ifelse(insurance_data$Premium == "Monthly" , 0.3 , 0) + ifelse(insurance_data$Age < 30 , 0.2 , 0) - insurance_data$Policy_Tenure*0.02
head(insurance_data , 5)

# Generating policy status 
# 1 signifies Policy is inactive
# 0 signifies Policy is active
insurance_data$Policy_status <- rep(0,n)
for(i in 1:n){
  insurance_data$Policy_status[i] <- ifelse(insurance_data$Lapse_rate < runif(1) , 0 , 1)
}

# Generating first 5 rows of updated insurance data
head(insurance_data , 5)

# GGPLOT of Proportion of Policies against Premium Payment Frequency
ggplot(insurance_data, aes(x = Premium_Frequency, fill = as.factor(Policy_status))) +
  geom_bar(position = "fill") +
  labs(title = "Impact of Premium Frequency on Policy Lapse",
       x = "Premium Payment Frequency",
       y = "Proportion of Policies",
       fill = "Status (1=Lapsed, 0=Active)") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# GGPLOT of Age of Policyholder against Policy Lapse status
ggplot(insurance_data, aes(x = as.factor(Policy_status), y = Age, fill = as.factor(Policy_status))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Lapse Status",
       x = "Lapse Status (0 = Active, 1 = Lapsed)",
       y = "Age of Policyholder") +
  theme_classic()

# Fitting GLM model to predict Policy Lapse Risk
lapse_model <- glm(Policy_status ~ Age + Premium_Frequency + Policy_Tenure, 
                   data = insurance_data, 
                   family = "binomial")

# Summary of fitted model
summary(lapse_model)

# Predict 
predict(lapse_model, n.ahead = 1)
