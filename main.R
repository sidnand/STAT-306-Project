library(dplyr)

data <- read.csv("./data.csv")

df <- data %>% select(Customer_Age, Credit_Limit, Income_Category)

df$Income_Category = as.factor(df$Income_Category)
df <- within(df, Income_Category <- relevel(Income_Category, ref = "Unknown"))

sum_df <- summary(df)

model1_noTrans = lm(Credit_Limit ~ Customer_Age, data = df)

model2_noTrans = lm(Credit_Limit ~ Customer_Age + Income_Category, data = df)

model3_noTrans = lm(Credit_Limit ~ Customer_Age + Income_Category + Customer_Age*Income_Category, data = df)

model1 <- lm(tan(Credit_Limit) ~ I(Customer_Age), data = df)
sum_model1 <- summary(model1)
#sum_model1

model2 <- lm(log(Credit_Limit) ~ I(Customer_Age) + I(Income_Category), data = df)
sum_model2 <- summary(model2)
# sum_model2

model3 <- lm(log(Credit_Limit) ~ I(Customer_Age) + I(Income_Category) + I(Income_Category) * I(Customer_Age), data = df)
sum_model3 <- summary(model3)
#sum_model3

#plot(model1_noTrans, lwd = 4, main = "Model 1")
#plot(model2_noTrans, lwd = 4, main = "Model 2")
# plot(model3_noTrans, lwd = 4, main = "Model 3")

#plot(model1, lwd = 4, main = "Model 1")
#plot(model2, lwd = 4, main = "Model 2")
# plot(model3, lwd = 4, main = "Model 3")





