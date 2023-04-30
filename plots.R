library(ggplot2)
library(ggpubr)
library(scatterplot3d)

dat <- read.csv("./data.csv")

mod1 <- lm(dat$Credit_Limit ~ dat$Customer_Age)

plot(dat$Customer_Age, dat$Credit_Limit, main = "(Model 1) Customer Age vs Credit Limit", xlab = "Age", ylab = "Credit Limit", font.main = 1)
abline(mod1, col = "red")

mod2 <- lm(dat$Credit_Limit ~ dat$Customer_Age + dat$Income_Category)
# 
# Plot a simple linear model with two categories

ggplot(data = dat, mapping = aes(x = Customer_Age,
                                 y = Credit_Limit,
                                 color = Income_Category,
                                 group = Income_Category)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "lm", se=F) +
    labs(
        title = "(Model 2) Customer Age per Income Category vs Credit Limit",
        color = "Income Category",
        x = "Age",
        y = "Credit Limit"
    ) +
    theme_bw()