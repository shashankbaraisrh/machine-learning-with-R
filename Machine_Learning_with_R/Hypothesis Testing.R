# Hypothesis testing exercises


# 1. Use the built-in 'iris' dataset in R.

data(iris)
head(iris)

# 2. Save the data of the 'Petal.Width' column into a new column called 'x'.

iris$x <- iris$Petal.Width
head(iris)

# 3. Calculate the mean and standard deviation of 'x'.

mean_x <- mean(iris$x)
cat("Mean of 'x' (Petal.Width):", mean_x, "\n")
sd_x <- sd(iris$x)
cat("Standard Deviation of 'x' (Petal.Width):", sd_x, "\n")


# 4. Perform a t-test on 'x' assuming a population mean (mu) of 1.3.

hypothesized_mean <- 1.3
t_test_result <- t.test(iris$x, mu = hypothesized_mean)
print(t_test_result)


# 5. Evaluate whether to accept or reject the null hypothesis. This null hypothesis assumes that there is no significant difference or effect, indicating that the true mean 'Petal.Width' in the population is 1.3.

alpha <- 0.05
p_value <- t_test_result$p.value

if (p_value <= alpha) {
  cat("Reject the null hypothesis. There is significant evidence that the true mean Petal.Width is different from 1.3.")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that the true mean Petal.Width is different from 1.3.")
}
cat("\nP-value:", p_value)



# 6. Using R, conduct a one-sample t-test to assess if the mean 'Petal.Width' of the iris dataset is less than 1.3. Perform the test and interpret the results, focusing on the directionality of the test.

t_test_result_less <- t.test(iris$Petal.Width, mu = 1.3, alternative = "less")
print(t_test_result_less)

# 7. Exercise Set2: Utilize the 'mtcars' dataset available in R.

data(mtcars)
head(mtcars)

# 8. Create a variable 'x' and store the 'mpg' data within it.

x <- mtcars$mpg
head(x)

# 9. Generate a histogram of 'x' using 'mpg'.

hist(x, 
     xlab = 'Miles/(US) gallon', ylab = 'Frequency', 
     main = 'Histogram of MPG')

# 10. Create a boxplot using 'x' as input.

boxplot(x, 
        xlab = 'Miles/(US) gallon', 
        main = 'Boxplot of MPG')


# 11. Calculate the mean and standard deviation of 'x'.

mean_x <- mean(x)
cat("Mean of 'x' (mpg):", mean_x, "\n")
sd_x <- sd(x)
cat("Standard Deviation of 'x' (mpg):", sd_x, "\n")


# 12. Perform a t-test assuming a population mean (mu) of 20.

hypothesized_mean <- 20
t_test_result <- t.test(x, mu = hypothesized_mean)
print(t_test_result)

# 13. Evaluate whether to accept or reject the null hypothesis.


alpha <- 0.05
p_value <- t_test_result$p.value

if (p_value <= alpha) {
  cat("Reject the null hypothesis. There is significant evidence that the true mean 'mpg' is different from 20.")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that the true mean 'mpg' is different from 20.")
}

cat("\nP-value:", p_value)

# 14. Exercise Set 3: Use the 'PlantGrowth' dataset available in R (contains data on the growth of plants treated with different fertilizers).

#PlantGrowth
data("PlantGrowth")
head(PlantGrowth,22)
str(PlantGrowth)
# 15. Perform a one-way ANOVA test comparing 'weight' across different 'group' categories.

anova_result <- aov(weight ~ group, data = PlantGrowth)
summary(anova_result)

# 16. Create a boxplot to show variance in the mean within and between groups.

boxplot(weight ~ group, data = PlantGrowth,
        xlab = 'Treatment Group', ylab = 'Weight',
        main = 'Boxplot of Plant Growth by Treatment')

means <- tapply(PlantGrowth$weight, PlantGrowth$group, mean)
points(1:3, means, col = 'blue', pch = 18)

# 17. Assess whether to accept or reject the null hypothesis based on the ANOVA test results.

anova_summary <- summary(anova_result)

p_value <- anova_summary[[1]]$`Pr(>F)`[1]

alpha <- 0.05

if (p_value <= alpha) {
  cat("Reject the null hypothesis. There is significant evidence that at least one group mean is different.")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that there are differences in group means.")
}

cat("\nP-value:", p_value)















