# 1. Perform the following operations using only the dplyr library.

install.packages("dplyr")
library(dplyr)

# 2. We will use the mtcars dataframe for this exercise!

df = mtcars
head(df, 6)


# 3.  Return rows of cars that have an mpg value greater than 20 and 6 cylinders.

result = df %>%
  filter(mpg > 20 & cyl == 6)
print(result)

# 4. Reorder the Data Frame by cyl first, then by descending wt.

reordered_mtcars = df %>%
  arrange(cyl, desc(wt))
print(reordered_mtcars)

# 5 . Select the columns mpg and hp

selected_columns = df %>%
  select(mpg, hp)
head(selected_columns, 6)

# 6. Select the distinct values of the gear column.

distinct_gear = df %>%
  distinct(gear)
print(distinct_gear)


# 7. Create a new column called "Performance" which is calculated by hp divided by wt.

mtcars_with_performance = df %>%
  mutate(Performance = hp / wt)
head(mtcars_with_performance, 6)

# 8. Find the mean mpg value using dplyr.

avg_mpg = df %>%
  summarize(avg_mpg = mean(mpg))
print(avg_mpg)

# 9. Use pipe operators to get the mean hp value for cars with 6 cylinders.

std_hp = df %>%
  filter(cyl == 6) %>%
  summarise(std_hp = mean(hp))
print(std_hp)