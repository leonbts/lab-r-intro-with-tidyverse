library(tidyverse)
employees <- data.frame(
  ID = 1:6,
  Name = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank"),
  Age = c(25, 30, 35, 40, 45, 50),
  Department = c("HR", "IT", "Finance", "IT", "HR", "Finance"),
  Salary = c(50000, 60000, 70000, 80000, 55000, 75000)
)

str(employees)
summary(employees)

# 1.
it_employees <- employees %>% 
  filter(Department == "IT")

# 2.
name_salary <- employees %>% 
  select(Name, Salary)

# 3.
employees_with_bonus <- employees %>%
  mutate(Bonus = Salary * 0.10)

# 4.
sorted_employees <- employees %>%
  arrange(desc(Salary))

# 5.
avg_salary_by_department <- employees %>%
  group_by(Department) %>%
  summarise(Average_Salary = mean(Salary))

print(it_employees)
print(name_salary)
print(employees_with_bonus)
print(sorted_employees)
print(avg_salary_by_department)


# Extra 1.1
total_salary_by_department <- employees %>%
  group_by(Department) %>%
  summarise(Total_Salary_Expenditure = sum(Salary))

# Extra 1.2
experienced_employees <- employees %>%
  filter(Age > 30) %>%
  mutate(Experience = Age - 25)

print(total_salary_by_department)
print(experienced_employees)


# Extra 2.1
hr_employees <- employees %>%
  filter(Department == "HR") %>%
  mutate(Bonus = Salary * 0.10) %>%
  arrange(desc(Bonus))

print(hr_employees)

# Extra 2.2
salary_expenditure <- employees %>%
  group_by(Department) %>%
  summarise(Total_Salary_Expenditure = sum(Salary))

print(
  ggplot(salary_expenditure, aes(x = Department, y = Total_Salary_Expenditure, fill = Department)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Total Salary Expenditure by Department",
         x = "Department",
         y = "Total Salary Expenditure") +
    theme(legend.position = "none")
)