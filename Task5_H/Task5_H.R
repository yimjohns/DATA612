library(tidyverse)
library(dplyr)
library(ggplot2)
# install.packages("Lahman")
library(Lahman)

# =======================================================================
# Answer 1
# =======================================================================
is_tibble(Batting)


# =======================================================================
# Answer 2
# 
# Batting has 110495 rows and 22 Columns
# =======================================================================
dim(Batting)


# =======================================================================
# Answer 3
# =======================================================================
as_tibble(Batting)


# =======================================================================
# Answer 4
# 
# 12 Players hit more than 30 home runs in 1991
# =======================================================================
Batting %>%
  select(playerID, teamID, yearID, HR) %>%
  filter(HR > 30, yearID == 1991) -> HR1991
# HR1991

# Number of players that hit more than 30 home runs in 1991
# nrow(HR1991)
count(HR1991)


# =======================================================================
# Answer 5
# =======================================================================
Batting %>%
  filter(yearID == 1991) %>%
  group_by(yearID) %>%
  summarise(MeanHR1991 = mean(HR))


# =======================================================================
# Answer 6
#
# 12 Players hit more than 30 home runs in 1990
# =======================================================================
Batting %>%
  select(playerID, teamID, yearID, HR) %>%
  filter(HR > 30, yearID == 1990) -> HR1990
HR1990

# Number of players that hit more than 30 home runs in 1990
count(HR1990)


# =======================================================================
# Answer 7
# =======================================================================
Batting %>%
  filter(yearID == 1990) %>%
  group_by(yearID) %>%
  summarise(MeanHR1990 = mean(HR))


# =======================================================================
# Answer 8
# =======================================================================
Batting %>%
  filter(yearID %in% c(1990, 1991), HR > 30) %>%
  select(yearID, HR) %>%
  mutate(yearID = as_factor(yearID)) %>%
  ggplot(mapping = aes(x = yearID, y = HR)) +
  geom_boxplot(mapping = aes(fill = yearID), 
               color = "darkblue", 
               show.legend = FALSE) +
  labs(
    title = "Box plot of Players that hit more than 30 Home Runs in 1990 and 1991",
    x = "Year",
    y = "Home Run"
  )
  

# =======================================================================
# Answer 9
# =======================================================================
Batting %>%
  filter(yearID %in% c(1990, 1991), HR > 30) %>%
  select(teamID, HR) %>%
  ggplot(mapping = aes(x = teamID, y = HR)) +
  geom_boxplot()



# =======================================================================
# Answer 10
# =======================================================================
tribble (~Name,  ~Age, ~Department, ~YrsofSrvce, ~EduLevel, ~Salary,
         "Carlos", 30,  "Personnel",  4,          "MS",       71500,
         "Jacob",  26,  "Accounting", 6,          "BS",       70000,
         "Elaine",  31,    "IT",      4,          "BS",       75000,
         "Alice",  42,   "Sales",     5,          "BS",       72000,
         "Juan",   31,    "IT",       7,          "BS",       68000,
         "Ray",    28,  "Accounting", 5,          "MS",       81000,
         "Kate",   25,   "Sales",     4,          "BS",       74000,
         "Leon",   30,  "Personnel",  11,         "MS",       78000,
         "Robert", 29,  "Accounting", 8,          "MS",       77500
) -> EmployeeData
EmployeeData



# =======================================================================
# Answer 11
# =======================================================================
ggplot(data = EmployeeData, mapping = aes(x = Name, y = YrsofSrvce)) +
  geom_bar(mapping = aes(fill = Name), stat = "identity") +
  geom_text(aes(label = YrsofSrvce), vjust = 2) +
  ggtitle("Years of Service Bar Graph")
