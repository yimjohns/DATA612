library(tidyverse)
library(dplyr)


# ======================================================================
# Question 1
# ======================================================================
TopColleges2019 <- read_csv("HW6/ForbesAmericasTopColleges2019.csv")



# ======================================================================
# Question 2
# ======================================================================
TopColleges2019 %>%
  filter(Name %in% 
           c("Howard University",
             "American University",
             "George Washington University")) %>%
  select(Name, Rank) %>%
  arrange(desc(Rank))


# ======================================================================
# Question 3
# ======================================================================
TopColleges2019 %>%
  slice(384:394) %>%
  select(Name, Rank)


# ======================================================================
# Question 4
# ======================================================================
TopColleges2019 %>%
  filter(Rank >= 384, Rank <= 394) %>%
  select(Name, Rank)


# ======================================================================
# Question 5
# ======================================================================
TM <- tribble(
  ~Name, ~Gender, ~Weight, ~Height, ~Age, ~EducationLevel,
  #-----|-------|---------|-------|-------|---------------
  "Leon", "Male",    202, "6ft0in",  30,     "BS",
  "Mary", "Female",  140, "5ft4in",  28,     "BS",
  "Alice","Female",  133, "5ft7in",  34,     "MS",
  "Ralph", "Male",   188, "6ft2in",  31,     "MS",
  "Brenda","Female", 176, "5ft8in",  27,     "BS"
)


TM

RC <- read_csv("Name,Gender,Weight,Height,Age,EducationLevel 
      Leon,Male,202,6ft0in,30,BS
      Mary,Female,140,5ft4in,28,BS
       Alice,Female,133,5ft7in,34,MS
      Ralph,Male,188,6ft2in,31,MS
      Brenda,Female,176,5ft8in,27,BS")
RC


read_csv("Name,Gender,Weight,Height,Age,EducationLevel\nLeon,Male,202,6ft0in,30,BS\n
      Mary,Female,140,5ft4in,28,BS\nAlice,Female,133,5ft7in,34,MS\n
      Ralph,Male,188,6ft2in,31,MS\nBrenda,Female,176,5ft8in,27,BS") -> NL
NL
