
# Tidying Data
library(tidyverse)
library(tidyr)
library(dplyr)

# Let's re-visit table 4a which is untidy for the column headings "1999"
#and "2000" are not variables but values of a variable.  
table4a
 
# We obtain a much more tidy representation of the data by applying the
# the function pivot_longer and argument designations cols, names_to and
# values_to.  Consider and run the following code.

pivot_longer(table4a, cols =c( "1999", "2000"),  names_to = "year",
             values_to = "cases")


# Let's re-visit table 2 which is untidy for each observation is spread
# across two rows.
table2

# Note for example that the observation Afghanistan and 1999 are  
# represented in two rows.

# We obtain a much more tidy representation of the data by applying the
# the function pivot_wider and argument designations names_from and 
# values _from.  Consider and run the following code.

pivot_wider(table2, names_from = type
            , values_from = count)

# Note that the argument names_from was applied to the column heading, 
# type for it contained variable names  (cases and population).

#Separate
# separate() pulls apart one column into multiple columns, by splitting 
# wherever a separator character appears.  Let's look at table 3
table3

# We will produce a more tidy data set representation by separating the
# the variable rate into columns cases and population.  Consider and run
# the following code.

separate(table3, rate, into = c("cases" , "population"))

# A way to specify the delimiter
separate(table3, rate, into = c("cases", "population"), sep = "/")

# Now consider a minor improvement on the previous code. Note that the
# output table indicates that the variables year, cases, and popula-
# tion are character columns.  We will change these designations so
# that columns will reflect integer output.

separate(table3, rate, into = c("cases" , "population"), convert =
           TRUE)


# Unite

# unite() is the inverse of separate(), it combines columns into a 
# single column.

# We will use table 5 to illustrate the usage of Unite.  Consider table
# 5

table5

# Note that century and year are separated, but they should be joined
# for better column representation.  Consider and run the following 
# code.

unite(table5 , year, century, year)

# Now let's improve on the column heading year by eliminating the under-
# score.  Consider and run the following code.


unite(table5 , year, century, year, sep = "")

table5 %>%
  unite(year, century, year, sep = "", )

# One more example
# NOw let us tidy a data set the needs several tidy actions.
# First, Let's read in the excel tidy practice data that was emailed.
read_csv("tidypracticedata.csv", show_col_types = FALSE)->tp
tp

View(tp)

# Tidying actions
# 1
# We will move the columns Student ID and AGE. A better placement
# for the those variables would be in the front of the data set with
# the existing personal information for the student as opposed to 
# being placed at the rear of the data set.
#       

tp%>%
  select(StudentName , StudentID, GENDER , AGE, Assign1,
  Assign2, Assign3, Assign4 , Assign5, Assign6)

# 2
# Now let's calculate and display the mean for assignment score by
# mutating. Our goal is to create a table that conveys meaningful
# and significant information that is easy to identify. You may have
# to use the View function to see the mutated table.


tp%>%
  select(StudentName , StudentID, GENDER , AGE, Assign1,
         Assign2, Assign3, Assign4 , Assign5, Assign6)%>%
  mutate(MeanAssign = (Assign1 + Assign2 + Assign3 + Assign4
                       + Assign5 + Assign6)/6)->tp1
tp1

View(tp1)




# 3
# Our table is to wide and thus that makes interpreting and analyzing
# the data in our table harder.

# Now let's apply the pivot_long function to convert the data set from
# one that is initially to wide to one that is longer. The new repre-
# sentation will collect the 6 assignments for each person and place
# them under one column.

tp1%>%
  select(StudentName , StudentID, GENDER , AGE, everything())%>%
  pivot_longer(col = Assign1 : Assign6 , names_to = "Assignments", 
               values_to =  "AssignScores") -> tp2
tp2

View(tp2)
#After creating a longer table, you change your mind and decide that the wider table had a better
#and more practical appearance.  You apply R code to convert back to a wider representation

tp2%>%
  pivot_wider(names_from = Assignments, values_from = AssignScores) ->tp3
tp3
View(tp3)
# 4
# Another action is to eliminate the redundancy for the column AGE.
# The designation  yrs for each number of that column is not needed,
# for the column heading indicates what the numbers represent. To 
# accomplish this task we will use a parsing function.


tp2%>%
  pivot_wider(names_from = Assignments, values_from = AssignScores) -> tp3
tp3

tp3%>%
  mutate(AGE = parse_number(AGE)) 


# Let's arrange the student's Names in alphabetical order by First Name

tp3%>%
  mutate(AGE = parse_number(AGE))-> tp4
tp4

View(tp4)
tp4%>%
  arrange(StudentName)
# Now let's place the variable MeanAssign back at the end where it was originally

tp4%>%
  arrange(StudentName)-> tp5
tp5

tp5%>%
  select(StudentName:AGE, Assign1:Assign6, MeanAssign)

# Finally we note that we have a misspelling in the StudentName column.  Marry Hubbbard should be
# Mary Hubbard. We apply R code to correct the typo.

tp5%>%
  select(StudentName:AGE, Assign1:Assign6, MeanAssign)->tp6
tp6

View(tp6)

tp6%>%
  mutate(StudentName = recode(StudentName, "Mary Hubbbard" = "Mary Hubbard")) -> tp7
tp7

View(tp7)


# We now have a nice tidy concise data set.


# More Tidying

library(tidyverse)
library(dplyr)


fish_encounters

fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)

us_rent_income

us_rent_income%>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe)) -> zz
zz

View(zz)


# Can perform aggregation with values_fn

warpbreaks

# levels(warpbreaks$wool)

as.data.frame(warpbreaks)

# Let's transform to a tibble

as_tibble(warpbreaks)

# or

warpbreaks <- as_tibble(warpbreaks[c("wool", "tension", "breaks")])
warpbreaks


warpbreaks%>%
  pivot_wider(
    names_from = wool,
    values_from = breaks,
    values_fn = max
  )


#  More Tidying


# We will examine the embedded readr data table challenge

challenge <- read_csv(readr_example("challenge.csv"))
challenge

# readr has read the first 1000 rows and assigned types accordingly

# note that by default column x is designated dbl and column y has 
# missing values


# Let's examine further by examining observational rows not seen in
# the console


library(dplyr)

challenge%>%
  slice(1001 : 1010)

# note that column x started with integers, but it also has decimal
# numbers. The column designation for x is double.

# This is okay for a double can be an integer or a decimal number

is_double(123)

is_double(11.23)

# now let's consider the following

challenge <- read_csv(readr_example("challenge.csv"))


problems(challenge)

# Let's fix the problem

challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

tail(challenge)


# Now let's run the following coding chunk

challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
) -> challenge2
challenge2



# Missing Values


#  Missing Values are typically denoted by  NA

# When missing values are used in operations, the result is NA
NA + NA

NA - 16

NA/16
#Let's find a the mean for values collected in a vector
c(16, 32, 14, 13) -> vector1
vector1

mean(vector1)

# Now let's attempt to find the mean for values in a vector that
# contains a missing value

c(16, 32, 14, 13, NA ) -> vector2
vector2

mean(vector2)


# Note that the result given is a NA


# We now calculate the mean controlling for the missing value using
# na.rm

mean(vector2, na.rm = TRUE)

# Another example

# Find the mean for the column b in the data table. Note that there
# are no missing values

tribble( ~a,   ~b, ~c,  ~d,
        "Jane", 2,  4,   6,
        "Ron",  8,  10,  12,
        "Mary", 14, 16,  18
) -> t1
t1

mean(t1$b)

# Now attempt to find the mean again for the values of column b, which
# now contains a missing value

tribble( ~a,   ~b, ~c,  ~d,
         "Jane", 2,  4,   6,
         "Ron",  NA,  10,  12,
         "Mary", 14, 16,  18
) -> t2
t2

mean(t2$b)    # Note that the result is NA



# We now calculate the mean, controlling for the missing values.
mean(t2$b, na.rm = TRUE)




q()
y