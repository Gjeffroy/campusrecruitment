## code to prepare `recruitment_data` dataset goes here
library(readr)
library(dplyr)
library(tidyr)

# load and remove useless columns
dataset <- readr::read_csv('data-raw/Placement_Data_Full_Class.csv') %>% select(- sl_no, -ssc_b, -hsc_b )

# store col name in two group (categorical or numeric)
numeric_col <- dataset %>% select_if(is.numeric)%>%colnames()
cat_col <- dataset %>% select_if(is.character)%>%colnames()

# save data and col name as .rda accesible in the package
usethis::use_data(dataset, overwrite = TRUE)
usethis::use_data(numeric_col, overwrite = TRUE)
usethis::use_data(cat_col, overwrite = TRUE)

# Unify column descirption with a dict and save it as .rda
col_description <- c("sl_no" = "Student ID",
  "gender" = "Gender",
  "ssc_p" = "Secondary education Score",
  "ssc_b" = "Secondary - Board of education",
  "hsc_p" = "Higher secondary education Score",
  "hsc_b" = "Higher secondary - Board of education ",
  "hsc_s" = "Specialization in Higher Secondary Education",
  "degree_p" = "Degree Percentage",
  "degree_t" = "Field of degree education",
  "workex" = "Prior work experience",
  "etest_p" = "Employability Score",
  "specialisation" = "MBA specialisation",
  "mba_p" = "MBA specialisation Score",
  "status" = "Employment Status",
  "salary" = "Salary in ₹"  )

usethis::use_data(col_description, overwrite = TRUE)


