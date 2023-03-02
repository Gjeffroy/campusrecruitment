## code to prepare `recruitment_data` dataset goes here

library(readr)
library(dplyr)
library(tidyr)

dataset <- readr::read_csv('data-raw/Placement_Data_Full_Class.csv')
numeric_col <- dataset %>% select_if(is.numeric)%>% select(- sl_no)%>%colnames()
cat_col <- dataset %>% select_if(is.character)%>%colnames()
usethis::use_data(dataset, overwrite = TRUE)
usethis::use_data(numeric_col, overwrite = TRUE)
usethis::use_data(cat_col, overwrite = TRUE)

# colums description
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


# create an aggregated dataset to depict studient profil by gender, education and status
# for each group, we compute median and standart deviation
# last, we scale it from 0 to 5 to represent it on a radar plot that as only one scale
# then adding it as a package
num_cols <- c('ssc_p', 'hsc_p', 'degree_p', 'etest_p', 'mba_p', 'salary')
cat_cols <- c('gender', 'hsc_s', 'status')
grouped_by <- dataset %>% dplyr::select(dplyr::all_of(c(cat_cols, num_cols))) %>%
  dplyr::group_by(gender, hsc_s, status)
median_student <- grouped_by %>%
  dplyr::summarise_at(num_cols,median)%>%
  dplyr::mutate(stat='median')
sd_student <- grouped_by %>%
  dplyr::summarise_at(num_cols,sd)%>%
  dplyr::mutate(stat='sd')

# Student profile
student_profil <- rbind(median_student, sd_student)%>%
  dplyr::mutate_at(c('salary'), ~tidyr::replace_na(.,0))%>%
  dplyr::ungroup()

lookup <- c('12th Grade Score' = 'hsc_p', '10th Grade score'  ='ssc_p', 'Degree Score' = 'degree_p','Employability Score'= 'etest_p' , 'MBA Score' = 'mba_p' )
student_profil <- student_profil %>% dplyr::rename(dplyr::all_of(lookup))

usethis::use_data(student_profil, overwrite = T)

