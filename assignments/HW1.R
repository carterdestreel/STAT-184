student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# PART A
section <- factor(section, levels = c("A", "B"))
students <- data.frame (
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

score_matrix <- as.matrix(students[, c("quiz1", "quiz2")])
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

course_record <- list(
  course = "R Programming", 
  scores = students, 
  cutoffs = c(pass = 70, excellent = 90)
)

# Inspect objects
typeof(student_id)
class(student_id)
length(student_id)

typeof(section)
class(section)
length(section)
str(section)

typeof(students)
class(students)
dim(students)
str(students)

typeof(score_matrix)
class(score_matrix)
dim(score_matrix)
str(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

# A vector stores elements in a basic type where they all have to be the same type. 
# While a list can have elements of different types and structures.
# A matrix is a 2D structure with one data type
# A factor stores category data in defined levels
# A dataframe is a table whose columns have different types mbut same number of rows



##
##
# Part B
print(score_matrix["S04", "quiz2"])

print(score_matrix[1:2, , drop=FALSE])

course_record["course"]
course_record[["course"]]
course_record$course

# [ selects elements and preserves their structure.
# [[ extracts a single element from a list or other objects.
# $ extracts a named component using its name.

students$average <- rowMeans(students[, c("quiz1", "quiz2")])
students$excellent <- students$average >= 90
selected_students <- students[students$section == "A" & students$average >= 80, c("student_id", "section", "average")]
selected_students

#   student_id section average
# 1        S01       A    83.5
# 5        S05       A    94.5


student_averages <- setNames(students$average, students$student_id)

student_averages
# S01  S02  S03  S04  S05  S06 
# 83.5 90.0 78.0 90.0 94.5 71.5 

# Its vectorized because all the operations are performed aat once rather then going through each one


csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements),]
measurements$sample_id[!complete.cases(measurements)]

# x == NA is not a valid missing-value test because NA represents an unknown value, 
# so comparisons involving NA generally return NA rather than true or false

measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 31
measurements$ph <- measurements$ph < 7

selected_measurements <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]

selected_measurements

#   sample_id  site temp_c temp_f    ph
#1       M01 North   18.2  63.76 FALSE
#7       M07 North   17.8  63.04  TRUE
#8       M08 South   21.2  69.16 FALSE

mean(measurements$temp_c, na.rm = TRUE)

mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)


A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B
dim(A * B)
A %*% B
dim(A %*% B)
# * multiplies corresponding elements of two matrices.
# %*% performs matrix multiplication using row-column dot products. 
# Both results have dimensions 2 by 2.



# part 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)


grade_one <- function(
  score,
  a_min = 90,
  b_min = 80,
  c_min = 70,
  d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# [1] NA
# [1] "A"
# [1] "B"
# [1] "B"
# [1] "C"

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id

grades

# P01 P02 P03 P04 P05 P06 P07 P08 
# "A" "B"  NA "D" "C" "B" "F" "A" 

# i is the number that we're on in how many people we're cycling through

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  result <- c(
    count = length(x),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = na.rm), digits),
    sd = round(sd(x, na.rm = na.rm), digits),
    min = round(min(x, na.rm = na.rm), digits),
    max = round(max(x, na.rm = na.rm), digits)
  )
  return(result)}


summarize_scores(scores)
#  count missing    mean      sd     min     max 
#    8.0     1.0    79.4    13.3    59.0    95.0 

summarize_scores(x = scores, na.rm = TRUE, digits = 2)
#  count missing    mean      sd     min     max 
#   8.00    1.00   79.43   13.28   59.00   95.00 


plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )




  
