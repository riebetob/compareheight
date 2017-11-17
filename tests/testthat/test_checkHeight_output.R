#Set system language to english
#Sys.setenv(LANG = "en")
library(dplyr)
test_that("Input and Output have same number of observations", {
  expect_equal(nrow(students), nrow(checkHeight(students)))
})

test_that("Both options of sex.specific work equally", {
  students_test <- students
  #All students get the same height
  students_test$height <- 1.80
  expect_equal(checkHeight(students_test, sex.specific = TRUE)[,2],
               checkHeight(students_test, sex.specific = FALSE)[,2])
})

test_that("Functioning of the argument check ", {
  #test if sex.specific is boolean
  expect_error(checkHeight(students, sex.specific = 5))
  #test if print.statemnet is boolean
  expect_error(checkHeight(students, print.statement = 5))
  #test if data frame is long enough
  expect_error(checkHeight(students[1:3]))
  #test if persons not in range are registered
  students_test <- students
  students_test[1,3] <- 5.80
  expect_error(checkHeight(students_test))
  #test if persons wrong gender is registered
  students_test <- students
  levels(students_test[,4]) <- c("F", "M", "C")
  expect_error(checkHeight(students_test))
  #test mean function
  expect_error(compareheight:::mean(NaN))
})

test_that(desc = "The Object can't be found if a  name is wrong", {
            test_students = students
            colnames(test_students)[3] = "servus"
            expect_that(checkHeight(test_students), throws_error() )
            })


test_that("Throws error if argument is missing", {
  expect_error(checkHeight())
})

test_that("Check if print statement works correctly", {
  expect_that(checkHeight(students,print.statement = TRUE),
              prints_text("Yippie, I calculated the mean differences!"))
})

#testthat::test_dir("C:\\Users\\Tobias\\Documents\\Uni\\Innovationslabor_1718\\compareheight\\tests")
