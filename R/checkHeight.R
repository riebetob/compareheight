.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Nice that you are using my first package")
}
#' Compare the height of persons in a dataset with the mean of the height of the other persons
#'
#' @param students.input A data.frame containig names and height of students
#' @param sex.specific=TRUE A logical that tells if gender spcific mean or mean of whole population should be compared
#' @return Difference in height to gender specific mean or the whole population.
#' @examples
#' checkHeight(students, TRUE)
#' @import checkmate
#' @importFrom magrittr %>%

#' @export
checkHeight <- function(students.input, sex.specific = TRUE, print.statement = FALSE){
  #Check if sex.specific is a logical value
  assertLogical(sex.specific)
  #Check if print.statement is a logical value
  assertLogical(print.statement)
  #Check if the Data Frame has at least 4 rows and 5 columns without missing values
  assertDataFrame(students.input, min.rows = 4, ncols = 5, any.missing = FALSE)
  #Check if first and second column are Numeric
  assertNumeric(c(students.input[,1], students.input[,2]))
  #Check if third column is numeric and has values between 1.30 and 2.40
  assertNumeric(students.input[,3], lower = 1.29, upper = 2.41)
  #Check if 4th column is of type factor and has values "M" and "F"
  assertFactor(students.input[,4], levels = c("M","F"))
  #Check if 5th column is Character
  assertCharacter(students.input[,5])

  #Check if the sex specific height difference or the difference to the whole population should be calculated
  if(sex.specific == TRUE){
    #Calculate the gender specific means
    women_mean_height = as.numeric(students.input %>%
                                     dplyr::group_by(sex) %>%
                                     summarise(compareheight:::mean(height)) %>%
                                     filter(sex == "F") %>%
                                     select("compareheight:::mean(height)"))
    men_mean_height = as.numeric(students.input %>%
                                   dplyr::group_by(sex) %>%
                                   summarise(compareheight:::mean(height)) %>%
                                   filter(sex == "M") %>%
                                   select("compareheight:::mean(height)"))
    #initialize an empty vector to save the genderspecific height differnces
    height_vector = c()
    #apply a function to the rows of the input dataframe
    height_vector = apply(students.input, MARGIN = 1,
                          FUN = function(student){
                            #substract the gender specific means from the individuals to get height differnces
                            (if (student["sex"] == "M") men_mean_height - as.numeric(student["height"])
                             else women_mean_height - as.numeric(student["height"]) )
                          } )
    #create the final dataframe containing name od the students and the height differnces
    #multiple height differences by 100 to get values in cm
    result.frame = data.frame("name" = students.input$name, "sexspec_height_diff" = height_vector*100)
  } else{
    #initialize an empty vector to save the height differnces
    height_vector = c()
    #calculate the mean height of the whole population
    mean_height = compareheight:::mean(students.input$height)
    #apply a function to the rows of the input dataframe
    height_vector = apply(students.input, MARGIN = 1,
                          FUN = function(student){
                            #substract the gender specific means from the individuals to get height differnces
                            mean_height - as.numeric(student["height"])
                          } )
    #create the final dataframe containing name od the students and the height differnces
    #multiple height differences by 100 to get values in cm
    result.frame = data.frame("name" = students.input$name, "height_diff" = height_vector*100)
  }
  #return the dataframe
  if(print.statement == TRUE){
    print("Yippie, I calculated the mean differences!")
  }
  return(result.frame)
}


