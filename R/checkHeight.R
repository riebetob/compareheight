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

#' @export
checkHeight <- function(students.input = students, sex.specific = TRUE){
  #Check if the sex specific height difference or the difference to the whole population should be calculated
  if(sex.specific == TRUE){
    #Calculate the gender specific means
    women_mean_height = as.numeric(students.input %>%
                                     group_by(sex) %>%
                                     summarise(mean(height)) %>%
                                     filter(sex == "F") %>%
                                     select("mean(height)"))
    men_mean_height = as.numeric(students.input %>%
                                   group_by(sex) %>%
                                   summarise(mean(height)) %>%
                                   filter(sex == "M") %>%
                                   select("mean(height)"))
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
      mean_height = mean(students.input$height)
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
  return(result.frame)
}


