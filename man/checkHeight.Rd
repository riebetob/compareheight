% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkHeight.R
\name{checkHeight}
\alias{checkHeight}
\title{Compare the height of persons in a dataset with the mean of the height of the other persons}
\usage{
checkHeight(students.input, sex.specific = TRUE, print.statement = FALSE)
}
\arguments{
\item{students.input}{A data.frame containig names and height of students}

\item{sex.specific=TRUE}{A logical that tells if gender spcific mean or mean of whole population should be compared}
}
\value{
Difference in height to gender specific mean or the whole population.
}
\description{
Compare the height of persons in a dataset with the mean of the height of the other persons
}
