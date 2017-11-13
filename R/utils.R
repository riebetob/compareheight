
#'Function for calculating mean and rounding on three decimals
#'@param values A vector with values
#'@return The mean rounded on three decimals

mean = function(values){
  #Checks if values are numeric and if the vector at least contains one value for
  #denominator not being 0 later. No missing values allowed
  assert(checkNumeric(values, any.missing = FALSE, min.len = 1))
  round(sum(values)/length(values),3)
}
