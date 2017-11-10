
#'Function for calculating mean and rounding on three decimals
#'@param values A vector with values
#'@return The mean rounded on three decimals
#'@export

mean = function(values){
  round(sum(values)/length(values),3)
}
