#' @title ' Functions to perform Trimmed mean and sides of a  right triangle
#'
#' @decription The first function calculates the length of the third side when the lengths of two sides of the triangle is given, based on the Pythagorean theorem.
#' @param Hypotenuse Length of the Hypotenuse of a right triangle
#' @param opposite Length of the opposite side which is across from a given angle of a right triangle
#' @param adjacent Length of the  adjacentside which is next to a given angle of a right triangle.
#' @return side The side that is unknown.
#' @examples rightTriangle(5,4,NA)
#' @examples rightTriangle(NA,4,3)
#' @export
rightTriangle <- function(Hypotenuse,opposite,adjacent){
  if (is.na(Hypotenuse== T)) {side <- sqrt(opposite^2 + adjacent^2)}
  if (is.na(opposite== T)) {side <- sqrt(Hypotenuse^2 - adjacent^2)}
  if (is.na(adjacent== T)) {side <- sqrt(Hypotenuse^2 - opposite^2)}
  return(side)

}
#' @description The second function will calculate the mean of a numeric vector, ignoring the s smallest value and l largest values.
#' @title Trimmed mean
#' @param x a numeric vector thet the Trimmed mean will be calculated
#' @param s number of minimum values that is ignored
#' @param l number of maximum values that is ignored
#' @return MeanX the mean of the given vector when the requested values are ignored.
#' @examples Trimmed_mean(c(9,10,11,12),1,2)
#' @examples Trimmed_mean(c(9,10,11,12),3,1)
#' @export
Trimmed_mean <- function(x,s,l){
  for (item in 1:s){
    x <- x[!x == min(x)]
  }
  for ( item2 in 1:l){
    x <- x[!x == max(x)]
  }
  MeanX <- mean(x)
  return(MeanX)
}

