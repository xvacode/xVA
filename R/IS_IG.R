#' @description Checks if the credit rating is investment grade or not (if not rating not recognised will be unrated)
#' 
#' @title Checks if Credit rating is Investment Grade
#' @param credit_rating The Credit Rating to be checked
#' @return TRUE if Rating is Investment Grade
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://en.wikipedia.org/wiki/Credit_rating
#' 
#' @examples
#' 
#' TRUE == IS_IG('AAA')
#' 
IS_IG <- function(credit_rating)  {

  base_ratings = c('AA','A','BBB')
  base_ratings = c('AAA', base_ratings, paste0(base_ratings,'+'), paste0(base_ratings,'-'))
  return(credit_rating %in% base_ratings)
}
