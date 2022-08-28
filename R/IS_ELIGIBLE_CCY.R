#' @description Checks if the specified currency is eligible to receive reduced regulatory risk weights
#' 
#' @title Checks if specified currency is low risk
#' @param ccy The currency to be checked
#' @return TRUE if the currency is is eligible to receive reduced regulatory risk weights
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references https://www.bis.org/basel_framework/chapter/MAR/50.htm?inforce=20230101&published=20200708
#' 
#' @examples
#' 
#' TRUE == IS_ELIGIBLE_CCY('EUR')
#' 
IS_ELIGIBLE_CCY <- function(ccy)  {
  
  eligible_ccies = c('USD','EUR','GBP','AUD','CAD','SEK','JPY','REPORTING')
  return(ccy %in% eligible_ccies)
}