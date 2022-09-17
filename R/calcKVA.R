#' Calculates the capital valuation adjustment by computing the default capital charge and the CVA capital charge and applying the required return-on-capital
#' @title Calculates the Capital Valuation Adjustment (KVA)
#' @param trades The full list of the Trade Objects
#' @param reg_data A list containing data related to the regulatory calculations (for example the 'framework' member variable can be 'IMM','SACCR','CEM')
#' @param EAD The Exposure-at-default calculated based on the prescribed framework as appearing in the 'reg_data'
#' @param effective_maturity The effective maturity of the trades performed with a specific counterparty
#' @param ignore_def_charge if set to true the default capital charge is set to zero
#' @return The capital valuation adjustment (KVA)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'
calcKVA = function(trades, reg_data, EAD, effective_maturity, ignore_def_charge = TRUE, cva_capital_charge)
{
  if(ignore_def_charge)
  {   def_capital_charge = 0
  }else
  {def_capital_charge = calcDefCapital(trades,EAD, reg_data, effective_maturity)}
  
  KVA = -(def_capital_charge+cva_capital_charge)*0.5*sqrt(effective_maturity)*reg_data$return_on_capital
  
  return(KVA)
}
