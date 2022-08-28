#' Calculates the capital valuation adjustment by computing the default capital charge and the CVA capital charge and applying the required return-on-capital
#' @title Calculates the Capital Valuation Adjustment (KVA)
#' @param CSA    The margin agreement with the counterparty
#' @param collateral    The current amount of collaterals currently exchanged with the counterparty
#' @param trades The full list of the Trade Objects
#' @param reg_data A list containing data related to the regulatory calculations (for example the 'framework' member variable can be 'IMM','SACCR','CEM')
#' @param time_points The timepoints that the analysis is performed on
#' @param EAD The Exposure-at-default calculated based on the prescribed framework as appearing in the 'reg_data'
#' @param effective_maturity The effective maturity of the trades performed with a specific counterparty
#' @param ignore_def_charge if set to true the default capital charge is set to zero
#' @return The capital valuation adjustment (KVA)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'
calcKVA = function(CSA, collateral, trades, reg_data, time_points, EAD, effective_maturity, ignore_def_charge = TRUE)
{
  if(ignore_def_charge)
  {   def_capital_charge = 0
  }else
  {def_capital_charge = calcDefCapital(trades,EAD, reg_data, effective_maturity)}
  

  #  cva_capital_charge = calcCVACapital(trades, EAD, reg_data$cpty_rating, effective_maturity)

  #  KVA = -(def_capital_charge+cva_capital_charge)*0.5*sqrt(effective_maturity)*reg_data$return_on_capital
}
