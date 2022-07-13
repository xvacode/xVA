#' Calculates the CVA capital charge based on the standardized approach
#' @title Calculates the CVA Capital Charge
#' @param trades The full list of the Trade Objects
#' @param EAD Exposure-at-Default
#' @param reg_data A list containing data related to the regulatory calculations 
#' @param superv A list containing supervisory data including correlations, risk weights etc
#' @param effective_maturity The effective maturity of the trades of the netting set
#' @param cva_sensitivities  The effective maturity of the trades of the netting set
#' @return The CVA capital charge of the trade set
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
calcCVACapital = function(trades, EAD, reg_data, , superv, effective_maturity, cva_sensitivities)
{
  rating_table = HashTable('RatingsMapping.csv',"character","numeric")
  
  reg_weight =rating_table$FindValue(reg_data$cpty_rating)
  
  
  if(reg_data$cva_framework=="STD-CVA")
  {
    df =(1-exp(-0.05*effective_maturity))/(effective_maturity*0.05)
    cva_capital_charge = qnorm(0.99)*sqrt(sum(0.5*reg_weight*EAD*df*effective_maturity)^2+0.75*sum((reg_weight*EAD*df*effective_maturity)^2))
  }else if(reg_data$cva_framework=="BA-CVA")
  {
    superv_rw = superv$superv_risk_weights[superv$superv_risk_weights$Sector == reg_data$cpty_sector, ifelse(IS_IG(reg_data$cpty_rating),2,3)]
    cva_capital_charge = EAD/1.4*superv_rw*effective_maturity
  }else if(reg_data$cva_framework=="SA-CVA")
  {
    # The assumption is being taken here that all trades belong to the same currency
    ccy = unlist(unique(lapply(trades, function(x) x$Currency)))
    
    if(is_eligible_ccy(ccy))
    {
      superv_tenors         = superv$IR_cormat_eligible_ccies[1:nrow(superv$IR_cormat_eligible_ccies)-1,1]
      matching_indices      = findInterval(cva_sensitivities$IR_tenors, superv_tenors)
      mapped_IR_sens        = cbind(cva_sensitivities$IR_delta, matching_indices)
      mapped_IR_sens_summed = data.table(sum(mapped_IR_sens, by=mapped_IR_sens$matching_indices))
      superv_tenors_indexed = data.table(cbind(superv_tenors,seq(1,length(superv_tenors))))
      mapped_IR_sens_summed = mapped_IR_sens_summed[superv_tenors_indexed]
      ir_charge = mapped_IR_sens_summed%*%superv$IR_cormat_eligible_ccies[,2:ncol(superv$IR_cormat_eligible_ccies)]%*%IR_RW[,2]
    }else{
      
    }

    
  }else
  {stop('cva_framework can be either "BA-CVA" or "SA-CVA" or "STD-CVA". Please correct your input')}
  
  return(cva_capital_charge)
}
