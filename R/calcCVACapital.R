utils::globalVariables("V1")
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
calcCVACapital = function(trades, EAD, reg_data, superv, effective_maturity, cva_sensitivities)
{
  rating_table = HashTable('RatingsMapping.csv',"character","numeric")
  
  reg_weight =rating_table$FindValue(reg_data$cpty_rating)
  superv_rw = superv$superv_risk_weights[superv$superv_risk_weights$Sector == reg_data$cpty_sector, ifelse(IS_IG(reg_data$cpty_rating),2,3)]
  
  if(reg_data$cva_framework=="STD-CVA")
  {
    df =(1-exp(-0.05*effective_maturity))/(effective_maturity*0.05)
    cva_capital_charge = qnorm(0.99)*sqrt(sum(0.5*reg_weight*EAD*df*effective_maturity)^2+0.75*sum((reg_weight*EAD*df*effective_maturity)^2))
  }else if(reg_data$cva_framework=="BA-CVA")
  {
    cva_capital_charge = EAD/1.4*superv_rw*effective_maturity
  }else if(reg_data$cva_framework=="SA-CVA")
  {
    # The (oversimplifying) assumption being taken here that all trades belong to the same currency
    ccy = unlist(unique(lapply(trades, function(x) x$Currency)))
    
    if(IS_ELIGIBLE_CCY(ccy))
    {
      cormat = superv$IR_cormat_eligible_ccies
      IR_RW  = superv$IR_RW[1:nrow(superv$IR_RW)-1,2]

    }else{
      cormat = superv$IR_cormat_other_ccies
      IR_RW  = superv$IR_RW[1:nrow(superv$IR_RW)-1,3]
    }
    
    # IR part
    superv_tenors         = as.numeric(cormat[1:nrow(cormat)-1,1])
    matching_indices      = findInterval(cva_sensitivities$IR_tenors, superv_tenors)
    mapped_IR_sens        = data.table(cbind(cva_sensitivities$IR_delta, matching_indices))
    mapped_IR_sens_summed = mapped_IR_sens[,sum(V1), by=matching_indices]
    superv_tenors_indexed = data.table(superv_tenors = superv_tenors,matching_indices = seq(1,length(superv_tenors)))
    mapped_IR_sens_summed = setkey(mapped_IR_sens_summed,"matching_indices")[setkey(superv_tenors_indexed,"matching_indices")]
    mapped_IR_sens_summed[is.na(V1),V1:=0]

    ir_charge = reg_data$sa_cva_multiplier*sqrt((mapped_IR_sens_summed$V1*IR_RW)%*%(data.matrix(cormat[1:(nrow(cormat)-1),2:(ncol(cormat)-1)])%*%(mapped_IR_sens_summed$V1*IR_RW)))
    
    # CS part
    cormat                = superv$CS_cormat_by_tenor
    superv_tenors         = cormat[,1]
    matching_indices      = findInterval(cva_sensitivities$CS_tenors, superv_tenors)
    mapped_CS_sens        = data.table(cbind(cva_sensitivities$CS_delta, matching_indices))
    mapped_CS_sens_summed = mapped_CS_sens[,sum(V1), by=matching_indices]
    superv_tenors_indexed = data.table(superv_tenors = superv_tenors,matching_indices = seq(1,length(superv_tenors)))
    mapped_CS_sens_summed = setkey(mapped_CS_sens_summed,"matching_indices")[setkey(superv_tenors_indexed,"matching_indices")]
    mapped_CS_sens_summed[is.na(V1),V1:=0]

    cs_charge = reg_data$sa_cva_multiplier*superv_rw*sqrt((mapped_CS_sens_summed$V1)%*%(data.matrix(cormat[,2:(ncol(cormat))])%*%(mapped_CS_sens_summed$V1)))
    
    cva_capital_charge = list()    
    cva_capital_charge$IR_charge = ir_charge
    cva_capital_charge$CS_charge = cs_charge
    cva_capital_charge$Total_charge = ir_charge + cs_charge
    
  }else
  {stop('cva_framework can be either "BA-CVA" or "SA-CVA" or "STD-CVA". Please correct your input')}
  
  return(cva_capital_charge)
}
