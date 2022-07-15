#' Calculates the xVA values (CVA, DVA, FVA, FBA, MVA, KVA)
#'
#' @title Calculates the xVA values
#' @param trades The full list of the Trade Objects
#' @param CSA    The margin agreement with the counterparty
#' @param collateral    The amount of collateral currently exchanged with the counterparty
#' @param sim_data A list containing data related to the calculation of simulated exposures (for example the model parameters and the number of simulations)
#' @param reg_data A list containing data related to the regulatory calculations (for example the 'ccr_framework' member variable can be 'IMM','SACCR','CEM')
#' @param credit_curve_PO   The credit curve of the processing organization
#' @param credit_curve_cpty The credit curve of the processing organization
#' @param funding_curve     A curve containing the credit spread for the funding of the collateral
#' @param spot_rates        The spot rates curve
#' @param cpty_LGD          The loss-given-default of the counterparty
#' @param PO_LGD            The loss-given-default of the processing organization
#' @param no_simulations    if true, no simulated exposure will be generated and the regulatory framework should be SA-CCR
#' @return A list containing the xVA values and the cva capital charge
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Gregory J., The xVA Challenge, 2015, Wiley
#'
xVACalculator = function(trades, CSA, collateral, sim_data, reg_data, credit_curve_PO, credit_curve_cpty, funding_curve, spot_rates, cpty_LGD, PO_LGD, no_simulations)
{
  if(no_simulations&&reg_data$ccr_framework=='IMM') stop('You have chosen not to use any simulations so IMM is not possible - please change the regulatory framework to SA-CCR')
  maturity      <- max(as.numeric(lapply(trades, function(x) x$Ei)))
  time_points    = GenerateTimeGrid(CSA, maturity)
  num_of_points  = length(time_points)
  
  initial_ir_rates  = spot_rates$Rates
  spot_curve     = spot_rates$CalcInterpPoints(time_points)
  initial_cs_rates  = credit_curve_cpty$Rates
  cpty_spread    = credit_curve_cpty$CalcInterpPoints(time_points)
  PO_spread      = credit_curve_PO$CalcInterpPoints(time_points)
  funding_spread = funding_curve$CalcInterpPoints(time_points)
  superv         = LoadSupervisoryCVAData()
  
  discount_factors = exp(-time_points*spot_curve)
  
  PD_cpty        = CalcPD(cpty_spread,cpty_LGD,time_points)
  PD_PO          = CalcPD(PO_spread,PO_LGD,time_points)
  PD_FVA         = CalcPD(funding_spread,1,time_points)
  if(!no_simulations)
  {    exposure_profile = CalcSimulatedExposure(discount_factors, time_points, spot_curve, CSA, trades, sim_data, reg_data$ccr_framework)
  }else
  {
    exposure_profile = list()
    exposure_profile$EE  = 0
    exposure_profile$EEE = 0
  }
  EAD = calcEADRegulatory(trades, reg_data$ccr_framework, reg_data$sa_ccr_simplified, CSA, collateral, exposure_profile$EEE, time_points)
  
  effective_maturity = calcEffectiveMaturity(trades, time_points, reg_data$ccr_framework, exposure_profile$EE)
  
  xVA = list()
  

  
  xVA$KVA        = calcKVA(CSA, collateral, trades, reg_data, time_points, EAD$EAD_Value, effective_maturity, reg_data$ignore_def_charge)
  if(!no_simulations)
  {
    xVA$CVA_simulated        = CalcVA(exposure_profile$EE,  discount_factors, PD_cpty, cpty_LGD)
    xVA$DVA_simulated        = CalcVA(exposure_profile$NEE, discount_factors, PD_PO, PO_LGD)
  }
  if(reg_data$cva_framework=='SA-CVA'&&no_simulations) stop('Please disable the no_simulations flag when selecting the SA-CVA model')
  
  if(reg_data$cva_framework=='SA-CVA')
  {
    bp = 0.0001
    
    cva_sensitivities = list()
    cva_sensitivities$CS_delta  = rep(0,length(credit_curve_cpty$Tenors))
    cva_sensitivities$CS_tenors = credit_curve_cpty$Tenors
    cva_sensitivities$IR_delta  = rep(0,length(spot_rates$Tenors))
    cva_sensitivities$IR_tenors = spot_rates$Tenors
    
    for(i in 1:length( credit_curve_cpty$Tenors))
    {
      credit_curve_cpty$Rates = initial_cs_rates
      credit_curve_cpty$Rates[i] = credit_curve_cpty$Rates[i]+100
      cpty_spread_bumped            = credit_curve_cpty$CalcInterpPoints(time_points)
      PD_cpty_bumped                = CalcPD(cpty_spread_bumped,cpty_LGD,time_points)
      PD_cpty_bumped[PD_cpty_bumped<PD_cpty] = PD_cpty[PD_cpty_bumped<PD_cpty]
      cva_sensitivities$CS_delta[i] = -(CalcVA(exposure_profile$EE, discount_factors, PD_cpty_bumped, cpty_LGD) - xVA$CVA_simulated)/bp
    }

    for(i in 1:length(spot_rates$Rates))
    {
      spot_rates$Rates = initial_ir_rates
      spot_rates$Rates[i] = spot_rates$Rates[i] + 1
      spot_curve_bumped            = spot_rates$CalcInterpPoints(time_points)
      discount_factors_bumped       = exp(-time_points*spot_curve_bumped)
      exposure_profile_bumped       = CalcSimulatedExposure(discount_factors_bumped, time_points, spot_curve_bumped, CSA, trades, sim_data, reg_data$ccr_framework)   
      cva_sensitivities$IR_delta[i] = -(CalcVA(exposure_profile_bumped$EE, discount_factors_bumped, PD_cpty, cpty_LGD) - xVA$CVA_simulated)/bp
    }
  }
  
  cva_capital_charge = calcCVACapital(trades, EAD, reg_data, superv, effective_maturity, cva_sensitivities)
  
  if(reg_data$ccr_framework=='SA-CCR')
  {
    pos_exposure = ifelse(EAD$Exposure_Tree$`Replacement Cost`$V_C+EAD$Exposure_Tree$addon<0,0,EAD$Exposure_Tree$`Replacement Cost`$V_C+EAD$Exposure_Tree$addon)
    neg_exposure = ifelse(EAD$Exposure_Tree$`Replacement Cost`$V_C-EAD$Exposure_Tree$addon>0,0,(ifelse(EAD$Exposure_Tree$`Replacement Cost`$V_C<0,EAD$Exposure_Tree$`Replacement Cost`$V_C,-EAD$Exposure_Tree$`Replacement Cost`$V_C)-EAD$Exposure_Tree$addon))
    pd_aggregate = 0
    
    for(i in 1:ceiling(effective_maturity))
    { pd_aggregate = pd_aggregate + reg_data$PD_cpty * (1 - reg_data$PD_cpty)^(i - 1)}
    
    xVA$CVA_SACCR            = -pos_exposure*pd_aggregate*cpty_LGD
    
    pd_aggregate = 0
    
    for(i in 1:ceiling(effective_maturity))
    { pd_aggregate = pd_aggregate + reg_data$PD_PO * (1 - reg_data$PD_PO)^(i - 1)}
    
    xVA$DVA_SACCR            = -neg_exposure*pd_aggregate*PO_LGD
    
    pd_aggregate = 0
    
    for(i in 1:ceiling(effective_maturity))
    { pd_aggregate = pd_aggregate + reg_data$PD_cpty * (1 - reg_data$PD_FVA)^(i - 1)}
    
    xVA$FCA_SACCR        = -pos_exposure*pd_aggregate
    xVA$FBA_SACCR        = -neg_exposure*pd_aggregate
    xVA$MVA_SACCR        = xVA$FCA_SACCR*2*sqrt(reg_data$mva_days/(250*maturity))*qnorm(reg_data$mva_percentile)/dnorm(0)
    
  }
  if(!no_simulations)
  {
    xVA$FCA_simulated        = CalcVA(exposure_profile$EE,  discount_factors, PD_FVA)
    xVA$FBA_simulated        = CalcVA(exposure_profile$NEE, discount_factors, PD_FVA)
    xVA$MVA_simulated        = xVA$FCA_simulated*2*sqrt(reg_data$mva_days/(250*maturity))*qnorm(reg_data$mva_percentile)/dnorm(0)
  }

  return(xVA)
}
