#' Calculates the Exposure-At-Default (EAD) based on the given regulatory framework. It supports the CEM, IMM and (simplified) SA-CCR frameworks
#' @title Calculates the Exposure-At-Default (EAD)
#' @param trades The full list of the Trade Objects
#' @param framework Specifies the regulatory framework used in the calculations. It can take the values of 'IMM', 'CEM', 'SA-CCR'
#' @param sa_ccr_simplified (Optional) Specifies whether the standard SACCR or its simplified version or the OEM will be implemented. It can take the values of '', 'simplified', 'OEM'
#' @param CSA The margin agreement with the counterparty
#' @param collateral    The amount of collaterals currently exchanged with the counterparty
#' @param EEE A vector containing the effective expected exposure against the counterparty
#' @param time_points The timepoints that the analysis is performed on
#' @return The Exposure-At-Default
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'

calcEADRegulatory = function(trades, framework, sa_ccr_simplified="", CSA, collateral, EEE, time_points)
{

  if(framework=='CEM')
  {
    addon_table = HashTable('AddonTable.csv',"numeric","numeric")
    MtM_Vector      <- as.numeric(lapply(trades, function(x) x$MtM))
    maturity_vector <- as.numeric(lapply(trades, function(x) x$Ei))
    NGR = CalcNGR(MtM_Vector)
    addon = rep(0,length(maturity_vector))

    for(i in 1:length(maturity_vector))
      addon[i] = addon_table$FindIntervalValue(maturity_vector[i])

    addon_amount = (0.4 + 0.6*NGR)*sum(addon)
    EAD = max(max(sum(MtM_Vector),0)+addon_amount-as.numeric(!is.null(CSA$IM_cpty))*CSA$IM_cpty,0)
  } else if(framework=='SA-CCR')
  {
    if(sa_ccr_simplified=='')
    {
      simplified = FALSE;
      OEM        = FALSE;
    }else if(sa_ccr_simplified=='simplified')
    {
      simplified = TRUE;
      OEM        = FALSE;
    }else if(sa_ccr_simplified=='OEM')
    {
      simplified = TRUE;
      OEM        = TRUE;
    }else
    {
      simplified = FALSE;
      OEM        = FALSE;
    }
    requireNamespace("SACCR")
    
    # calculating the maturity factor
    MF = CSA$CalcMF(simplified = simplified)
    # calculating the add-on
    trades_tree = SACCR::CreateTradeGraph(trades)
    trades_tree <- SACCR::CalcAddon(trades_tree, MF, simplified = simplified, OEM = OEM)
    # calculating the RC and the V-c amount
    rc_values <- SACCR::CalcRC(trades, CSA, list(collateral), simplified = simplified)
    trades_tree$`Replacement Cost` = rc_values
    # calculating the PFE after multiplying the addon with a factor if V-C<0
    PFE <- SACCR::CalcPFE(rc_values$V_C, Addon_Aggregate = trades_tree$addon, simplified = simplified)
    # calculating the Exposure-at-Default
    EAD <- SACCR::CalcEAD(rc_values$RC, PFE)

  } else if(framework=='IMM')
  { EAD = 1.4*mean(EEE[time_points<=1])}

  if(framework=='SA-CCR')
  { return(list(EAD_Value = EAD, Exposure_Tree = trades_tree))
  }else{return(EAD)}
  
}
