#' Calculates the Exposure-At-Default (EAD) based on the given regulatory framework. It supports the CEM, SA-CCR and IMM frameworks
#' @title Calculates the Exposure-At-Default (EAD)
#' @param trades The full list of the Trade Objects
#' @param framework Specifies the regulatory framework used in the calculations. It can take the values of 'IMM', 'CEM', 'SA-CCR'
#' @param col The margin agreement with the counterparty
#' @param EEE A vector containing the effective expected exposure against the counterparty
#' @param time_points The timepoints that the analysis is performed on
#' @return The Exposure-At-Default
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'

calcEAD = function(trades, framework, col,EEE,time_points)
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
    EAD = max(max(sum(MtM_Vector),0)+addon_amount-as.numeric(!is.null(col$IM_cpty))*col$IM_cpty,0)
  } else if(framework=='SA-CCR')
  {
    requireNamespace("SACCR")

    current_collateral = Trading::Collateral(ID="col_1",csa_id="csa_1",Amount=0.03,type="VariationMargin")
    MF = col$CalcMF()
    # calculating the add-on
    trades_tree = SACCR::CreateTradeGraph(trades)
    trades_tree <- SACCR::CalcAddon(trades_tree, MF)
    # calculating the RC and the V-c amount
    rc_values <- SACCR::CalcRC(trades, col, list(current_collateral))
    # calculating the PFE after multiplying the addon with a factor if V-C<0
    PFE <- SACCR::CalcPFE(rc_values$V_C, trades_tree$addon)
    # calculating the Exposure-at-Default
    EAD <- SACCR::CalcEAD(rc_values$RC,PFE)

  } else if(framework=='IMM')
  { EAD = 1.4*mean(EEE[time_points<=1])}

  return(EAD)
}
