#' @description Loads the supervisory data (factors, correlation and option volatility)
#' for each Asset Class and SubClass
#' @title Supervisory Data Loading
#'  
#' @return A list with the required data
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references MAR50 - Credit Value Adjustment Framework
#' https://www.bis.org/basel_framework/chapter/MAR/50.htm?inforce=20230101&published=20200708
LoadSupervisoryCVAData <- function()  {

  file_names  = c('IR_cormat_other_ccies','IR_cormat_hard_ccies','CS_cormat_by_sector','CS_cormat_by_tenor','hedge_cpty_corr','superv_risk_weights','CS_ref_Sector_cormat',
                  'CS_Sector_cpty_cormat','COMM_RW','EQ_RW','IR_RW')
  
  file_names_csv = paste0(file_names,'.csv')
  
  superv = list()
  
  for(i in 1:length(file_names))
  {   
    superv[[i]] = read.csv(file_names_csv[i],header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
    if(ncol(superv[[i]])==2)
    {        superv[[i]][,2]   = as.numeric(sub("%","",superv[[i]][,2]))/100
    }else
    { 
      if(file_names[i] %in% c('COMM_RW','EQ_RW'))
      {        superv[[i]][,ncol( superv[[i]])]   = as.numeric(sub("%","",superv[[i]][,ncol( superv[[i]])]))/100
      }else
      {        superv[[i]][,2:ncol( superv[[i]])]   = apply( superv[[i]][,2:ncol( superv[[i]])],2, function(x) as.numeric(sub("%","",x))/100)       }
    }
  }
  names(superv) = file_names
  return(superv)
}
