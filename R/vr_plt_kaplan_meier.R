#' Plot Kaplan-Meier Curve for Existing Tidy Survival Object
#'
#' TODO: Define and describe the purpose of this function. 
#' 
#' @param broom_object, 
#' @param title = "", 
#' @param abbreviations = NULL, 
#' @param variable_definitions = NULL, 
#' @param N = NULL, 
#' @param N_unit = "patients", 
#' @param time_unit = "days", 
#' @param data_source = NULL, 
#' @param estimate_name = "survival probability" 
#'
#' @return ggplot object 
#' @export
#'
#' @examples
#' # TODO: Define an example for this function
#' # Create interim data models
#' library(survival)
#' library(ggplot2)
#' library(dplyr)
#' library(visR)
#' data("veteran")
#' data <-  veteran %>% 
#'     mutate(trt = as.factor(case_when(
#'        trt == 1 ~ "standard therapy", 
#'        trt == 2 ~ "test chemotherapy"
#'    )))
#'
#' equation <- "survival::Surv(time, status) ~ trt"
#' risk_table_data <- vr_est_km_risk_table(data, equation)
#' time_unit = "days"
#' broom_object <- vr_est_kaplan_meier(data, equation)
#' risk_table_data <- vr_est_km_risk_table(data, equation)
#' 
#' # Create plot
#' table <- vr_plt_km_risk_table(risk_table_data,
#'                               time_unit = time_unit)
#'                               
#' ##not run TODO: Check why this example is failing
#' vr_plt_kaplan_meier(
#'   broom_object, 
#'   N = "Patients", 
#'   time_unit = "days", 
#'   data_source = "this is the data source label"
#'  )

## SHAESEN2:
 # Name function: vr_KM_plot is more interesting for sorting purposes and easier to remember: vr = package, KM = analysis, plot = option
 # broom_object: would be nice to have a check for this. 
   # Often we apply KM for multiple PARAMCD/subgroups => mapply. Is this something we can consider eg if input is a list of broom we can use the names of the list as subtitle
 # title = "": Needs to be conditionally added. Otherwise you introduce an empty line, shrinking the graph
 # Is N at top really necessary? It does not seem informative. I would prefer to have an argument for risk table here.
 # estimate_name => ylabel is a more informative name
 # I would focuss on the visual representation => special features such as caption can still be added in RTF/latex/html. Especially the three arguments you have here is overkill.
 
## minimal proposal
  vr_KM_plot <- function(
     broom_object = NULL                       # object: does it need to be broom? Can we input KM object instead? (keeping in mind validation and possible breaking updates in broom)
    ,title = NULL
    #,tally = "USUBJID"                         # tally is much clear than N_unit + use controlled terminology. Does not seem very useful to have as for correct interpretation, you need xasixtable saying Number of subject/tally-name at risk.
    ,y_label = "Suvival Probability"
    ,x_label = NULL                            # take PARAMCD if present in broom object => use label of AVAL/CHG, ... Time (days) is not sufficient We would need "Time to resolution of xxx"
    ,xaxistable=FALSE
  ){
    ## In base R calculating the at risk set seems simpler. At risk should be defined based on x-asis ticks.
    x00<-c(nsclc.KM$time[1:nsclc.KM$strata[1]])
    x10<-nsclc.KM$time[(nsclc.KM$strata[1]+1):(nsclc.KM$strata[1]+nsclc.KM$strata[2])]
    x01 <- table(cut(x = x00, breaks = c(-1,toi)))
    x11 <- table(cut(x = x10, breaks = c(-1,toi))) 
    xt0 <- nrow(nsclc[nsclc$trt01an==0,])-cumsum(x01)
    xt1 <- nrow(nsclc[nsclc$trt01an==1,])-cumsum(x11)
  
  }
  

#################################################

vr_plt_kaplan_meier <- function(
    broom_object, 
    title = "", 
    abbreviations = NULL, 
    variable_definitions = NULL, 
    N = NULL, 
    N_unit = "patients", 
    time_unit = "days", 
    data_source = NULL, 
    estimate_name = "survival probability"
) {
  
    # Get number of patients
    if (is.null(N)) {
        N <- 
            broom_object  %>% 
            dplyr::group_by(strata) %>% 
            dplyr::filter(row_number() == 1) %>% 
            dplyr::ungroup() %>% 
            dplyr::summarize(N = sum(n.risk)) %>% 
            dplyr::pull(N)
    }

    # Build caption
    data_source_caption <- sprintf("Data Source: %s", data_source)
    abbreviations_caption <- sprintf("Abbreviations: %s", abbreviations)
    variable_definitions_caption <- sprintf("Variable Definitions: %s", variable_definitions)
    
    plot <- ggplot2::ggplot(broom_object, aes(x = time)) + 
        pammtools::geom_stepribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata), alpha = 0.25) + 
        geom_step(aes(y = estimate, col = strata)) + 
        # ggplot2::theme_light() + 
        ggsci::scale_color_nejm() + 
        ggsci::scale_fill_nejm() + 
        ggplot2::ylab(estimate_name) + 
        ggplot2::xlab(sprintf("time (%s)", time_unit)) + 
        ggplot2::labs(
            title = title, 
            #subtitle = sprintf("N [%s] = %s", N_unit, N), 
            caption = paste(abbreviations_caption, variable_definitions_caption, data_source_caption))
      
    return(plot)
}
