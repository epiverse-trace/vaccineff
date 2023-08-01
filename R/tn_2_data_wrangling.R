#' Title
#'
#' @param data
#' @param vaccination_column
#' @param screening_column
#' @param strain
#'
#' @return
#' @export
#'
#' @examples
data_wrangling <- function(data, vaccination_column, screening_column,
                           strain = FALSE) {

  contingency_list <- NULL
  data_interest <- data[,c(screening_column, vaccination_column)]
  if(!strain) {
    data_interest[["screening_res"]] <- ifelse(data_interest[[screening_column]] == "Neg", 0, 1)
    data_interest[["vaccination_cat"]] <- ifelse(data_interest[[vaccination_column]] == "No", 0, 1)

    contingency_table <- as.matrix(table(data_interest[["vaccination_cat"]],
                                         data_interest[["screening_res"]]))

    colnames(contingency_table) <- c("Non-infected", "Infected")
    rownames(contingency_table) <- c("Unvaccinated", "Vaccinated")

    contingency_list[["Global"]] <- contingency_table
  } else {
    strains <- unique(data_interest[[screening_column]])
    for (s in strains) {
      print(s)
      if(s=="Neg") next
      idx = which(data_interest[[screening_column]]=="Neg" |
                    data_interest[[screening_column]]== s)
      tmp_data = data_interest[idx, ]
      tmp_data[["screening_res"]] <- ifelse(tmp_data[[screening_column]] == "Neg", 0, 1)
      tmp_data[["vaccination_cat"]] <- ifelse(tmp_data[[vaccination_column]] == "No", 0, 1)

      contingency_table <- as.matrix(table(tmp_data[["vaccination_cat"]],
                                           tmp_data[["screening_res"]]))

      colnames(contingency_table) <- c("Non-infected", "Infected")
      rownames(contingency_table) <- c("Unvaccinated", "Vaccinated")

      contingency_list[[s]] <- contingency_table
    }
  }

  contingency_list
}

