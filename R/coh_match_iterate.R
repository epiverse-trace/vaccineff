#' @title Rematch Step for Iterative Match
#'
#' @description This auxiliary function performs the re-matching strategy for
#' `iterate_match()`. It rematches the population with status given by
#' `rematch_status` and excludes the removed population with status
#' `control_status`. A `tryCatch` validation handles errors when no matches
#' can be generated due to the lack of treated or untreated individuals.
#'
#'
#' @inheritParams iterate_match
#' @param im Iteration number for error message
#' @return List that contains three `data.frame`:
#' `adjusted`: updated adjusted matches
#' `adjusted_i_s`: new adjusted matches for `rematch_status`
#' `matched_i_s`: all the new matches for `rematch_status`
#'
#' @keywords internal

rematch_ <- function(all,
                     adjusted,
                     outcome_date_col,
                     censoring_date_col,
                     immunization_date_col,
                     removed_i,
                     vacc_status_col,
                     rematch_status,
                     control_status,
                     nearest,
                     exact,
                     start_cohort,
                     end_cohort,
                     im) {
  # Separate removed vaccinated and unvaccinated to rematch separately
  removed_s <- removed_i[
    removed_i[[vacc_status_col]] == rematch_status,
  ]
  unmatched_i_s <- all[!(all$match_id %in% adjusted$match_id) &
      !(all$match_id %in% removed_i$match_id),
  ]
  adjusted_i_s <- data.frame()
  matched_i_s <- data.frame()
  # try match if there are individuals with the opposite status
  if (nrow(
    unmatched_i_s[unmatched_i_s[[vacc_status_col]] == control_status, ]
  ) > 0
  ) {
    # concat unmatched_i and removed to match again
    removed_s <- subset(removed_s, select = names(unmatched_i_s))
    unmatched_i_s <- rbind(unmatched_i_s, removed_s)
    tryCatch({
      # try match and adjust
      matched_i_s <- match_cohort_(
        data_set = unmatched_i_s,
        vacc_status_col = vacc_status_col,
        nearest = nearest,
        exact = exact
      )
      adjusted_i_s <- adjust_exposition(
        matched_cohort = matched_i_s,
        outcome_date_col = outcome_date_col,
        censoring_date_col = censoring_date_col,
        immunization_date = immunization_date_col,
        start_cohort = start_cohort,
        end_cohort = end_cohort
      )
    }, error = function(e) {
      # MatchIt returns error if there are no enough individuals
      # from both groups to match
      warning("Error at iteration ", im, ": ", e$message, "- skipping to next")
    })
    # Control for new matched and adjusted population
    # Add new matches for removed unvaccinated if:
    # 1. There are matches
    # 2. Pairs were not previously matched
    if (
      (nrow(adjusted_i_s) > 0) &&
        (
          nrow(adjusted_i_s[adjusted_i_s$match_id %in% adjusted$match_id, ])
          == 0
        )
    ) {
      adjusted_i_s$subclass <- as.factor(as.numeric(adjusted_i_s$subclass) +
                                           nrow(adjusted))
      adjusted <- rbind(adjusted, adjusted_i_s)
    }
  }
  return(
    list(
      adjusted = adjusted,
      adjusted_i_s = adjusted_i_s,
      matched_i_s = matched_i_s
    )
  )
}

#' @title Iterate Match
#'
#' @description This function iterates to re-match registers that were
#' removed after adjusting exposition times. To avoid generating the
#' same pairs already removed, each iteration is split in two steps,
#' one for the removed vaccinated population and the other for the
#' unvaccinated. A `tryCatch` validation handles errors when no matches
#' can be generated due to the lack of treated or untreated individuals.
#' The threshold for the maximum number of iterations is the total
#' removed population for the first iteration. The algorithm iterates
#' until no new adjusted pairs are generated or the maximum number of
#' iterations is reached.
#'
#' @inheritParams match_cohort
#' @param all `data.frame` with the entire cohort.
#' @param matched `data.frame` with the matched cohort.
#' @param adjusted `data.frame` with the adjusted cohort to calculate
#' removed cases. Default is NULL, which returns 0.
#' @return `data.frame` with adjusted pairs after iterating.
#' @keywords internal

iterate_match <- function(all,
                          matched,
                          adjusted,
                          outcome_date_col,
                          censoring_date_col,
                          immunization_date_col,
                          vacc_status_col,
                          vaccinated_status,
                          unvaccinated_status,
                          exact,
                          nearest,
                          start_cohort,
                          end_cohort) {

  # Set removed, matched and ajusted for the first iteration
  matched_i <- matched
  adjusted_i <- adjusted
  removed_i <- matched_i[!(matched_i$match_id %in% adjusted_i$match_id), ]

  # Set control parameter to avoid infinte while loop
  im <- 0
  thershold <- nrow(removed_i) #maximum number of iterations: size of removed

  # Iterate until threshold
  # or until the procedure generates zero adjusted population
  while ((nrow(adjusted_i) != 0) && (im < thershold)) { #nolint
    # Iteration for unvaccinated
    new_match <- rematch_(
      all = all,
      adjusted = adjusted,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      immunization_date_col = immunization_date_col,
      removed_i = removed_i,
      vacc_status_col = vacc_status_col,
      rematch_status = unvaccinated_status,
      control_status = vaccinated_status,
      nearest = nearest,
      exact = exact,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      im = im
    )

    adjusted <- new_match$adjusted
    adjusted_iu <- new_match$adjusted_i_s
    matched_iu <- new_match$matched_i_s

    # Iteration for vaccinated
    new_match <- rematch_(
      all = all,
      adjusted = adjusted,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      immunization_date_col = immunization_date_col,
      removed_i = removed_i,
      vacc_status_col = vacc_status_col,
      rematch_status = vaccinated_status,
      control_status = unvaccinated_status,
      nearest = nearest,
      exact = exact,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      im = im
    )

    adjusted <- new_match$adjusted
    adjusted_iv <- new_match$adjusted_i_s
    matched_iv <- new_match$matched_i_s

    # Update data.frames for next iteration
    # matched and adjusted
    list_adj <- list(adjusted_iu, adjusted_iv)
    list_mat <- list(matched_iu, matched_iv)
    adjusted_i <- data.frame()
    matched_i <- data.frame()
    for (i in seq_along(list_adj)) {
      # Selective concat: only concat non-empty data.frames to avoid
      # errors generated by incompatibility in columns
      # Notice that match_cohort_ and adjust_eposition create
      # additional columns
      if (nrow(list_adj[[i]]) > 0) {
        adjusted_i <- rbind(adjusted_i, list_adj[[i]])
      }
      if (nrow(list_mat[[i]]) > 0) {
        matched_i <- rbind(matched_i, list_mat[[i]])
      }
    }

    # New removed registers
    removed_i <- matched_i[!(matched_i$match_id %in% adjusted_i$match_id), ]
    # update iteration number
    im <- im + 1
  }
  return(adjusted)
}
