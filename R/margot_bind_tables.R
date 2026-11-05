#' Bind and format domain-specific tables or a single table
#'
#' This function binds a named list of domain-specific tables, or handles a single data frame.
#' It optionally sorts by the E-value bound column, renames columns, highlights rows above a threshold,
#' and groups rows by domain when provided. Works for latex, html, or markdown output.
#' It accepts both `transformed_table` layouts of [margot_plot()]: the default reporting layout,
#' whose bound column is "E-value bound" and whose outcomes are in an "Outcome" column, and the
#' legacy layout, whose bound column is "E_Val_bound" and whose outcomes are row names.
#'
#' @param tables_list a named list of data frames, each a domain table, or a single data frame.
#'   For lists, row names are captured as an "Outcome" column; for data frames, same applies.
#' @param sort_E_val_bound character: one of "none", "asc", or "desc". default "none".
#'   For a list of domain tables, rows are sorted within each domain and the domains keep their order.
#'   Every table in a list must use the same layout (report or legacy, read from the columns
#'   rather than from the spelling of the bound column); mixed layouts stop with an error.
#'   Within one layout, bound columns spelled differently are given the first table's spelling.
#' @param e_val_bound_threshold numeric; threshold for highlighting rows. default 1.1.
#' @param highlight_color character or NULL; background colour for highlighted rows. default "yellow".
#' @param bold logical; whether to bold highlighted rows. default TRUE.
#' @param output_format character; one of "latex", "html", or "markdown". default "markdown".
#' @param rename_cols logical; whether to apply column renaming. default TRUE.
#' @param col_renames named list; mapping new_name = old_name for renaming.
#'   default list("E-Value" = "E_Value", "E-Value bound" = "E_Val_bound").
#' @param rename_ate logical; if TRUE, renames the effect column to "ATE". default FALSE.
#' @param threshold_col character; name of the (pre-rename) column to test and sort by. default "E_Val_bound".
#'   When that column is absent, the first of "E_Val_bound", "E-value bound", "E-Value bound" and
#'   "E-Value Bound" that is present is used.
#' @param kbl_args list; extra arguments for kableExtra::kbl(). default list(booktabs=TRUE, caption="Combined Results", align=NULL).
#'
#' @importFrom dplyr bind_rows arrange desc select everything
#' @importFrom kableExtra kbl row_spec pack_rows
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#' @export
margot_bind_tables <- function(
    tables_list,
    sort_E_val_bound = c("none", "asc", "desc"),
    e_val_bound_threshold = 1.1,
    highlight_color = "yellow",
    bold = TRUE,
    output_format = "markdown",
    rename_cols = TRUE,
    col_renames = list("E-Value" = "E_Value", "E-Value bound" = "E_Val_bound"),
    rename_ate = FALSE,
    threshold_col = "E_Val_bound",
    kbl_args = list(booktabs = TRUE, caption = "Combined Results", align = NULL)) {
  # coerce sort argument
  sort_E_val_bound <- match.arg(sort_E_val_bound)

  # the bound column of either margot_plot() table layout
  bound_candidates <- unique(c(threshold_col, "E_Val_bound", "E-value bound", "E-Value bound", "E-Value Bound"))
  find_bound_col <- function(df) {
    found <- bound_candidates[bound_candidates %in% names(df)]
    if (length(found)) found[[1]] else NA_character_
  }

  # handle single data-frame vs named list
  if (inherits(tables_list, "data.frame")) {
    df <- tables_list
    # ensure outcome column
    if (!"Outcome" %in% names(df)) {
      df <- tibble::rownames_to_column(df, var = "Outcome")
    }
    combined <- df
    domain_mode <- FALSE
  } else {
    # tables from different layouts would bind into split estimate and interval
    # columns, so refuse them. the layout is read from the schema, not from the
    # spelling of the bound column: legacy tables carry `confidence_level` or
    # quantile-headed interval columns such as "2.5 %", report tables neither
    table_layout <- function(df) {
      legacy <- "confidence_level" %in% names(df) || any(grepl("^[0-9.]+ %$", names(df)))
      if (legacy) "legacy" else "report"
    }
    layouts <- vapply(tables_list, table_layout, character(1))
    if (length(unique(layouts)) > 1L) {
      stop(
        "The tables mix the report and legacy layouts of margot_plot(). ",
        "Build every table with the same `table_columns` value.",
        call. = FALSE
      )
    }
    # within one layout, give every bound column the first table's spelling
    table_bound_cols <- vapply(tables_list, find_bound_col, character(1))
    target_bound <- table_bound_cols[!is.na(table_bound_cols)][1]
    if (!is.na(target_bound)) {
      for (i in seq_along(tables_list)) {
        current <- table_bound_cols[[i]]
        if (!is.na(current) && current != target_bound) {
          names(tables_list[[i]])[names(tables_list[[i]]) == current] <- target_bound
        }
      }
    }
    # process list of tables with domain id
    tables_list <- lapply(tables_list, function(df) {
      if (!"Outcome" %in% names(df)) {
        df <- tibble::rownames_to_column(df, var = "Outcome")
      }
      df
    })
    combined <- dplyr::bind_rows(tables_list, .id = "Domain") %>%
      dplyr::select(Domain, Outcome, everything())
    domain_mode <- TRUE
  }

  bound_col <- find_bound_col(combined)
  if (is.na(bound_col)) bound_col <- threshold_col

  # sort by the E-value bound if requested; domains keep their order and
  # contiguous rows, so that the grouped row ranges stay correct
  if (sort_E_val_bound != "none" && !bound_col %in% names(combined)) {
    stop("No E-value bound column found to sort by; set `threshold_col`.", call. = FALSE)
  }
  if (sort_E_val_bound != "none") {
    domain_rank <- if (domain_mode) match(combined$Domain, unique(combined$Domain)) else rep(1L, nrow(combined))
    bound_values <- combined[[bound_col]]
    sort_key <- if (sort_E_val_bound == "asc") bound_values else -bound_values
    combined <- combined[order(domain_rank, sort_key), , drop = FALSE]
    rownames(combined) <- NULL
  }

  # rename ATE column if requested
  if (rename_ate) {
    old_eff <- intersect(c("E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]"), names(combined))
    if (length(old_eff) == 1) {
      names(combined)[names(combined) == old_eff] <- "ATE"
    }
  }

  # apply column renaming map
  if (rename_cols && length(col_renames) > 0) {
    for (new_nm in names(col_renames)) {
      old_nm <- col_renames[[new_nm]]
      idx <- which(names(combined) == old_nm)
      if (length(idx) == 1) names(combined)[idx] <- new_nm
    }
  }

  # determine threshold column name after renaming
  threshold_col_name <- if (bound_col %in% names(combined)) {
    bound_col
  } else {
    renamed <- names(col_renames)[col_renames == bound_col]
    if (length(renamed)) renamed[[1]] else bound_col
  }

  # output for markdown
  if (output_format == "markdown") {
    df_md <- combined
    if (bold && threshold_col_name %in% names(df_md)) {
      hi_rows <- which(as.numeric(df_md[[threshold_col_name]]) > e_val_bound_threshold)
      if (length(hi_rows) > 0) {
        df_md[] <- lapply(df_md, as.character)
        for (r in hi_rows) {
          df_md[r, ] <- lapply(df_md[r, ], function(v) {
            if (nzchar(v) && !grepl("^\\*\\*.*\\*\\*$", v)) {
              paste0("**", v, "**")
            } else {
              v
            }
          })
        }
      }
    }
    align_vec <- if (!is.null(kbl_args$align)) kbl_args$align else rep("l", ncol(df_md))
    return(knitr::kable(
      df_md,
      format    = "markdown",
      caption   = kbl_args$caption,
      row.names = FALSE,
      align     = align_vec,
      col.names = names(df_md),
      escape    = FALSE
    ))
  }

  # output for html or latex
  disp_df <- if ("Domain" %in% names(combined)) {
    combined[, setdiff(names(combined), "Domain")]
  } else {
    combined
  }
  hi_rows <- which(as.numeric(disp_df[[threshold_col_name]]) > e_val_bound_threshold)

  tbl <- do.call(kableExtra::kbl, c(list(disp_df, format = output_format), kbl_args))

  # highlight rows
  if (length(hi_rows) > 0) {
    if (!is.null(highlight_color)) {
      if (output_format == "html") {
        tbl <- tbl %>% kableExtra::row_spec(
          hi_rows,
          background = highlight_color,
          extra_css  = if (bold) "font-weight: bold;" else NULL
        )
      } else {
        tbl <- tbl %>% kableExtra::row_spec(
          hi_rows,
          bold       = bold,
          background = highlight_color
        )
      }
    } else if (bold) {
      css <- if (output_format == "html") "font-weight: bold;" else NULL
      tbl <- tbl %>% kableExtra::row_spec(hi_rows, extra_css = css, bold = bold)
    }
  }

  # pack rows by domain if in domain mode
  if (domain_mode) {
    domain_vec <- combined$Domain
    domains <- unique(domain_vec)
    idx <- data.frame(
      start = vapply(domains, function(x) min(which(domain_vec == x)), integer(1)),
      end   = vapply(domains, function(x) max(which(domain_vec == x)), integer(1))
    )
    for (i in seq_along(domains)) {
      tbl <- try(tbl %>% kableExtra::pack_rows(domains[i], idx$start[i], idx$end[i]), silent = TRUE)
    }
  }

  tbl
}


# margot_bind_tables <- function(
#     tables_list,
#     sort_E_val_bound      = c("none","asc","desc"),
#     e_val_bound_threshold = 1,
#     highlight_color       = "yellow",
#     bold                  = TRUE,
#     output_format         = "markdown",
#     rename_cols           = TRUE,
#     col_renames           = list("E-Value"="E_Value", "E-Value bound"="E_Val_bound"),
#     rename_ate            = FALSE,
#     threshold_col         = "E_Val_bound",
#     kbl_args              = list(booktabs=TRUE, caption="Combined Results by Domain", align=NULL)
# ) {
#   # coerce sort argument
#   sort_E_val_bound <- match.arg(sort_E_val_bound)
#
#   # bring rownames into a column called "Outcome"
#   tables_list <- lapply(tables_list, function(df) {
#     if (!"Outcome" %in% names(df)) {
#       df <- tibble::rownames_to_column(df, var = "Outcome")
#     }
#     df
#   })
#
#   # bind rows with domain identifier
#   combined <- dplyr::bind_rows(tables_list, .id = "Domain") %>%
#     dplyr::select(Domain, Outcome, everything())
#
#   # sort by raw E_Val_bound if requested
#   if (sort_E_val_bound == "asc") {
#     combined <- combined %>% arrange(.data$E_Val_bound)
#   } else if (sort_E_val_bound == "desc") {
#     combined <- combined %>% arrange(desc(.data$E_Val_bound))
#   }
#
#   # 1) rename effect column to "ATE" if requested
#   if (rename_ate) {
#     old_eff <- intersect(c("E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]"), names(combined))
#     if (length(old_eff) == 1) {
#       names(combined)[names(combined) == old_eff] <- "ATE"
#     }
#   }
#
#   # 2) apply column renaming map
#   if (rename_cols && length(col_renames) > 0) {
#     for (new_nm in names(col_renames)) {
#       old_nm <- col_renames[[new_nm]]
#       idx <- which(names(combined) == old_nm)
#       if (length(idx) == 1) {
#         names(combined)[idx] <- new_nm
#       }
#     }
#   }
#
#   # determine final threshold column name
#   threshold_col_name <- if (threshold_col %in% names(combined)) {
#     threshold_col
#   } else {
#     names(col_renames)[col_renames == threshold_col] %||% threshold_col
#   }
#
#   # markdown output: bold rows above threshold
#   if (output_format == "markdown") {
#     df_md <- combined
#     if (bold && threshold_col_name %in% names(df_md)) {
#       hi <- which(as.numeric(df_md[[threshold_col_name]]) > e_val_bound_threshold)
#       if (length(hi) > 0) {
#         df_md[] <- lapply(df_md, as.character)
#         for (r in hi) {
#           df_md[r, ] <- lapply(df_md[r, ], function(v) {
#             if (nzchar(v) && !grepl("^\\*\\*.*\\*\\*$", v)) {
#               paste0("**", v, "**")
#             } else {
#               v
#             }
#           })
#         }
#       }
#     }
#     align_vec <- if (!is.null(kbl_args$align)) {
#       kbl_args$align
#     } else {
#       rep("l", ncol(df_md))
#     }
#     return(knitr::kable(
#       df_md,
#       format    = "markdown",
#       caption   = kbl_args$caption,
#       row.names = FALSE,
#       align     = align_vec,
#       col.names = names(df_md),
#       escape    = FALSE
#     ))
#   }
#
#   # latex/html output
#   domain_vec <- combined$Domain
#   disp_df <- dplyr::select(combined, -Domain)
#   hi_rows    <- which(as.numeric(disp_df[[threshold_col_name]]) > e_val_bound_threshold)
#
#   tbl <- do.call(kableExtra::kbl, c(list(disp_df, format = output_format), kbl_args))
#
#   if (length(hi_rows) > 0) {
#     if (!is.null(highlight_color)) {
#       if (output_format == "html") {
#         tbl <- tbl %>%
#           kableExtra::row_spec(
#             hi_rows,
#             background = highlight_color,
#             extra_css  = if (bold) "font-weight: bold;" else NULL
#           )
#       } else {
#         tbl <- tbl %>%
#           kableExtra::row_spec(
#             hi_rows,
#             bold       = bold,
#             background = highlight_color
#           )
#       }
#     } else if (bold) {
#       css <- if (output_format == "html") "font-weight: bold;" else NULL
#       tbl <- tbl %>%
#         kableExtra::row_spec(hi_rows, extra_css = css, bold = bold)
#     }
#   }
#
#   # pack rows by Domain
#   domains <- unique(domain_vec)
#   idx     <- data.frame(
#     start = vapply(domains, function(x) min(which(domain_vec == x)), integer(1)),
#     end   = vapply(domains, function(x) max(which(domain_vec == x)), integer(1))
#   )
#   for (i in seq_along(domains)) {
#     tbl <- try(tbl %>% kableExtra::pack_rows(domains[i], idx$start[i], idx$end[i]), silent = TRUE)
#   }
#
#   tbl
# }
