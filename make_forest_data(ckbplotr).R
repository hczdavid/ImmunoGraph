#' This code is from ckbplotr package;  https://github.com/neilstats/ckbplotr
#' Prepares data set for a forest plot
#'
#' \code{make_forest_data}
#'
#' @param headings A data frame that contains the headings to be used for the
#'   rows of the plot. The data frame must contain columns 'heading1', 'heading2'
#'   and 'heading3'. Use NA if a lower level heading is not required.
#' @param rows A character vector. The top level headings (heading1) of rows
#'   to be included in the plot.
#' @param cols A list of data frames. These should include columns or point
#'   estimates, and standard errors or confidence interval limits. If you
#'   specify a headings data frame, then they must also all contain a key column
#'   with the same name (which can be specified by col.key).
#' @param colnames A character vector. The titles to be used for each forest plot.
#'   If none provided, then they will be numbered 1, 2, 3 ...
#' @param col.key Name of column that links the headings provided in headings
#'   and the results given in each data frame provided in cols.
#'
#'   If headings data frame is not given, then this column will be used as headings
#'   for each row of the plot.
#'
#'   (Default: "key")
#' @param col.estimate Name of column that provides point estimates.
#'   (Default: "estimate")
#' @param col.stderr Name of column that provides standard errors. (Default: "stderr")
#' @param col.lci Name of column that provides lower limit of confidence intervals.
#' @param col.uci Name of column that provides upper limit of confidence intervals.
#' @param col.pval Currently does nothing.
#' @param col.left A character vector of names of columns to be printed to the left of the plot.
#' @param col.right A character vector of names of columns to be printed to the right of the plot.
#' @param ci.delim Character string to separate lower and upper limits of
#'   confidence interval. (Default: ", ")
#' @param whiteci A list of character vectors. List must be the same length as cols.
#'   Identify the rows (using the key values) for which the CI should be plotted in white. (Default: NULL)
#' @param diamond A list of character vectors. List must be the same length as cols.
#'   Identify the rows (using the key values) for which the estimate and CI should be plotted using a diamond. (Default: NULL)
#' @param bold A list of character vectors. List must be the same length as cols.
#'   Identify the rows (using the key values) for which text should be bold.
#'   If a row has bold text in all cols, then heading will additionally be bold. (Default: NULL)
#' @param boldheadings A character vector. Identify headings (using key values)
#'   which should additionally be bold. (Default: NULL)
#' @param exponentiate Exponentiate estimates (and CIs) before plotting,
#'   use log scale on the axis, and add a line at null effect. (Default: TRUE)
#' @param blankrows A numeric vector of length 4 specifying the number of blank rows
#'   after a heading1, at the end of a heading1 'section', after
#'   a heading2, and at the end of a heading2 'section. (Default: c(1, 1, 0, 0))
#' @param scalepoints Should the points be scaled by inverse of the standard
#'   error? (Default: FALSE)
#' @param addtext A list of data frames. List must be the same length as cols.
#'   Data frames should contain a col.key column, and one or more of:
#'
#'   1. a column named 'text' containing character strings
#'
#'   2. columns named 'het_dof', 'het_stat', and 'het_p' containing character strings
#'
#'   3. columns names 'trend_stat' and 'trend_p' containing character strings
#'
#'   The character strings (for 'text'), heterogeneity or trend test results will
#'   be plotted to the right of each forest plot below the key specified in the
#'   col.key column.
#'
#'
#' @return A dataset from which the plot is generated.
#'
#'
#' @keywords internal
#'

make_forest_data <- function(
  headings     = NULL,
  rows         = NULL,
  cols,
  colnames      = NULL,
  col.key       = "key",
  col.estimate  = "estimate",
  col.stderr    = "stderr",
  col.lci       = NULL,
  col.uci       = NULL,
  col.pval      = NULL,
  col.left      = NULL,
  col.right     = NULL,
  ci.delim      = ", ",
  whiteci       = NULL,
  diamond       = NULL,
  bold          = NULL,
  boldheadings  = NULL,
  exponentiate  = TRUE,
  blankrows     = c(1, 1, 0, 0),
  scalepoints   = FALSE,
  addtext       = NULL
){

  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if (is.null(colnames)) { colnames <- as.character(1:length(cols)) }
  if (!is.character(colnames)) stop("colnames must be a character vector")
  if (!all(!duplicated(colnames))) stop("colnames must be unique")
  if (length(cols) != length(colnames)) stop("cols and colnames must be the same length")
  if (!(length(blankrows == 4) & is.numeric(blankrows))) stop("blankrows must be a length 4 vector")

  # Make vector of keys after which extra rows are added for addtext
  extrarowkeys <- c()
  addtextcols <- tibble::tibble(text = character(),
                                het_dof = character(),
                                het_stat = character(),
                                het_p = character(),
                                trend_stat = character(),
                                trend_p = character())
  if (!is.null(addtext)) {
    for (i in 1:length(addtext)) {
      addtext[[i]] <- dplyr::bind_rows(addtextcols, addtext[[i]]) %>%
        dplyr::mutate(extratext = dplyr::case_when(
          !is.na(text) ~ paste0("'", text, "'"),
          !is.na(het_stat) ~ paste0("paste('Heterogeneity: ', chi[",
                                    het_dof,
                                    "]^2,'=",
                                    het_stat,
                                    " (p=",
                                    het_p,
                                    ")', sep='')"),
          !is.na(trend_stat) ~ paste0("paste('Trend: ', chi[1]^2,'=",
                                      trend_stat,
                                      " (p=",
                                      trend_p,
                                      ")', sep='')")
        )) %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      extratext) %>%
        dplyr::mutate(key = as.character(key))

      extrarowkeys <- c(extrarowkeys, addtext[[i]][["key"]])
    }
  }
  extrarowkeys <- unique(extrarowkeys)

  if (is.null(headings)) {
    out <- cols[[1]] %>%
      dplyr::mutate(Heading = !!rlang::sym(col.key),
                    key = !!rlang::sym(col.key),
                    extrarowkey = "") %>%
      dplyr::select(Heading, key, extrarowkey) %>%
      dplyr::add_row(Heading = "") %>%
      dplyr::mutate(row = 1:dplyr::n())

    # Add extra rows for addtext
    if (!is.null(addtext)) {
      for (k in 1:length(extrarowkeys)) {
        out <- out %>%
          dplyr::add_row(Heading = "", extrarowkey = paste0(extrarowkeys[[k]]),
                         .after = which(out$key == extrarowkeys[[k]]))
      }
    }
    out <- out %>%
      dplyr::mutate(row = 1:dplyr::n())


  } else {

    if (is.null(rows)) stop("argument rows must be given if headings is used")
    if (!col.key %in% names(headings)) stop(paste0(col.key, " must be a column in ",  deparse(substitute(headings))))

    for (col in cols) {
      if (!col.key %in% names(col)) stop(paste0(col.key, " must be a column in every data frame given in cols"))
    }

    for (head1 in rows) {
      if (!(head1 %in% headings$heading1)) {
        stop(paste(head1,"is not in heading1 column of", deparse(substitute(headings))))
      }
    }

    headings <- headings %>%
      dplyr::mutate(heading1 = heading1,
                    heading2 = heading2,
                    heading3 = heading3,
                    key = !!rlang::sym(col.key))

    out <- tibble::tibble(Heading = "", key = "", extrarowkey = "", removelater = TRUE)

    for (head1 in rows) {
      #    print(paste0("now working on: ", head1))
      l2headings <- headings %>%
        dplyr::filter(heading1 == head1) %>%
        dplyr::select(heading2, key) %>%
        dplyr::distinct(heading2, .keep_all = TRUE)

      if (is.na(l2headings[[1, "heading2"]])) {
        if (head1 != "") {out <- tibble::add_row(out, Heading = head1, key = headings %>% dplyr::filter(heading1 == head1) %>% dplyr::pull(key))}

        # Add extra row for addtext
        if (headings[which(headings$heading1 == head1),"key"] %in% extrarowkeys) {
          out <- tibble::add_row(out,
                                 Heading = "",
                                 extrarowkey = headings[[which(headings$heading1 == head1), "key"]])
        }
      }
      else{



        if (head1 != "") {out <- tibble::add_row(out, Heading = head1)}

        if (blankrows[[1]] > 0) {for (i in 1:blankrows[[1]]) { out <- tibble::add_row(out, Heading = "") }}



        for (head2 in 1:nrow(l2headings)) {
          #      print(paste0("now working on: ", l2headings[[head2, "heading2"]]))

          l3headings <- headings %>%
            dplyr::filter(heading1 == head1 & heading2 == l2headings[[head2, "heading2"]]) %>%
            dplyr::select(heading3, key)

          #     print(l3headings)

          if (is.na(l3headings[[1, "heading3"]])) {
            if (head2 != "") {out <- tibble::add_row(out, Heading = l2headings[[head2, "heading2"]], key = l2headings[[head2, "key"]])}

            # Add extra row for addtext
            if (l2headings[[head2, "key"]] %in% extrarowkeys) {
              out <- tibble::add_row(out,
                                     Heading = "",
                                     extrarowkey = l2headings[[head2, "key"]])
            }
          }
          else{
            if (head2 != "") {out <- tibble::add_row(out, Heading = l2headings[[head2, "heading2"]])}
            if (blankrows[[3]] > 0) {for (i in 1:blankrows[[3]]) { out <- tibble::add_row(out, Heading = "") }}
            for (head3 in 1:nrow(l3headings)) {
              out <- tibble::add_row(out,
                                     Heading = l3headings[[head3, "heading3"]],
                                     key = l3headings[[head3, "key"]])

              # Add extra row for addtext
              if (l3headings[[head3, "key"]] %in% extrarowkeys) {
                out <- tibble::add_row(out,
                                       Heading = "",
                                       extrarowkey = l3headings[[head3, "key"]])
              }
            }
          }
          if (blankrows[[4]] > 0) {for (i in 1:blankrows[[4]]) { out <- tibble::add_row(out, Heading = "") }}
        }
      }
      if (blankrows[[2]] > 0) {for (i in 1:blankrows[[2]]) { out <- tibble::add_row(out, Heading = "") }}
    }


    # add a blank heading at bottom if needed
    if (tail(out$Heading, 1) != "") {
      out <- out %>%
        tibble::add_row(Heading = "")
    }

    out <- out %>%
      dplyr::filter(is.na(removelater) | !removelater) %>%
      dplyr::select(-removelater) %>%
      dplyr::mutate(row = 1:dplyr::n())
  }


  # make datatoplot
  datatoplot <- tibble::tibble()

  for (i in 1:length(cols)) {

    if (!is.null(col.lci)) {
      cols[[i]] <- cols[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      lci      = !!rlang::sym(col.lci),
                      uci      = !!rlang::sym(col.uci),
                      !!!rlang::syms(col.right))
    } else {
      cols[[i]] <- cols[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      stderr   = !!rlang::sym(col.stderr),
                      !!!rlang::syms(col.right))

    }

    out1 <- merge(out, cols[[i]], by = "key", all.x = TRUE) %>%
      dplyr::mutate(column = colnames[[i]],
                    linecolour = dplyr::if_else(key %in% whiteci[[i]], "white", "black"),
                    diamond = key %in% diamond[[i]],
                    addbold = key %in% bold[[i]])

    if (!is.null(addtext)){
      out1 <- merge(out1, addtext[[i]], by.x = "extrarowkey", by.y = "key", all.x = TRUE)
    } else {
      out1 <- dplyr::mutate(out1, extratext = as.character(NA))
    }



    datatoplot <- dplyr::bind_rows(datatoplot, out1) %>%
      dplyr::mutate(bold = dplyr::if_else(is.na(estimate) | diamond | addbold, "bold", "plain"))
  }



  if (exponentiate == TRUE) {
    tf       <- exp
    inv_tf   <- log
    scale    <- "log"
  } else {
    tf       <- identity
    inv_tf   <- identity
    scale    <- "identity"
  }

  # Make 'column' a factor, so that facets will be in the correct order
  datatoplot <- datatoplot %>%
    dplyr::mutate(column = factor(column,
                                  levels = colnames,
                                  labels = colnames,
                                  ordered = TRUE))


  # Adding CIs and text to show estimate and CI
  if (!is.null(col.lci)) {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(estimate),
                    lci_transformed = tf(lci),
                    uci_transformed = tf(uci)
      )
    minse <- min((datatoplot$estimate - datatoplot$lci)/1.96, na.rm = TRUE)
    datatoplot$size <- 1.96*minse/(datatoplot$estimate - datatoplot$lci)
  } else {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(estimate),
                    lci_transformed = tf(estimate - 1.96*stderr),
                    uci_transformed = tf(estimate + 1.96*stderr)
      )
    minse <- min(datatoplot$stderr, na.rm = TRUE)
    datatoplot$size <- minse/datatoplot$stderr
  }

  datatoplot <- datatoplot %>%
    dplyr::mutate(textresult = dplyr::case_when(
      !is.na(estimate) & bold == "bold" ~ paste0("bold('",format(round(estimate_transformed, 2), nsmall = 2),
                                                 " (",
                                                 format(round(lci_transformed, 2), nsmall = 2),
                                                 ci.delim,
                                                 format(round(uci_transformed, 2), nsmall = 2),
                                                 ")')"),
      !is.na(estimate) ~ paste0("'",format(round(estimate_transformed, 2), nsmall = 2),
                                " (",
                                format(round(lci_transformed, 2), nsmall = 2),
                                ci.delim,
                                format(round(uci_transformed, 2), nsmall = 2),
                                ")'"),
      !is.na(extratext) ~ extratext,
      TRUE              ~ "''"),
      boldheading = key %in% boldheadings)


  if (!scalepoints) {
    datatoplot$size <- 1
  }

  return(datatoplot)
}




