#' Calculate Risk difference
#'
#' Function to calculate risk difference by unconditional maximum likelihood estimation (Wald)
#' for any given treatment pairs.
#'
#' @param x input data
#'      input data can be one of the following: r x 2 table, vector of numbers from a
#'      contigency table (will be transformed into r x 2 table in row-wise order),
#'      or single factor or character vector that will be combined with y into a table.
#'
#' @param y single factor or character vector that will be combined with x into a table
#'        (default is NULL)
#'
#' @param conf.level confidence level (default is 0.95)
#'
#' @param rev reverse order of "rows", "colums", "both", or "neither" (default)
#'
#' @param correction Yate's continuity correction
#'
#' @param verbose To return more detailed results
#'
#' @return a  list containg a data,measure,p.value,correction
#' @export
#'
#' @examples
#' riskdiff_wald(
#'   x = matrix(c(178, 79, 1411, 1486), 2, 2),
#'   conf.level = 0.95,
#'   rev = c("neither", "rows", "columns", "both"),
#'   correction = FALSE,
#'   verbose = FALSE
#' )
riskdiff_wald <-
  function(x, y = NULL,
           conf.level = 0.95,
           rev = c("neither", "rows", "columns", "both"),
           correction = FALSE,
           verbose = FALSE) {
    if (is.matrix(x) && !is.null(y)) {
      stop("y argument should be NULL")
    }
    if (is.null(y)) {
      x <- epitable(x, rev = rev)
    } else {
      x <- epitable(x, y, rev = rev)
    }
    tmx <- table.margins(x)
    p.exposed <- sweep(tmx, 2, tmx["Total", ], "/")
    p.outcome <- sweep(tmx, 1, tmx[, "Total"], "/")
    Z <- qnorm(0.5 * (1 + conf.level))
    nr <- nrow(x)
    wald <- matrix(NA, nr, 3)
    wald[1, 1] <- 1
    for (i in 2:nr) {
      a <- x[i, 2]
      b <- x[i, 1]
      c <- x[1, 2]
      d <- x[1, 1]

      # point estimate of risk difference
      est <- (a / (a + b)) - (c / (c + d))
      # standard error of risk difference
      se_RD <- sqrt((a * b / (a + b)^3) + (c * d / (c + d)^3))

      ci <- est + c(-1, 1) * Z * se_RD
      wald[i, ] <- c(est, ci)
    }
    pv <- tab2by2.test(x, correction = correction)
    colnames(wald) <- c("estimate", "lower", "upper")
    rownames(wald) <- rownames(x)
    cn2 <- paste(
      "risk difference with",
      paste(100 * conf.level, "%", sep = ""),
      "C.I."
    )
    names(dimnames(wald)) <- c(names(dimnames(x))[1], cn2)

    rr <- list(
      x = x,
      data = tmx,
      p.exposed = p.exposed,
      p.outcome = p.outcome,
      measure = wald,
      conf.level = conf.level,
      p.value = pv$p.value,
      correction = pv$correction
    )
    rrs <- list(
      data = tmx,
      measure = wald,
      p.value = pv$p.value,
      correction = pv$correction
    )

    attr(rr, "method") <- "Unconditional MLE & normal approximation (Wald) CI"
    attr(rrs, "method") <- "Unconditional MLE & normal approximation (Wald) CI"

    if (verbose == FALSE) {
      rrs
    } else {
      rr
    }
  }
