#' Add a text label to a plot
#'
#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param col Colour of text
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, col = "grey35", ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, col = col, ...)
}

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

gold <- function() 1 / ((1 + sqrt(5))/2)

pal_func <- function(n) {
  viridisLite::plasma(n, begin = 0.00, end = 0.85, direction = -1)
}

pal_func2 <- function(n) {
  viridisLite::viridis(n, begin = 0.0, end = 0.87, direction = -1)
}

ecogram_panel <- function(x,
  pal = function(n) RColorBrewer::brewer.pal(n, "Dark2"),
  year_limits = c(1930, 2010),
  right_gap = 30, xaxes = NULL, stop_lab = 0.77, darken_factor = 1.0,
  label_gap = -1.9, label_cex = 0.85, bottom_frac_up = 0.025, log_y = FALSE,
  ncols = 2, show_seg = TRUE, yfrac_let = 0.08, ymax = NULL,
  lab_text = "", label_side = c("right", "left"), cex.axis = 0.85,
  connector_length = 1.25) {

  x <- dplyr::filter(x, year >= year_limits[1], year <= year_limits[2])

  col <- suppressWarnings(pal(length(unique(x$gram_canonical))))
  col <- rep(col, 99)
  col <- col[seq_len(length(unique(x$gram_canonical)))]
  col <- darken(col, factor = darken_factor)

  pred <- data.frame(select(x, year, total_words))
  pred <- pred[!duplicated(pred), , drop = FALSE] %>%
    arrange(year)

  suppressMessages(library(mgcv))
  sm <- plyr::ddply(x, "gram_canonical", function(xx) {

    this_pred <- dplyr::filter(pred,
      year >= min(xx$year),
      year <= max(xx$year))

    m <- tryCatch({
      mgcv::gam(total ~ s(year), offset = log(total_words), data = xx,
        family = quasipoisson(link = "log"))
    },
      error = function(e) NA)
    if (!is.na(m)[[1]]) {
      p <- predict(m, se.fit = TRUE, newdata = this_pred)
      out <- data.frame(year = this_pred$year,
        y = exp(p$fit),
        ymin = exp(p$fit - 1 * p$se.fit),
        ymax = exp(p$fit + 1 * p$se.fit))
      out$ymin[out$ymax > 10 * out$y] <- out$y[out$ymax > 10 * out$y]
      out$ymax[out$ymax > 10 * out$y] <- out$y[out$ymax > 10 * out$y]
      out$ymin[1:1] <- NA
      out$ymax[1:1] <- NA
    } else {
      warning("GAM failed to fit for '", unique(xx$gram_canonical),
        "'. Reverting to plotting the raw data.", call. = FALSE)
      out <- data.frame(year = xx$year,
        y = xx$total/xx$total_words,
        ymin = NA, ymax = NA)
    }
    out
  })

  lab <- dplyr::group_by(sm, gram_canonical) %>%
    arrange(year) %>%
    summarise(year = year[n()], y = y[n()])

  cols <- group_by(sm, gram_canonical) %>%
    summarise(last_val = y[n()]) %>%
    arrange(last_val) %>%
    mutate(this_col = col)

  sm <- inner_join(sm, cols, by = "gram_canonical")
  x <- inner_join(x, cols, by = "gram_canonical")
  lab <- inner_join(lab, cols, by = "gram_canonical")

  if (is.null(ymax))
    current_max <- max(sm$y)
  else
    current_max <- max(sm$y, ymax)

  lower <- 0
  if (log_y) lower <- 0.1
  ylim <- c(lower, current_max * 1.19)

  log_arg <- ifelse(log_y, "y", "")

  if (label_side[[1]] == "right")
    xlim <- year_limits + c(0, right_gap)
  else
    xlim <- year_limits + c(-right_gap, 0)

  plot(1, 1, type = "n", xlim = xlim, ylim = ylim,
    axes = FALSE, ann = FALSE, yaxs = "i", xaxs = "i",
    log = log_arg)

  abline(v = seq(1950, 2010, 20), col = "grey92", lwd = 0.8)

  uniq <- group_by(x, gram_canonical) %>%
    summarise(first_year = min(year)) %>%
    arrange(first_year) %>%
    pull(gram_canonical)

  max_axis2 <- ifelse(ii %in% seq_len(ncols),
    ylim[2], ylim[2] * stop_lab)
  axis(2, col.ticks = "grey35", col = NA,
    at = pretty(seq(0, max_axis2, length.out = 200), n = 3), cex.axis = cex.axis)

  if (!is.null(xaxes)) {
    if (ii %in% xaxes) {
      axis(1, col.ticks = "grey35", at = seq(1930, 2010, 20),
        padj = -0.4, col = NA, cex.axis = cex.axis, gap.axis = 0.01)
    }
  }

  box(col = "grey45")

  for (i in seq_along(uniq)) {
    raw <- dplyr::filter(x, gram_canonical == uniq[i])
    lines(raw$year, raw$total/raw$total_words, lwd = .85, col = "grey75")
  }
  for (i in seq_along(uniq)) {
    smo <- dplyr::filter(sm, gram_canonical == uniq[i])
    polygon(x = c(smo$year, rev(smo$year)), y = c(smo$ymin, rev(smo$ymax)),
      border = NA, col = paste0(smo$this_col, "35"))
    lines(smo$year, smo$y, col = smo$this_col, lwd = 2.5)
  }

  lab$y_temp <- lab$y
  lab$y_temp[lab$y_temp < current_max * bottom_frac_up] <-
    current_max * bottom_frac_up
  lab$ynew <- suppressWarnings(TeachingDemos::spread.labs(lab$y_temp,
    mindiff = 1.22 * strheight('A'),
    maxiter = 9000, stepsize = 1/500, min = current_max * bottom_frac_up,
    max = current_max * 1.05))

  for (i in seq_along(uniq)) {
    this_lab <- dplyr::filter(lab, gram_canonical == uniq[i])
    text(year_limits[2]+label_gap, this_lab$ynew,
      this_lab$gram_canonical, pos = 4,
      col = this_lab$this_col, cex = label_cex)
  }
  if (show_seg)
    segments(x0 = year_limits[2], x1 = year_limits[2] + connector_length,
      y0 = lab$y, y1 = lab$ynew,
      col = "grey60", lwd = 0.83)

  .letter <- paste0("(", letters[[ii]], ")")
  add_label(xfrac = 0.0, yfrac = yfrac_let, label = .letter, cex = 1.2,
    font = 2)

  u <- par("usr")
  x <- u[1] + 0.0 * (u[2] - u[1]) + strwidth(.letter, font = 2, cex = 1.2) +
    strwidth("  ")
  y <- u[4] - yfrac_let * (u[4] - u[3])
  text(x, y, labels = lab_text, pos = 4, col = "grey35", cex = 1.1)
  ii <<- ii + 1
}

ecogram_panels <- function(dat, right_gap = 50, ncols = 2, csv_out = NULL, ...) {
  npanels <- length(unique(dat$panel))
  nrows <- ceiling(npanels / ncols)
  dat <- dat[!duplicated(dat), , drop = FALSE]
  dat_out <- dat
  dat <- dat %>%
    group_by(year, panel, gram_canonical, total_words) %>%
    summarise(total = sum(total)) %>%
    ungroup() %>%
    arrange(panel, gram_canonical, year)

  if (!is.null(csv_out)) readr::write_csv(dat, csv_out)

  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey35", mar = c(0.025, 1.9, 0, 0), oma = c(1.7, 0.5, .5, .5))
  ii <<- 1
  xaxes <- seq(npanels - (ncols - 1), npanels)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", function(x) {
      ecogram_panel(x, right_gap = right_gap, xaxes = xaxes, ncols = ncols,
      lab_text = as.character(unique(gsub("[0-9]+: ", "", x$panel))),
      ...)})
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.55,
    col = "grey35", cex = 0.85, las = 0)
  invisible(dat_out)
}
