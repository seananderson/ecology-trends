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

ecogram_panel <- function(x,
  pal = function(n) RColorBrewer::brewer.pal(n, "Dark2"),
  year_limits = c(1930, 2010),
  right_gap = 30, xaxes = NULL, stop_lab = 0.7, darken_factor = 1.0,
  label_gap = -1.9, label_cex = 0.85, bottom_frac_up = 0.025, log_y = FALSE,
  show_seg = FALSE, yfrac_let = 0.08, ymax = max(x$total/x$total_words),
  lab_text = "") {

  x <- dplyr::filter(x, year >= year_limits[1], year <= year_limits[2])

  col <- suppressWarnings(pal(length(unique(x$gram_canonical))))
  col <- rep(col, 99)
  col <- col[seq_len(length(unique(x$gram_canonical)))]
  col <- darken(col, factor = darken_factor)

  pred <- data.frame(select(x, year, total_words))
  pred <- pred[!duplicated(pred), ] %>%
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
      out$ymin[1:1] <- NA
      out$ymax[1:1] <- NA
    } else {
      warning(paste0("GAM failed to fit for '", unique(xx$gram_canonical),
        "'. Plotting the raw data."))
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

  # current_max <- max(c(x$total/x$total_words, sm$y))
  # current_max <- max(sm$y)
  current_max <- ymax
  lower <- 0
  if (log_y) lower <- 0.1
  ylim <- c(lower, current_max * 1.06)

  log_arg <- ifelse(log_y, "y", "")
  plot(1, 1, type = "n", xlim = year_limits + c(0, right_gap), ylim = ylim,
    axes = FALSE, ann = FALSE, yaxs = "i", xaxs = "i",
    log = log_arg)

  abline(v = seq(1950, 2010, 20), col = "grey95", lwd = 0.8)

  uniq <- group_by(x, gram_canonical) %>%
    summarise(first_year = min(year)) %>%
    arrange(first_year) %>%
    pull(gram_canonical)

  max_axis2 <- ifelse(ii %in% c(1, 2), current_max, current_max*stop_lab)
  axis(2, col.ticks = "grey80", col = NA,
    at = pretty(seq(0, max_axis2, length.out = 200), n = 3))

  if (!is.null(xaxes)) {
    if (ii %in% xaxes) {
      axis(1, col.ticks = "grey80", at = seq(1930, 2010, 20),
        padj = -0.2, col = NA)
    }
  }

  box(col = "grey65")

  for (i in seq_along(uniq)) {
    raw <- dplyr::filter(x, gram_canonical == uniq[i])
    lines(raw$year, raw$total/raw$total_words, lwd = .85, col = "grey80")
  }
  for (i in seq_along(uniq)) {
    smo <- dplyr::filter(sm, gram_canonical == uniq[i])
    polygon(x = c(smo$year, rev(smo$year)), y = c(smo$ymin, rev(smo$ymax)),
      border = NA, col = paste0(smo$this_col, "35"))
    lines(smo$year, smo$y, col = smo$this_col, lwd = 2.75)
  }

  # abline(v = year_limits[2]+0, col = "grey70", lwd = 1)
  # rect(xleft = year_limits[2]+0, ybottom = -1,
  #   ytop = current_max * 1.3, xright = 2100, col = "white",
  #   border = NA)

  lab$y_temp <- lab$y
  lab$y_temp[lab$y_temp < current_max * bottom_frac_up] <- current_max * bottom_frac_up
  lab$ynew <- suppressWarnings(TeachingDemos::spread.labs(lab$y_temp,
    mindiff = 1.2 * strheight('A'),
    maxiter = 5000, stepsize = 1/500, min = current_max * bottom_frac_up,
    max = current_max * 1.05))

  for (i in seq_along(uniq)) {
    this_lab <- dplyr::filter(lab, gram_canonical == uniq[i])
    text(year_limits[2]+label_gap, this_lab$ynew, this_lab$gram_canonical, pos = 4,
      col = this_lab$this_col, cex = label_cex)
  }
  if (show_seg)
    segments(x0 = year_limits[2], x1 = year_limits[2]+1.2, y0 = lab$y, y1 = lab$ynew,
      col = "grey75", lwd = 0.65)
  # mtext(unique(x$panel), side = 3, line = -2, cex = 0.8, adj = 0)
  add_label(xfrac = 0.0, yfrac = yfrac_let, label = LETTERS[[ii]], cex = 1.2)

  u <- par("usr")
  x <- u[1] + 0.0 * (u[2] - u[1]) + strwidth(LETTERS[[ii]]) + strwidth("  ")
  y <- u[4] - yfrac_let * (u[4] - u[3])
  text(x, y, labels = lab_text, pos = 4, col = "grey50", cex = 1.2)
  ii <<- ii + 1
}

ecogram_panels <- function(dat, right_gap = 50, ncols = 2, ...) {
  npanels <- length(unique(dat$panel))
  nrows <- ceiling(npanels / ncols)
  dat <- dat[!duplicated(dat), ]
  dat <- dat %>%
    group_by(year, panel, gram_canonical, total_words) %>%
    summarise(total = sum(total)) %>%
    ungroup() %>%
    arrange(panel, gram_canonical, year)
  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey55", mar = c(0.025, 1.9, 0, 0), oma = c(1.7, 1.1, .5, .5))
  ii <<- 1
  xaxes <- seq(npanels - (ncols - 1), npanels)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", ecogram_panel, right_gap = right_gap, xaxes = xaxes,
      ...)
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.05,
    col = "grey45", cex = 0.85, las = 0)
}
