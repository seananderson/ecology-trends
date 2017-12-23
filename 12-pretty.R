
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

ecogram_panel <- function(x,
  pal = "Set2", year_limits = c(1930, 2010),
  right_gap = 30, xaxes = NULL) {

  # per <- 1e8
  x <- dplyr::filter(x, year >= year_limits[1], year <= year_limits[2])
  col <- RColorBrewer::brewer.pal(8L, pal)[-(5:7)]

  darken <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
  }

  col <- darken(col, factor = 1.04)


    # col = RColorBrewer::brewer.pal(8L, "Dark2")

    pred <- data.frame(select(x, year, total_words))
  pred <- pred[!duplicated(pred), ] %>%
    arrange(year)

  # browser()

  library(mgcv)
  sm <- plyr::ddply(x, "gram_canonical", function(xx) {

    this_pred <- dplyr::filter(pred,
      year >= min(xx$year),
      year <= max(xx$year))
    m <- tryCatch({
      # mgcv::gam(log(total/total_words) ~ s(year), data = xx,
      # family = gaussian())

      mgcv::gam(total ~ s(year), offset = log(total_words), data = xx,
        family = nb(link = "log"))
    },
      error = function(e) NA)
    if (!is.na(m)[[1]]) {
      out <- data.frame(year = this_pred$year,
        y = exp(predict(m, newdata = this_pred)))
    } else {
      out <- data.frame(year = x$year,
        y = x$total/per)
    }
    out
  })

  current_max <- max(c(x$total/x$total_words, sm$y))
  # mult <- ifelse(max(lab$ynew, na.omit = TRUE) * 1.02 > max(current_max), 1.2, 1.02)
  # ylim <- c(0, max(current_max, lab$ynew, na.omit = TRUE) * mult)
  ylim <- c(0, current_max * 1.06)
  # ylim = c(0, max(sm$y))

  plot(1, 1, type = "n", xlim = year_limits + c(0, right_gap), ylim = ylim,
    axes = FALSE, ann = FALSE, yaxs = "i", xaxs = "i")

  abline(v = seq(1950, 1990, 20), col = "grey95", lwd = 0.8)

  uniq <- sort(unique(x$gram_canonical))
  for (i in seq_along(uniq)) {
    raw <- dplyr::filter(x, gram_canonical == uniq[i])
    lines(raw$year, raw$total/raw$total_words, lwd = .95, col = "grey75")
    # paste0(col[i], "95")
  }
  for (i in seq_along(uniq)) {
    smo <- dplyr::filter(sm, gram_canonical == uniq[i])
    lines(smo$year, smo$y, col = col[i], lwd = 3)
  }

  abline(v = year_limits[2]+0, col = "grey70", lwd = 1)
  rect(xleft = year_limits[2]+0, ybottom = -1,
    ytop = current_max * 1.3, xright = 2100, col = "grey98",
    border = NA)


  lab <- dplyr::group_by(sm, gram_canonical) %>%
    arrange(year) %>%
    summarise(year = year[n()], y = y[n()])
  lab$y_temp <- lab$y
  lab$y_temp[lab$y_temp < current_max * 0.04] <- current_max * 0.04

  lab$ynew <- suppressWarnings(TeachingDemos::spread.labs(lab$y_temp,
    mindiff = 1.2 * strheight('A'),
    maxiter = 5000, stepsize = 1/500, min = current_max * 0.04,
    max = current_max * 1.05))

  text(year_limits[2]-1.9, lab$ynew, lab$gram_canonical, pos = 4,
    col = col[seq_along(uniq)], cex = 0.85)
  # segments(x0 = year_limits[2], x1 = year_limits[2]+1.5, y0 = lab$y, y1 = lab$ynew,
  # col = "#00000040", lwd = 0.5)
  # mtext(unique(x$panel), side = 3, line = -2, cex = 0.8, adj = 0)
  add_label(xfrac = 0.01, yfrac = 0.1, label = LETTERS[[ii]], cex = 1.1)


  axis(2, col.ticks = "grey80", col = NA,
    at = pretty(seq(0, current_max*0.7, length.out = 200), n = 3))

  if (!is.null(xaxes)) {
    if (ii %in% xaxes) {
      axis(1, col.ticks = "grey80", at = seq(1930, 2010, 20),
        padj = -0.2, col = NA)
    }
  }
  ii <<- ii + 1
  box(col = "grey65")
}

library(dplyr)
d <- read.csv("data/methods-models.csv", strip.white = TRUE,
  stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/method-grams.rds")
out <- readRDS("data/generated/method-grams.rds")

d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total)) %>%
  dplyr::filter(show == "yes")
d <- d[!duplicated(d), ]

d <- d %>%
  group_by(year, panel, gram_canonical, total_words) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  arrange(panel, gram_canonical, year)

# plot(1, 1, xlim = c(1935, 2010), ylim = c(0, 1))
# count_0 <- function(x)
#   unlist(lapply(x, function(xx) length(strsplit(xx, split = " ")[[1]])))
#
# d <- mutate(d, sw = strwidth(gram_canonical), n0 = count_0(gram_canonical))
# d <- mutate(d, gram_canonical =
# ifelse(n0 > 1, sub(" ", "\\\n", gram_canonical),
# gram_canonical))

# widest <- max(strwidth(d$gram_canonical))

# d <- mutate(d, gram_canonical =
# sub("significant difference", "significant\\\ndifference", gram_canonical))

# d <- mutate(d, gram_canonical =
# sub("random-effects model", "random-effects\\\nmodel", gram_canonical))

pdf("figs/stats.pdf", width = 6, height = 5)
par(mfrow = c(3, 2))
par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
  col.axis = "grey55", mar = c(0, 1.9, 0, 0), oma = c(1.7, 1.1, .5, .5))
ii <<- 1
xaxes <- c(5, 6)
mutate(d, total_words = total_words/1e5, total = total) %>%
  plyr::d_ply("panel", ecogram_panel, right_gap = 50, xaxes = xaxes)
mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.05,
  col = "grey45", cex = 0.85, las = 0)
dev.off()
