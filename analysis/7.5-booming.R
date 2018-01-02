source("analysis/extract-functions.R")
source("analysis/shape_top_decade.R")
source("analysis/pretty-panels.R")

fit_glm <- function(dat, tresh = 20) {
  if (sum(dat[,"total"] > 0) >= tresh) {
    m <- tryCatch({
      glm(total ~ year, offset = log(total_words), data = dat,
        family = quasipoisson(link = "log"))
    },
      error = function(e) NA)
  } else {
    m <- NA
  }
  if(!is.na(m[[1]])[[1]]) exp(coef(m)[["year"]]*10) else NA
}

process_slopes <- function(data, n = 8) {
  sm <- plyr::ddply(data, c("decade", "lemma"), function(xx) {
    r1 <- fit_glm(filter(xx, year %in% 1930:1960))
    r2 <- fit_glm(filter(xx, year %in% 1980:2010))
    tibble(rate_early = r1, rate_late = r2)
  }) %>% as_tibble()

  b1_ <- sm %>% filter(decade == "1940s") %>% top_n(n, -rate_early)
  b2_ <- sm %>% filter(decade == "2000s") %>% top_n(n, rate_late)
  b <- bind_rows(b1_, b2_)
  inner_join(b, data, by = c("decade", "lemma"))
}

plot_boom <- function(dat, right_gap = 15,
  label_cex = 0.85, ...) {
  dat <- dat %>% mutate(gram_canonical = lemma, panel = decade) %>%
    arrange(panel, gram_canonical, year)
  npanels <- length(unique(dat$panel))
  n <- length(unique(dat$panel))
  ncols <- floor(sqrt(n))
  nrows <- ceiling(sqrt(n))
  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey55", mar = c(0.025, 2.1, 0, 0), oma = c(1.7, 1.1, .5, .5))
  ii <<- 1
  xaxes <- seq(npanels - (ncols - 1), npanels)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", function(x) {
      ecogram_panel(x, xaxes = xaxes,
        right_gap = right_gap, label_cex = label_cex, yfrac_let = 0.06,
        lab_text = paste(simple_cap(unique(x$panel)), collapse = ""),
        bottom_frac_up = 0.01, label_gap = -1.0,
        show_seg = TRUE,
        label_side = if(ii %in% 2:3) "left" else "right", ...)
      rect(
        xleft = if(ii %in% 2:3) 1930 else 1980,
        xright = if(ii %in% 2:3) 1960 else 2010,
        ybottom = 0,
        ytop = 500,
        col = "#00000015", border = NA)
    })
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.05,
    col = "grey45", cex = 0.85, las = 0)
}

pop1 <- readRDS("data/generated/pop1-cleaned.rds")
pop2 <- readRDS("data/generated/pop2-cleaned.rds")

p1 <- shape_top_decade(pop1, gram_db = gram_db1, total1 = total1, top = 300)
p2 <- shape_top_decade(pop2, gram_db = gram_db2, total1 = total1, top = 300)

b1 <- process_slopes(p1)
b2 <- process_slopes(p2)

bb <- bind_rows(
  mutate(b1, decade = paste(decade, "1-grams")),
  mutate(b2, decade = paste(decade, "2-grams")))

pdf("figs/booms.pdf", width = 8, height = 5)
plot_boom(filter(bb, lemma != ""), right_gap = 35)
dev.off()
