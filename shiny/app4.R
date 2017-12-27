library(shiny)
library(tidyverse)
setwd("../")
source("analysis/pretty-panels.R")

d <- readRDS("data/generated/d_grams.rds")
d$notes <- NULL
d$subpanel <- NULL
d$total_words <- NULL

out <- readRDS("data/generated/method-grams.rds") %>%
  mutate(theme = "tools") %>% select(-total_words)
d <- bind_rows(d, out)
out <- readRDS("data/generated/conservation-grams.rds") %>%
  mutate(theme = "conservation") %>% select(-total_words)
d <- bind_rows(d, out)
out <- readRDS("data/generated/human-impacts-grams.rds") %>%
  mutate(theme = "human-impacts") %>% select(-total_words)
d <- bind_rows(d, out)
out <- readRDS("data/generated/becky-2017-12-14.rds") %>%
  mutate(theme = "scale") %>% select(-total_words)
d <- bind_rows(d, out)

d$gram <- tolower(d$gram)
d <- d %>% mutate(gram = gsub("-", " ", gram))
# terms <- unique(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/method-grams.rds")

setwd("shiny")
d <- d[!duplicated(d), ]

temp <- filter(d, year >= 1930, year <= 2010) %>%
  mutate(gram_canonical = gram) %>%
  group_by(year, theme, gram_canonical) %>%
  summarize(total = sum(total)) %>%
  ungroup()

temp <- plyr::ddply(temp, c("theme", "gram_canonical"), function(x) {
  all_years <- data.frame(year = seq(min(x$year), max(x$year)),
    gram_canonical = x$gram_canonical[[1]], stringsAsFactors = FALSE,
    theme = x$theme[[1]])
  x <- left_join(all_years, x, by = c("year", "gram_canonical", "theme"))
  x$total[is.na(x$total)] <- 0
  x
})

x <- readRDS("../data/generated/method-grams.rds") %>%
  select(year, total_words) %>%
  unique()

temp <- inner_join(temp, x, by = "year")

themes <- sort(unique(temp$theme))

ui <- fluidPage(titlePanel("n-grams"),
  sidebarLayout(sidebarPanel(
    uiOutput("choose_theme"),
    uiOutput("choose_sort"),
    uiOutput("choose_thresh"),
    uiOutput("choose_terms")
  ),
    mainPanel(plotOutput("plot"))))
server <- function(input, output) {

  output$choose_thresh <- renderUI({
    sliderInput("thresh", "Quantile threshold", 0, 0.9, 0.25, step = 0.05)
  })

  output$choose_theme <- renderUI({
    selectInput("theme", "Theme", as.list(themes))
  })

  output$choose_sort <- renderUI({
    selectInput("sort", "Sorting",
      c("Alphabetical", "Max value", "Big in 1940s", "Big in 2000s"),
      selected = "Max value")
  })

  # Check boxes
  output$choose_terms <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$theme))
      return()

    thresh_dat <- filter(temp, theme %in% input$theme) %>%
      group_by(gram_canonical) %>%
      summarise(m = max(total/total_words*1e5),
        big_early = sum(total[year < 1950]),
        big_late = sum(total[year >= 2000])) %>%
      mutate(max_round = round(m, 1))
    thresh <- thresh_dat %>%
      pull(m) %>%
      quantile(probs = input$thresh) %>% `[[`(1)
    dat <- inner_join(temp, thresh_dat, by = "gram_canonical")

    # Get the data set with the appropriate name
    dat <- filter(dat, gram_canonical %in% filter(thresh_dat, m > thresh)$gram_canonical,
      theme %in% input$theme)
    grams <- unique(select(dat, gram_canonical, max_round, m, big_early, big_late))
    if (input$sort == "Alphabetical")
      grams <- arrange(grams, gram_canonical)
    if (input$sort == "Max value")
      grams <- arrange(grams, -m)
    if (input$sort == "Big in 1940s")
      grams <- arrange(grams, -big_early)
    if (input$sort == "Big in 2000s")
      grams <- arrange(grams, -big_late)

    checkboxGroupInput(
      "gram_input",
      "n-gram",
      choiceNames = paste0(grams$gram_canonical, " (", grams$max_round, ")"),
      choiceValues = grams$gram_canonical
    )
  })

  output$plot <- renderPlot({
    dat <- temp %>%
      filter(gram_canonical %in% input$gram_input, theme %in% input$theme)
    par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 1.0,
      col.axis = "grey55", mar = c(0.025, 1.9, 0, 0), oma = c(1.7, 1.1, .5, .5))
    ii <<- 1
    if (nrow(dat) > 1L) {
      mutate(dat, total_words = total_words/1e5, total = total) %>%
        ecogram_panel(right_gap = 30, xaxes = 1, label_gap = -0.8, label_cex = 1,
          pal = function(n) RColorBrewer::brewer.pal(n, "Dark2"))
    } else {
      plot(1, 1, type = "n", axes = FALSE, ann = FALSE)
      box(col = "grey45")
    }
    mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = 0,
      col = "grey45", cex = 1.0, las = 0)
  })
}
shinyApp(ui = ui, server = server)
