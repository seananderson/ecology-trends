library(shiny)
library(tidyverse)
setwd("../")
source("analysis/pretty-panels.R")

# 1. Change this:
# d <- read.csv("data/methods-models.csv",
  # strip.white = TRUE, stringsAsFactors = FALSE, comment.char = "#")
d <- read.csv("data/conservation-terms.csv", strip.white = TRUE, stringsAsFactors = FALSE)
# d <- read.csv("data/paul_human_impacts3.csv", strip.white = TRUE, stringsAsFactors = FALSE)
# d <- read.csv("data/scale_panels_1_nohyphens_18dec.csv",
# strip.white = TRUE, stringsAsFactors = FALSE)

# 2. Change this filename:
# out <- readRDS("data/generated/method-grams.rds")
# out <- readRDS("data/generated/becky-2017-12-14.rds")
# out <- readRDS("data/generated/human-impacts-grams.rds")
out <- readRDS("data/generated/conservation-grams.rds")


# 3. If you don't have a 'show' column in your .csv file (Paul), uncomment:
# d$show <- "yes"

# 4. Click "Run app" in top right of RStudio (after install the shiny package)
# or run runApp('shiny') in the console.

# ------------------------------------------------
# No need to edit below.

# source("analysis/extract-functions.R")

# ------------------------------



# ------------------------------
d$gram <- tolower(d$gram)
# terms <- unique(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/method-grams.rds")

setwd("shiny")
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total))
d <- d[!duplicated(d), ]

grams <- sort(unique(filter(d, !panel %in% "other")$gram_canonical))

temp <- d %>%
  select(-panel) %>%
  unique() %>%
  group_by(gram_canonical, year, total_words) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  group_by(gram_canonical) %>%
  mutate(n_years = length(unique(year))) %>%
  ungroup()

ui <- fluidPage(titlePanel("Ecology n-grams"),
  sidebarLayout(sidebarPanel(
    checkboxGroupInput(
      "gram_input",
      "n-gram",
      choices = grams
    )
  ),
    mainPanel(plotOutput("plot"))))
server <- function(input, output) {
  output$plot <- renderPlot({
    dat <- temp %>%
      filter(gram_canonical %in% input$gram_input)
      par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 1,
        col.axis = "grey55", mar = c(0.025, 1.9, 0, 0), oma = c(1.7, 1.1, .5, .5))
      ii <<- 1
      if (nrow(dat) > 1L) {
      mutate(dat, total_words = total_words/1e5, total = total) %>%
        ecogram_panel(right_gap = 60, xaxes = 1)
      } else {
        plot(1, 1, type = "n", axes = FALSE, ann = FALSE)
        box(col = "grey45")
      }
      mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.05,
        col = "grey45", cex = 0.85, las = 0)
  })
}
shinyApp(ui = ui, server = server)
