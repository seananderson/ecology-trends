library(shiny)
library(tidyverse)
setwd("../")
source("analysis/pretty-panels.R")

d <- readRDS("data/generated/d_grams.rds")

# ------------------------------
d$gram <- tolower(d$gram)
# terms <- unique(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/method-grams.rds")

setwd("shiny")
d <- d[!duplicated(d), ]

temp <- filter(d, year > 1929, year < 2011) %>%
  mutate(gram_canonical = gram) %>%
  filter(theme == "tools")

thresh_dat <- group_by(temp, gram) %>% summarise(m = max(total/total_words*1e5))

thresh <- thresh_dat %>%
  pull(m) %>%
  quantile(probs = 0.5) %>% `[[`(1)

temp <- filter(temp, gram %in% filter(thresh_dat, m > thresh)$gram)

grams <- sort(unique(temp$gram_canonical))
themes <- sort(unique(temp$theme))

ui <- fluidPage(titlePanel("Ecology n-grams"),
  sidebarLayout(sidebarPanel(
    checkboxGroupInput(
      "theme_input",
      "theme",
      selected = "conservation",
      choices = themes
    ),
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
      filter(gram_canonical %in% input$gram_input, theme %in% input$theme_input)
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
