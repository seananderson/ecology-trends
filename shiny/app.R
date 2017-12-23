library(shiny)
library(tidyverse)
setwd("../")

# 1. Change this:
d <- read.csv("data/methods-models.csv",
  strip.white = TRUE, stringsAsFactors = FALSE, comment.char = "#")
# d <- read.csv("data/conservation-terms.csv", strip.white = TRUE, stringsAsFactors = FALSE)
# d <- read.csv("data/paul_human_impacts3.csv", strip.white = TRUE, stringsAsFactors = FALSE)
# d <- read.csv("data/scale_panels_1_nohyphens_18dec.csv",
  # strip.white = TRUE, stringsAsFactors = FALSE)

# 2. Change this filename:
out <- readRDS("data/generated/method-grams.rds")
# out <- readRDS("data/generated/becky-2017-12-14.rds")
# out <- readRDS("data/generated/human-impacts-grams.rds")
# out <- readRDS("data/generated/conservation-grams.rds")

# 3. If you don't have a 'show' column in your .csv file (Paul), uncomment:
# d$show <- "yes"

# 4. Click "Run app" in top right of RStudio (after install the shiny package)
# or run runApp('shiny') in the console.

# ------------------------------------------------
# No need to edit below.

# source("analysis/extract-functions.R")

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
  ungroup() %>%
  filter(year <= 2011, year > 1930, n_years > 8)

lab <- plyr::ddply(temp, c("gram_canonical"), function(x) {
  xx <- filter(x, year <= 2010, year > 1930)
  m <- mgcv::gam(total / total_words * 10000 ~ s(year), data = xx)
  data.frame(year = 2010, y = predict(m, newdata = data.frame(year = 2010))[[1]])
})

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
    lab_local <- filter(lab, gram_canonical %in% input$gram_input)
    temp %>%
      filter(gram_canonical %in% input$gram_input) %>%
      ggplot(
        aes(
          year,
          total / total_words * 10000,
          group = gram_canonical,
          colour = gram_canonical
        )
      ) +
      guides(colour = FALSE) +
      geom_line(alpha = 0.5, colour = "grey30") +
      stat_smooth(
        method = "gam",
        formula = y ~ s(x),
        se = FALSE,
        method.args = list(family = gaussian(link = "identity")),
        lwd = 1.5
      ) +
      ggsidekick::theme_sleek() +
      scale_color_brewer(palette = "Set2") +
      ggrepel::geom_text_repel(
        data = lab_local,
        aes(label = gram_canonical, y = y),
        size = 3.5,
        nudge_x = 10,
        segment.size = 0.2,
        segment.color = "#00000030"
      ) +
      ylab("Instances per 10,000 words") + xlab("") +
      scale_x_continuous(breaks = seq(1920, 2012, 20),
        limits = c(1935, 2035))
  })
}
shinyApp(ui = ui, server = server)
