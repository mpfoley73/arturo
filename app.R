# Arturo
# Shiny app that displays artwork from Cleveland Museum of Art (CMA) collection.
# Chatbot pairs artwork metadata with user messages.
# 
# CMA API documenation: https://openaccess-api.clevelandart.org/
# Published to www.shinyapps.io (log in with GitHub)
# Live URL: https://mpfoley73.shinyapps.io/arturo/
# Created 2/9/2025

library(shiny)
library(bslib)
library(shinyjs)  # to disable action buttons
library(dplyr)
library(glue)
library(shinychat)
library(ellmer)
library(markdown)
library(httr2)
library(tidyverse)

## -----------------------------------------------------------------------------
# Setup
## -----------------------------------------------------------------------------

# API does not always provide a URL for artwork. `EMPTY_IMAGE` is a 1x1px 
# empty image to display. Consider using a placeholder image instead.
EMPTY_IMAGE <- "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="

get_open_exhibits <- function() {
  # Gets dataframe of exhibitions currently open at CMA.
  # Example API request: 
  # https://openaccess-api.clevelandart.org/api/exhibitions/?opened_before=2025-08-08&closed_after=2025-08-08&venures=The%20Cleveland%20Museum%20of%20Art
  today_str <- as.character(Sys.Date())
  
  request("https://openaccess-api.clevelandart.org/api/exhibitions/") |>
    req_url_query(
      opened_before = today_str,
      closed_after = today_str,
      venues = "The Cleveland Museum of Art"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("data") |>
    filter(is_venue_cma == TRUE) |>
    arrange(desc(opening_date))
}

get_artworks_for_exhibit <- function(exhibition_id) {
  # Get dataframe of artworks for specified exhibition.
  # Example API request: 
  # https://openaccess-api.clevelandart.org/api/artworks/?exhibition_id=681860
  res <- 
    request("https://openaccess-api.clevelandart.org/api/artworks/") |>
    req_url_query(exhibition_id = exhibition_id) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("data")
  
  # Possible to have no associated artwork in API.
  if (length(res) == 0) {
    return(tibble(
      title = "(no artwork available)",
      description = "The CMA API returned no artwork for this exhibit.",
      did_you_know = "The CMA API does not contain all exhibited artwork."
    ))
  }
  
  artworks <- tibble::as_tibble(res)
  # print(colnames(artworks))
  
  # images col is a dataframe column with columns for web, print, and full size
  # images. Need to unnest it to get inside.
  artwork_images <- 
    artworks |>
    select(id, images) |>
    unnest(images)
  
  # images might be an empty dataframe (1 row with 0 cols). If there is a `web`
  # col, use its URL. Otherwise, manually create the col.
  if ("web" %in% colnames(artwork_images)) {
    artwork_urls <-
      artwork_images |>
      unnest(web) |>
      select(id, artwork_url = url)
  } else {
    artwork_urls <-
      artwork_images |>
      mutate(url = NA_character_) |>
      select(id, artwork_url = url)
  }

  # If there is a url, use it, otherwise, create an empty image url.
  artworks |>
    left_join(artwork_urls, by = "id") |>
    mutate(artwork_url = coalesce(artwork_url, EMPTY_IMAGE))
}


## -----------------------------------------------------------------------------
# UI
## -----------------------------------------------------------------------------

ui <- page_navbar(

  title = "Arturo",
  theme = bs_theme(preset = "flatly"),

  nav_spacer(),
  
  nav_panel(
    "Home", 
    page_fluid(
      useShinyjs(),  # initialize shinyjs
      sidebarLayout(
        sidebarPanel(
          width = 5,
          card(
            card_header("Let me show you around"),
            card_body(glue("Select an exhibit currently on display ",
                           "here at the Cleveland Museum of Art and ",
                           "we can walk around and chat a bit."))),
          uiOutput("exhibit_selector"),
          p(),
          card(height = "550px", uiOutput("chat_win"))
        ),
        mainPanel(
          width = 7,
          div(style = "width: 700px; margin: auto", h2(textOutput("exhibit_title"))),
          div(style = "width: 700px; margin: auto", h3(textOutput("img_title"))),
          p(),
          div(style = "width: 700px; margin: auto", fluidRow(
            column(
              width = 12,
              p(textOutput("img_seq_desc")),
              fluidRow(
                column(4, actionButton("prev_img", "Prevous", width = "130px")),
                column(2, actionButton("next_img", "Next", width = "130px"))
              ), 
              p(),
              card(
                card_body(uiOutput("item_img")),
                card_footer(
                  htmlOutput("item_desc"),
                  p(),
                  htmlOutput("tombstone"),
                  htmlOutput("url")
                )
              )
            )
          )),
          div(style = "width: 700px; margin: auto", fluidRow(
            column(4, imageOutput("arturo_icon", height = "160px")),
            column(8, card(
              card_header("Did you know?"),
              card_body(htmlOutput("did_you_know"))))
          ))
        )
      )
    )
  ),
  
  nav_panel(
    "Info",
    page_fluid(
      card(
        card_header("About Arturo"),
        card_body(includeMarkdown("info.md"))
      )
    )
  )
)

## -----------------------------------------------------------------------------
# Server
## -----------------------------------------------------------------------------

server <- function(input, output) {
  
  chat_obj <- reactiveVal(
    ellmer::chat_openai(
      system_prompt = interpolate_file("system_prompt.md"),
      model = "gpt-4o-mini"
    )
  )
  
  exhibits <- reactiveVal()
  selected_exhibit <- reactiveVal()
  artworks <- reactiveVal()

  artwork_index <- reactiveVal(1)
  
  # Load exhibits and initialize
  observe({
    open_exhibits <- get_open_exhibits()
    exhibits(open_exhibits)
    
    # Default to most recently opened exhibit
    recent_exhibit <- open_exhibits[1, ]
    selected_exhibit(recent_exhibit)
    
    # Load its artworks
    artworks_for_exhibit <- get_artworks_for_exhibit(recent_exhibit$id)
    artworks(artworks_for_exhibit)
  })
  
  # UI for exhibition dropdown
  output$exhibit_selector <- renderUI({
    req(exhibits())
    selectInput(
      "exhibit_choice", "Choose an Exhibit:",
      choices = setNames(exhibits()$id, exhibits()$title),
      selected = selected_exhibit()$id)
  })

  # Update on exhibit selection
  observeEvent(input$exhibit_choice, {
    new_exhibit <- exhibits() |> filter(id == input$exhibit_choice)
    selected_exhibit(new_exhibit)
    artworks(get_artworks_for_exhibit(new_exhibit$id))
  })
  
  # Show exhibit title
  output$exhibit_title <- renderText({
    shiny::req(selected_exhibit())
    selected_exhibit()$title
  })

  curr_item <- reactive({
      artworks()[artwork_index(), ]
  })
  
  observe({
    curr_title <- curr_item()$title
    curr_desc <- curr_item()$description
    curr_trivia <- curr_item()$did_you_know
    curr_artwork_url <- curr_item()$artwork_url
    shinyjs::toggleState(
      "prev_img", 
      condition = artwork_index() > 1 # enabled if
    )
    shinyjs::toggleState(
      "next_img", 
      condition = artwork_index() < nrow(artworks())
    )
    # Manually update chat to include artwork in context.
    new_user_turn <- ellmer::Turn(
      "user", 
      contents = list(ellmer::ContentText("What is this?")))
    artwork_json <- jsonlite::toJSON(curr_item())
    new_system_turn_str <- interpolate_file("new_artwork_turn.md")
    new_system_turn <- ellmer::Turn(
      "assistant", 
      contents = list(ellmer::ContentText(new_system_turn_str)))
    chat_obj()$add_turn(user = new_user_turn, system = new_system_turn)
  })
  
  observeEvent(input$next_img, { 
    artwork_index(min(artwork_index() + 1, nrow(artworks()))) 
  })
  
  observeEvent(input$prev_img, { 
    artwork_index(max(artwork_index() - 1, 1)) 
  })
  
  output$img_seq_desc <- renderText({
    total <- nrow(artworks())
    glue("Item {artwork_index()} of {total} in exhibit.")
  })
  
  output$item_img <- renderUI({
    artwork_url <- curr_item()$artwork_url
    img_width = if_else(artwork_url == EMPTY_IMAGE, "1px", "475px")
    tags$img(src = artwork_url, width = img_width)
  })
  
  output$did_you_know <- renderText({ curr_item()$did_you_know })
  output$img_title <- renderText({ curr_item()$title })
  output$item_desc <- renderText({ curr_item()$description })
  output$tombstone <- renderText({ curr_item()$tombstone })
  output$url <- renderText({ 
    glue('<a href="{curr_item()$url}" target="_blank">{curr_item()$url}</a>') })
  
  output$arturo_icon <- renderImage({
    list(src = "resources/arturo.png", height = "150px")
  }, deleteFile = FALSE)
  
  output$chat_win <- renderUI({
    shinychat::chat_ui("chat_sc", placeholder = "Ask Arturo")
  })
  
  observeEvent(input$chat_sc_user_input, {
    stream <- chat_obj()$stream_async(input$chat_sc_user_input)
    shinychat::chat_append("chat_sc", stream)
  })
  
}

shinyApp(ui = ui, server = server)
