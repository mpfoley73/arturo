#
# Arturo
#

library(shiny)
library(bslib)
library(dplyr)
library(glue)
library(shinychat)
library(ellmer)
library(markdown)

source("setup.R")

ui <- page_navbar(
  title = "Arturo",
  theme = bs_theme(preset = "flatly"),

  nav_spacer(),
  
  nav_panel(
    "Home", 
    page_fluid(
      sidebarLayout(
        sidebarPanel(
          width = 5,
          card(
            card_header("Let me show you around"),
            card_body(glue("Select an exhibit currently on display ",
                           "here at the Cleveland Museum of Art and ",
                           "we can walk around and chat a bit."))),
          selectInput("exhibit", "Open Exhibitions", exhibits$title),
          p(),
          card(height = "550px", uiOutput("chat_win"))
        ),
        mainPanel(
          width = 7,
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
                card_footer(htmlOutput("item_desc"))
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  chat_obj <- reactiveVal(ellmer::chat_openai(model = "gpt-4o-mini"))
  
  item_idx <- reactiveVal(1)
  
  exh_id <- reactive(exhibits |> filter(title == input$exhibit) |> pull(id))

  exh_items <- reactive({ artworks |> filter(exhibition_id == exh_id()) })
  
  curr_item <- reactive({ exh_items()[item_idx(), ] })
  
  observe({ 
    x <- exh_id()
    item_idx(1)
  })
  
  observe({
    curr_title <- curr_item()$title
    curr_desc <- curr_item()$description
    curr_trivia <- curr_item()$did_you_know
    system_prompt <- interpolate_file("system_prompt.md")
    chat_obj()$set_system_prompt(system_prompt)
  })
  
  observeEvent(input$next_img, { item_idx(min(item_idx() + 1, nrow(exh_items()))) })
  
  observeEvent(input$prev_img, { item_idx(max(item_idx() - 1, 1)) })
  
  output$img_seq_desc <- renderText({
    glue("Item {item_idx()} of {nrow(exh_items())} in exhibit.")
  })
  
  output$item_img <- renderUI({
    image_url <- curr_item()$image_url
    tags$img(src = image_url, width = "475px")
  })
  
  output$did_you_know <- renderText({ curr_item()$did_you_know })
  
  output$img_title <- renderText({ curr_item()$title })
  
  output$item_desc <- renderText({ curr_item()$description })
  
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
