Arturo is a Shiny app that uses a chatbot to take you around the [Cleveland Museum of Art](https://www.clevelandart.org/) (CMA). This app is running [here](https://mpfoley73.shinyapps.io/arturo/).

![](resources/arturo.png)

Thanks to CMA's [open access policy](https://www.clevelandart.org/open-access), you can access public-domain artworks from their collection via their API. I pulled currently open exhibit data offline in [/scripts/prep_data.R](https://github.com/mpfoley73/arturo/blob/master/scripts/prep_data.R). The Shiny app uses the data to create a virtual tour. I pipe the artwork metadata into a chat with OpenAI using the **ellmer** package. The app user can engage in a chat. The chat interface is managed by the **shinychat** package.
