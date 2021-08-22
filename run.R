pacman::p_load(shiny)
port <- Sys.getenv('PORT')
shiny::runApp(
    appDir = "app.R",
    # host = '0.0.0.0',
    # port = as.numeric(port)
)
