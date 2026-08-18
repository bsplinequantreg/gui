# inst/shiny/app1.R

# Ce fichier est un wrapper pour app.R
# Il permet de lancer App1 (version stable)

source("app1/app.R")

shiny::shinyApp(ui = ui, server = server)
