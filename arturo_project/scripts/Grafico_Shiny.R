# Dashboards

library(shiny)
library(ggplot2)


# Define la UI
ui <- shinyUI(fluidPage(
  titlePanel("Transfencias SIPAP"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x", "Seleccione el valor de X:",
                  min = 0, max = 6, value = 1)
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
))

# Define el servidor
server <- shinyServer(function(input, output) {
  output$grafico <- renderPlot({
    
    # Carga los datos
    ggplot(suma_por_año, 
           aes(xd, año)) + geom_point() +
          labs(y = "Año Mes", x = "Importe destino en millones",
           title = "Transferencias por montos y Moneda Euro") +
      xlim(1, input$x)
     # ylim(a, a)
  })
})
# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
