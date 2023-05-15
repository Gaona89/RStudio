# Dashboards

library(shiny)
library(ggplot2)


# Define la UI
ui <- shinyUI(fluidPage(
  titlePanel("Transfencias SIPAP"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x", "Seleccione el valor de X:",
                  min = -1, max = 10, value = 1)
    ),
    mainPanel(
      plotOutput("grafico"),
      plotOutput("grafico2")
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
  
  output$grafico2 <- renderPlot({
    
    ggplot(suma_por_añoG, aes( xd, año)) + 
      geom_point() + labs(y = "Año Mes", x = "Importe destino en miles de millones", 
       title = "Transferencias por montos y Moneda Guaranies")+

      xlim(1, input$x)
  })
})
# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
