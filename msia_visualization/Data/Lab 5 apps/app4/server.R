shinyServer(function(input, output) {
  
  # Define datasetInput as a reactive expression
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # The output$caption is computed based on a reactive 
  # expression that returns input$caption
  output$caption <- renderText({
    input$caption })
  
  # The output$summary depends on the datasetInput reactive expression
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset) })
  
  # The output$view depends on both the databaseInput reactive expression and input$obs
  output$view <- renderTable({
    head(datasetInput(), n = input$obs) }) })

