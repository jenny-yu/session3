library(shiny)
ui <- fluidPage(
  titlePanel("Challenge example"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = "choose one:",
                  choices = list("None" = 0, "Bunny" = 1, "Fish" = 2, "Cat" = 3)
      ),
      sliderInput("slider", label = "Image height",
                  min = 1, max = 1000, value = 150),
      textOutput("display")
    ),
    mainPanel (uiOutput("image"))
  )
)


server <- function(input, output) {
  #1. reactive fn to set the height of the image
  imageHeight <- reactive({return(input$slider)})
  #2. reactive fn to assign the categories of the slider input
  sliderCat <- reactive({
    return(ifelse(imageHeight()<=200,1,ifelse(imageHeight()<=600,2,3)))
  })     
  #3. output$display text values reflect slider input categories
  output$display <- renderText({
    if  (input$select != 0) {
      if (sliderCat()==1) {
        paste0("value = ", input$select, ".jpg", " ", "small")}
      else if (sliderCat()==2 && input$select != 0) {
        paste0("value = ", input$select, ".jpg", " ", "medium")}
      else {paste0("value = ", input$select, ".jpg", " ", "large")}
    }
    else {paste0("value = ", input$select, ".jpg", " ", "none")}  
  })
  #4. if no image is showing, the reactive should return "none"
  selectedImage <- reactive({
    ifelse(input$select==0, "", paste0("image_", input$select, ".jpg"))
  })
  output$image <- renderUI({
    img(src=selectedImage(), height= imageHeight())
  })
}


shinyApp(ui, server)
