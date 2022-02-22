# install.packages('shiny')

library(shiny)

ui <- fluidPage(
    h1('Hello, Shiny!'),
    img(src = 'https://w.namu.la/s/3ca92265fffe4dd38a729df3df7dd41ea3ef12b72383958c8fd0bea79ffaa0331810fb8bea8ba09156d69e5f792eef522dd8eb932389ed972ff233c2c9adb34f5934a9d70d6f71db6ec9f11c80efcfca7d16a235f4f13a8b176f094e906b742c290cdcd1f2e71f6c05761d4f83aa461f'))

server <- function(input, output, session) {
    # Do something here!
}

shinyApp(ui = ui, server = server)









































