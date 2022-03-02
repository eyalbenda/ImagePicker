#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(shinyjs)
source("fncs.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("TrIage of Non-organized DirEctoRies of microSCOPy imagEs (TinderScope) "),
    sidebarLayout(
      sidebarPanel(
    #Select a folder to work with
        shinyDirButton('folder', 'Select input folder', 'Please select a folder', FALSE),
        shinyDirButton('output', 'Select output folder', 'Please select a folder', FALSE),
        selectInput('ext',label = 'file extension',choices = c("tiff","tif","png","jpg"),multiple = T,selected = "tif"),
        tableOutput("work_dir_print"),
        sliderInput("vis_depth", "Select depth to batch images for selection", min = 1, max = 4, value = 1),
        actionButton('populate', 'Populate Images'),
        uiOutput("batch_choose"),
        div(id = "folder_copy",
            uiOutput('copy_folder'),
            uiOutput('next_folder')
        )
      ),
      mainPanel(plotOutput("images_view"))) 
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    volumes = getVolumes()()
    if(all(grepl('Volumes',volumes)))
      volumes = c('Root' = '/Volumes/')
    observe(shinyDirChoose(input, 'folder', roots=volumes, filetypes=input$ext))
    observe(shinyDirChoose(input, 'output', roots=volumes))
    path_object = reactiveValues()
    path_object$dirname = reactive(parseDirPath(volumes, input$folder))
    path_object$output_folder = reactive(parseDirPath(volumes, input$output))
    path_object$df_file = reactive(image_parse(path_object$dirname(),input$ext))
    path_object$parsed_batch = reactive(image_batch(path_object$df_file(),input$vis_depth)) 
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$folder
                 },
                 handlerExpr = {
                   req(is.list(input$folder))
                shinyjs::show("populate")
                })
    observeEvent(input$populate,{
      output$batch_choose = renderUI({
        numericInput(inputId = 'batch',
                     label = 'Choose image batch',
                     value = 1,
                     min = 1,
                     max=length(path_object$parsed_batch()))
      })  
      output$copy_folder = renderUI(actionButton('copy_folder',"Copy to Output Folder"))
      output$next_folder = renderUI(actionButton('next_folder',"Next"))
      shinyjs::hide("populate")
    })
    observeEvent(input$vis_depth,
    {
      updateNumericInput(inputId = "batch",value = -1)
      updateNumericInput(inputId = "batch",value = 1)
    })
    observeEvent(input$next_folder,
                   if(isolate(input$batch) < length(isolate(path_object$parsed_batch())))
                   {
                     updateNumericInput(inputId = "batch",value = isolate(input$batch) + 1)
                   } 
                 )
    
    observeEvent(input$batch,{
     imgs_full_path = file.path(path_object$dirname(),path_object$parsed_batch()[[input$batch]])
     output$images_view = renderPlot({
       pl = img_plot(imgs_full_path,names(path_object$parsed_batch()[input$batch]))
       summary = "Images Plots"
       list(pl,summary)
       })
    })
    
    observeEvent(input$copy_folder,{
    cur_batch = isolate(input$batch)
    imgs_full_path = file.path(isolate(path_object$dirname()),isolate(path_object$parsed_batch())[[cur_batch]])
    target_paths = file.path(isolate(path_object$output_folder()),isolate(path_object$parsed_batch())[[cur_batch]])
    for(f in 1:length(imgs_full_path))
    {
      if(!dir.exists(dirname(target_paths[f])))
        dir.create(dirname(target_paths[f]),recursive = T)
    }
    file.copy(imgs_full_path,target_paths,overwrite = F)
    if(cur_batch < length(isolate(path_object$parsed_batch())))
    {
      updateNumericInput(inputId = "batch",value = cur_batch + 1)
    } 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
