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
  tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),
  tags$head(tags$style("#test .modal-dialog {height: fit-content !important;}")),
  
  # Application title
  titlePanel("Image picker (TinderSCOPE)"),
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
          sliderInput("size", "size", min = 500, max = 2000, value = 500),
          uiOutput('copy_folder'),
          uiOutput('next_folder')
      ),
      div(id = "crop_div",
          uiOutput('imgs_selector'),
          uiOutput('crop_dialog'))
    ),
    mainPanel(plotOutput("images_view"))) 
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cropValModal = modalDialog(
    title = "Crop image batch",
    plotOutput("cropped_pl",inline = T),
    footer = tagList(actionButton("save_crops",label = "Save"),actionButton("cancel_crop","Cancel")),
    easyClose = F
  )
  cropSetModal = modalDialog(
    title = "Crop image batch",
    plotOutput("pl",brush = "crop_brush"),
    footer = tagList(actionButton("crop_execute","Crop"),actionButton("close_modal",label = "Cancel")),
    easyClose = F
  )
  
  shinyjs::hide("crop_div")
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
    },
    width = function() input$size,
    height = function() input$size
    )
    base_file_names = reactive(basename(imgs_full_path))
    output$imgs_selector = renderUI(selectInput('imgs_selector',label = 'crop image selector',choices = isolate(base_file_names()),multiple = F))
    output$crop_dialog = renderUI(actionButton('crop_dialog',label = 'open crop dialog'))
    shinyjs::show("crop_div")
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
  observeEvent(input$plot_brush,{
    print(input$plot_brush)
  })
  
  observeEvent(input$crop_dialog,{
    cur_images = file.path(isolate(path_object$dirname()),isolate(path_object$parsed_batch())[[isolate(input$batch)]])
    selected_image = isolate(input$imgs_selector)
    img_ind = which(basename(cur_images)==selected_image)
    print(cur_images)
    print(selected_image)
    print(img_ind)
    cur_image = cur_images[img_ind]
    print(cur_image)
    output$pl = renderPlot(img_plot(cur_image,selected_image))
    showModal(cropSetModal)
  })
  
  observeEvent(input$close_modal,{
    removeModal()
  })
  
  crop_paths = reactiveValues()
  observeEvent(input$crop_execute,{
    if(is.null(isolate(input$crop_brush)))
    {
      print("No area selected")
    } else
    {
      coords=isolate(input$crop_brush)
      relative_images_path = isolate(path_object$parsed_batch())[[isolate(input$batch)]]
      full_images_path = file.path(isolate(path_object$dirname()),isolate(path_object$parsed_batch())[[isolate(input$batch)]])
      output_paths = imgs_crop(full_images_path,relative_images_path,coords)
      output$cropped_pl = renderPlot({img_plot(output_paths,names(path_object$parsed_batch()[input$batch]))},
        width = function() input$size,
        height = function() input$size
        )
      destination_paths = file.path(isolate(path_object$output_folder()),dirname(relative_images_path),"cropped")
      k = 1
      while(any(sapply(destination_paths,dir.exists)))
        
      {
        destination_paths = gsub("cropped",paste0("cropped_",k),destination_paths)
        k = k +1
      }
      destination_paths = file.path(destination_paths,basename(full_images_path))
      crop_paths$from =output_paths
      crop_paths$to=destination_paths
      showModal(cropValModal)
    }
    observeEvent(input$cancel_crop,ignoreInit = TRUE,{
      removeModal()
    })
    observeEvent(input$save_crops,ignoreInit = TRUE,{
      for(f in 1:length(crop_paths$to))
      {
        if(!dir.exists(dirname(crop_paths$to[f])))
          dir.create(dirname(crop_paths$to[f]),recursive = T)
      }
      file.copy(crop_paths$from,crop_paths$to,overwrite = F)
      removeModal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
