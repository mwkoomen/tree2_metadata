
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(shinycssloaders)
library(dashboardthemes)

x <- read.csv2(
    "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/test.csv",
    sep=',',
    header = T,
    encoding = "UTF-8")
tabdata <- x %>%
    filter(item_text_e != "n/a" &
            grid_text_e != "n/a" &  
           item_text_e != "") %>%
    group_by(variable_name,
             item_id, 
             item_version,
             wave,
             item_text_e,
             item_text_d,
             item_text_f,
             item_text_i,
             grid_text_e,
             grid_text_d,
             grid_text_f,
             grid_text_i,
             data_collection,
             data_collection_a,
             mode,
             mode_a,
             module,
             subsample) %>%
    tally() %>%
    select(item_id,
           item_version,
           variable_name,
           wave,
           item_text_e,
           item_text_d,
           item_text_f,
           item_text_i,
           grid_text_e,
           grid_text_d,
           grid_text_f,
           grid_text_i,
           data_collection,
           data_collection_a,
           mode,
           mode_a,
           module,
           subsample)

ui <- dashboardPage(
    dashboardHeader(title="TREE2 Codebook"),
    dashboardSidebar(
                     sidebarMenu(
                         menuItem("Browse by Variable", tabName = "exp", icon = icon("th")),
                         menuItem("Browse by Theme", tabName = "theme", icon = icon("tree")),
                         menuItem("Download documentation", tabName = "download", icon = icon("arrow-alt-circle-down"))
                     ),
                     br(),
                     checkboxGroupInput("data", tags$span("Filter: data collection", style = "color: black;"),
                                        choiceNames = list(
                                            tags$span("Base", style = "color: black;"),
                                            tags$span("Complementary", style = "color: black;") 
                                        ),
                                        choiceValues = c(1,2),
                                        selected = c(1,2)
                     ),            
                     checkboxGroupInput("wave", tags$span("Filter: survey waves", style="color:black;"), 
                                        choiceNames = list(
                                            tags$span("0", style="color:black;"),
                                            tags$span("1", style="color:black;"),
                                            tags$span("2", style="color:black;")
                                        ),
                                        choiceValues = c(0,1,2),
                                        selected = c(0,1,2)
                     )
    ),
    dashboardBody(    
        # shinyDashboardThemes(
        # theme = "onenote"
        # ),
        tags$head(
            tags$style(HTML("
                    .skin-blue .main-header .logo {
                        color: black;
                        /*font-family: \"Georgia\", Times, \"Times New Roman\", serif;*/
                        font-weight: bold;
                        font-size: 20px;                        
                        background-color: #F0EFEF;
                    }
                    .skin-blue .main-header .logo:hover {
                        background-color: #F0EFEF;
                    }
                    .skin-blue .main-header .navbar {
                        background-color: #F0EFEF;
                    } 
                    .skin-blue .main-header .navbar .sidebar-toggle:hover{
                        background-color: #F0EFEF;
                    } 
                    .skin-blue .main-header .navbar .sidebar-toggle{
                        background-color: #F0EFEF;
                    } 
                    .skin-blue .main-sidebar {
                        background-color: #F0EFEF;
                    } 
                    .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                        background-color: #F0EFEF;
                        color: #000000;
                    }                    
                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                        color: black;
                        background-color: #CFCFCF;
                    } 
                    .content-wrapper, .right-side {
                        background-color: #FFFFFF;
                    }                    
                    #gridtext {
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 18px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;
                    }
                    #itemtext {
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 18px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;                      
                    } 
                    #meta {
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 20px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;                      
                    }
                    "))
        ),        
        tabItems(
            tabItem(tabName = "exp",
                sidebarLayout(
                    sidebarPanel(
                    htmlOutput("exp_intro"),
                    br(),
                    DTOutput("items"),
                ),
                mainPanel(
                    htmlOutput('meta'),
                    br(),
                    htmlOutput("gridtext"),
                    br(),
                    htmlOutput('itemtext'),
                    br(),
                    DTOutput("values")#,
                    # tags$style(".well {
                    #            background-color:#FFFFFFF;
                    #            border-radius: 25px;
                    #            border: 2px solid #AAAAAA;
                    #            padding: 20px;
                    #            }")
                    ),
                )
            ),
            tabItem(tabName = "download",
                    htmlOutput("dwn_text"),
                    br(),
                    actionButton(inputId='dwn', label="Download .pdf", 
                                 icon = icon("arrow-alt-circle-down"), 
                                 onclick ="location.href='https://github.com/mwkoomen/tree2_metadata/raw/main/data/Test.pdf';")
            )
        )
    )
)    

server <- function(input, output) {
    
    data <- reactive({
             req(input$wave)
             req(input$data)
             df_data <- tabdata %>% 
                 dplyr::filter(wave %in% input$wave & data_collection %in% input$data)
    })
    resp_values <- reactive({
        req(input$items_rows_selected)
        values <- x %>% 
            dplyr::filter(item_id==data()$item_id[input$items_rows_selected] & 
                              wave==data()$wave[input$items_rows_selected]) %>%
            select(response_value, value_text_e, value_text_d, value_text_f, value_text_i)
    })    
    output$items <- DT::renderDataTable(data()[c(3,4,14)],
                             options = list(
                                 lengthMenu = c(15, 25, 50), 
                                 pageLength = 15, 
                                 autoWidth=T, 
                                 dom='ft',
                                 initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                     "}")
                                 ),            
                             selection = list(mode = 'single'),
                             colnames = c('Variable', 'Wave', 'Data collection'), 
                             rownames=F
    )
    output$meta = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<b>Variable </b>",
                  as.character(data()$variable_name[input$items_rows_selected]), 
                  "</b>")
        }
        else{cat("<b>Variable</b>")} 
    })
    output$gridtext = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<b>Grid text</b>
                    <br>
                    <table style=\"width:90%\">
                      <tr>
                        <th>EN</th>
                        <th>DE</th>
                        <th>FR</th>
                        <th>IT</th>
                      </tr>
              <tr>
                <td>",as.character(data()$grid_text_e[input$items_rows_selected]),"</td>
                <td>",as.character(data()$grid_text_d[input$items_rows_selected]),"</td>
                <td>",as.character(data()$grid_text_f[input$items_rows_selected]),"</td>
                <td>",as.character(data()$grid_text_i[input$items_rows_selected]),"</td>
              </tr>
            </table>"
                )
        }
        else{cat("<b>Grid text</b>")} 
    })
    output$itemtext = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<b>Item text</b>
                    <br>
                    <table style=\"width:90%\">
                      <tr>
                        <th>EN</th>
                        <th>DE</th>
                        <th>FR</th>
                        <th>IT</th>
                      </tr>
              <tr>
                <td>",as.character(data()$item_text_e[input$items_rows_selected]),"</td>
                <td>",as.character(data()$item_text_d[input$items_rows_selected]),"</td>
                <td>",as.character(data()$item_text_f[input$items_rows_selected]),"</td>
                <td>",as.character(data()$item_text_i[input$items_rows_selected]),"</td>
              </tr>
            </table>"
            )            
        }
        else{cat("<b>Item text</b>")} 
    })
    output$response = renderPrint({
            cat("<b>Response values</b><br>")
    })
    output$values = DT::renderDataTable(resp_values(),
                                        options=list(
                                            dom='t',
                                            initComplete = JS(
                                                "function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                "}")
                                            ),
                                        colnames = c('Value','Label [EN]','Label [DE]','Label [FR]','Label [IT]'), 
                                        rownames=F)

    output$exp_intro = renderPrint({
            cat("<font size=4> <b>Select a variable</b></font>")
    })    
    output$dwn_text = renderPrint({
        cat("<font size=5> <b>Download full documentation</b></font>")
    })     
}

# Run the application 
shinyApp(ui = ui, server = server)
