
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(shinycssloaders)

# x <- read.csv2(
#     "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/tree2_metadata_202012091031.csv",
#     sep=',',
#     header = T,
#     encoding = "UTF-8")
tabdata <- x %>%
    filter(item_text_e != "n/a" &
            grid_text_e != "n/a" &  
           item_text_e != "") %>%
    group_by(item_name,
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
           item_name,
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
    dashboardHeader(title="TREE 2 Codebook"),
    dashboardSidebar(
                     sidebarMenu(
                         menuItem("Browse by Variable", tabName = "exp", icon = icon("tree")),
                         menuItem("Browse by Theme", tabName = "theme", icon = icon("tree"))
                     ),
                     br(),
                     textOutput("filter"),
                     checkboxGroupInput("data", "Data collection", 
                                        choices = list(
                                            "Base" = 1,
                                            "Complementary" = 2
                                            ),
                                        selected = c(1,2)
                     ),            
                     checkboxGroupInput("wave", "Include waves", 
                                        choices = list(
                                            "0" = 0,
                                            "1" = 1, 
                                            "2" = 2
                                        ),
                                        selected = c(0,1,2)
                     )
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML("
                    #gridtext {
                      color: black;
                      background: #FFFFFF;
                      font-family:calibri;
                      font-size: 22px;
                      font-style: none;
                      border-radius: 25px; 
                      border: 2px solid #73AD21;
                      padding: 20px;
                    }
                    #itemtext {
                      color: black;
                      background: #FFFFFF;
                      font-family:calibri;
                      font-size: 22px;
                      font-style: none;
                      border-radius: 25px; 
                      border: 2px solid #73AD21;
                      padding: 20px;                      
                    } 
                    #meta {
                      color: black;
                      background: #FFFFFF;
                      font-family:calibri;
                      font-size: 22px;
                      font-style: none;
                      border-radius: 25px; 
                      border: 2px solid #73AD21;
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
                    DTOutput("items"),width = 4),
                mainPanel(
                    htmlOutput('meta'),
                    br(),
                    htmlOutput("gridtext"),
                    br(),
                    htmlOutput('itemtext'),
                    br(),
                    htmlOutput("response"),
                    br(),
                    DTOutput("values"),
                    tags$style(".well {
                               background-color:#FFFFFFF;
                               border-radius: 25px; 
                               border: 2px solid #73AD21;
                               padding: 20px;
                               }")
                    )
                )
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
                             options = list(lengthMenu = c(15, 25, 50), pageLength = 15, autoWidth=T),            
                             selection = list(mode = 'single'),
                             filter="top", rownames=F
    )
    output$meta = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<font size=5> Variable: <b>",
                  as.character(data()$item_name[input$items_rows_selected]), 
                  "</b></font>")
        }
        else{cat("<font size=5><b>Variable:</b> ...please select a variable</font>")} 
    })
    output$gridtext = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<font size=4> <b>Grid text:</b><br>[EN]:",
                as.character(data()$grid_text_e[input$items_rows_selected]),
                "<br>[DE]:",
                as.character(data()$grid_text_d[input$items_rows_selected]),
                "<br>[FR]:",
                as.character(data()$grid_text_f[input$items_rows_selected]),
                "<br>[IT]:",
                as.character(data()$grid_text_i[input$items_rows_selected]),                
                "</font>")
        }
        else{cat("<font size=4><b>Grid text:</b> ...")} 
    })
    output$itemtext = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<font size=4> <b>Item text:</b><br>[EN]:",
                as.character(data()$item_text_e[input$items_rows_selected]),
                "<br>[DE]:",
                as.character(data()$item_text_d[input$items_rows_selected]),
                "<br>[FR]:",
                as.character(data()$item_text_f[input$items_rows_selected]),
                "<br>[IT]:",
                as.character(data()$item_text_i[input$items_rows_selected]),                
                "</font>")
        }
        else{cat("<font size=4><b>Item text:</b> ...")} 
    })
    output$response = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<font size=4> <b>Response values:</b><br></font>")
        }
        else{cat("")} 
    })
    output$values = DT::renderDataTable({
        if(length(input$items_rows_selected) > 0){
            resp_values()
        }
        else{cat("")} 
    })
    output$exp_intro = renderPrint({
            cat("<font size=5> <b>Select a variable:</b></font>")
    })    
    output$filter <- renderText({ 
        " Quick select filters" 
    })    
    output$intro_text <- renderText({ 
        "Hello friend!" 
    })
    output$intro_body <- renderText({ 
        "This application lets you explore some key variables from the European Value Survey and run a simple regression." 
    })
    output$body_intro <- renderText({ 
        "Two main variables are covered by this application:" 
    }) 
    output$list1 <- renderText({ 
        "1: [V72] To what extent do you agree with the following statement: 'When a mother works for pay, the children suffer'"
    })
    output$list2 <- renderText({ 
        "2: [V80] To what extent do you agree with the following statement: 'When jobs are scarce, employers should give priority to citizens over immigrants'"
    })
    output$body_tabs <- renderText({ 
        "You can view barplots for those variables plus some relevant demographic variables 
        in the exploration tabs on dashboard on the left." 
    })
    output$body_outro <- renderText({ 
        "Select your country of choice on the top corner of the dashboard to automatically calculate 
        that country's descriptive statistics!" 
    })
    output$body_outro2 <- renderText({ 
        "To run a simple linear regression, select an outcome and which control 
        variales or age polynomials you would like to inlcude in the model and press the Regression tab on the left."
    })       
    output$outro <- renderText({ 
        "Enjoy!"  
    })       
}

# Run the application 
shinyApp(ui = ui, server = server)
