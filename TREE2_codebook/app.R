
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(shinycssloaders)
library(shinyTree)
#library(dashboardthemes)
#library(shinyWidgets)

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
             theme1,
             theme2,
             theme3,
             concept_text,
             concept_text_long,
             suf_name,
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
             subsample,
             variable_type,
             format) %>%
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
           subsample,
           variable_type,
           theme1,
           theme2,
           theme3,
           concept_text,
           concept_text_long,
           suf_name,
           format
    )
gtheme <- tabdata %>% group_by(theme1) %>% tally() %>% select(theme1)
t1 <- x2 %>% dplyr::group_by(theme_l1)%>%tally()%>%select(theme_l1)
theme1 <- as.list(t1$theme_l1)
theme_list <- list()
for (l in theme1){
    z <- x2 %>% dplyr::filter(theme_l1 == l) %>%
        group_by(theme_l2) %>% tally() %>% select(theme_l2)
    d <- as.list(z$theme_l2)
    u <- list()
    for (r in d){
        m <- x2 %>% dplyr::filter(theme_l2 == r) %>%
            group_by(theme_l3) %>% tally() %>% select(theme_l3) 
        h <- as.list(m$theme_l3)
        i <- list()
        for (n in h){
            v <- x2 %>% dplyr::filter(theme_l3 == n) %>%
                group_by(item_id) %>% tally() %>% select(item_id)
            t <- as.list(v$item_id)
            i[[n]] <- t
        }
        u[[r]] <- i 
    }
    theme_list[[l]] <- u 
}
rm(u,v,w,z,e,l,n,r,k,m,p,t,d,h,i,j)

themes <- tabdata %>% group_by(theme1, theme2, theme3, concept_text_long) %>% 
    tally() %>% 
    select(theme1, theme2, theme3, concept_text_long)
ui <- dashboardPage(
    dashboardHeader(title="TREE2 Codebook"),
    dashboardSidebar(
                     sidebarMenu(
                         menuItem("Overview", tabName = "home", icon = icon("home")),                         
                         menuItem("Browse ALL Variables", tabName = "exp", icon = icon("th")),
                         menuItem("Browse themes (Tree)", tabName = "themetree", icon=icon("tree")),
                         menuItem("Browse themes (Box)", tabName = "theme", icon = icon("tree")),
                         menuItem("Browse by Data / SUF file", tabName = 'suf', icon=icon('clone')),
                         menuItem("Download full documentation", tabName = "download", icon = icon("arrow-alt-circle-down"))
                     )
                     # materialSwitch(inputId = "w0", label = tags$span("Wave 0", style = "color: black;"), status = "success", value=T),
                     # materialSwitch(inputId = "w1", label = tags$span("Wave 1", style = "color: black;"), status = "success", value=T),
                     # materialSwitch(inputId = "w2", label = tags$span("Wave 2", style = "color: black;"), status = "success", value=T)
    ),
    dashboardBody(    
        # shinyDashboardThemes(
        # theme = "onenote"
        # ),
        tags$head(
            tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#7F7F7F
                    }
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
                    #gridtext, #suf_gridtext {
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 18px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;
                    }
                    #itemtext,#suf_itemtext {
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 18px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;                      
                    } 
                    #meta, #suf_meta{
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 20px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;                      
                    }
                    #meta1, #suf_meta1 {
                      color: black;
                      background: #F0EFEF;
                      font-family:calibri;
                      font-size: 18px;
                      font-style: none;
                      border-radius: 25px; 
                      padding: 20px;                      
                    } 
                    #response, #suf_response {
                      color: black;
                      background: #FFFFFF;
                      font-family:calibri;
                      font-size: 18px;
                      font-style: none;
                    }
                    #items,#theme_items,#suf_items,#theme1,#theme2,#theme3,#concept,#meta2{ cursor: pointer; }
                    .shiny-output-error { visibility: hidden; }
                    .shiny-output-error:before { visibility: hidden; }
                    "))
        ),        
        tabItems(
            tabItem(tabName = "home",
                    # mainPanel(
                    #     img(src='data/LogoTREE_grün mit Schrift_links.jpg', align = "right")
                    # )                    
            ),
            tabItem(tabName = "exp",
                sidebarLayout(
                    sidebarPanel(
                    htmlOutput('intro1'),
                    br(),
                    checkboxGroupInput("format", tags$span("Data format", style = "color: black;"),
                                       choiceNames = list(
                                           tags$span("cross-sectional", style = "color: black;"),
                                           tags$span("longitudinal", style = "color: black;") 
                                       ),
                                       choiceValues = c(1,2),
                                       selected = c(1,2),
                                       inline=F
                    ),                     
                    checkboxGroupInput("wave", tags$span("Include survey waves:", style="color:black;"), 
                                       choiceNames = list(
                                           tags$span("0", style="color:black;"),
                                           tags$span("1", style="color:black;"),
                                           tags$span("2", style="color:black;")
                                       ),
                                       choiceValues = c(0,1,2),
                                       selected = c(0,1,2),
                                       inline = F
                    ),
                    br(),
                    htmlOutput("exp_intro"),
                    br(),
                    DTOutput("items")
                ),
                mainPanel(
                    htmlOutput('meta'),
                    br(),
                    htmlOutput("meta1"),
                    br(),
                    htmlOutput("gridtext"),
                    br(),
                    htmlOutput('itemtext'),
                    br(),
                    htmlOutput('response'),
                    br(),
                    DTOutput("values"),
                     tags$style(".well {
                                background-color:#FFFFFFF;
                                border-radius: 25px;
                                border: 2px solid #AAAAAA;
                                padding: 20px;
                                }")
                    )
                )
            ),
            tabItem(tabName = "themetree",
                    shinyTree("tree"), 
                    verbatimTextOutput("treeprint")
            ),
            tabItem(tabName = 'theme',
                    box(title = "1: Select global-themes", status = "primary",height = "400" ,
                        solidHeader = T, width="3",
                        DTOutput("theme1")),
                    box( title = "2: Select meso-themes", status = "primary", height = 
                             "400",width = "3",solidHeader = T, 
                         DTOutput("theme2")),
                    box( title = "3. Select sub-themes", status = "primary", height = 
                             "400",width = "3",solidHeader = T, 
                         DTOutput("theme3")),
                    box( title = "4: Select variable concepts", status = "primary", height = 
                              "400",width = "3",solidHeader = T, 
                          DTOutput("concept")),
                    box( title = "", status = "primary", height = 
                             "400",width = "12",solidHeader = F,
                         DTOutput('meta2')
                         )
            ),            
            tabItem(tabName = 'suf',
                    sidebarLayout(
                        sidebarPanel(
                                selectInput(
                                    "sufs",tags$span("Select a Sientific Use-File:", 
                                                     style = "color: black;
                                                     font-size: 16px;"),x$suf_name,multiple = F,selected="T0"
                            ),
                            br(),
                        htmlOutput("suf_intro"),
                        br(),
                        DTOutput("suf_items")
                    ),
                    mainPanel(
                        htmlOutput('suf_meta'),
                        br(),
                        htmlOutput("suf_meta1"),
                        br(),
                        htmlOutput("suf_gridtext"),
                        br(),
                        htmlOutput('suf_itemtext'),
                        br(),
                        htmlOutput('suf_response'),
                        br(),
                        DTOutput("suf_values")
                    )
                )
            ),
            tabItem(tabName = "download",
                    box(title = "Download full study codebook", status = "primary",height = "300" ,
                        solidHeader = T, width="6",
                             actionButton(inputId='dwn', label="Download .pdf", 
                               icon = icon("arrow-alt-circle-down"), 
                               onclick ="location.href='https://github.com/mwkoomen/tree2_metadata/raw/main/data/Test.pdf';")
                        ),
                    box(title = "Download scaling documentation", status = "primary",height = "300" ,
                        solidHeader = T, width="6",
                        actionButton(inputId='dwn2', label="Download .pdf", 
                                     icon = icon("arrow-alt-circle-down"), 
                                     onclick ="location.href='https://github.com/mwkoomen/tree2_metadata/raw/main/data/Test.pdf';")
                        ),                    
                    box(title = "Download weighting documentation", status = "primary",height = "300" ,
                        solidHeader = T, width="6",
                        actionButton(inputId='dwn3', label="Download .pdf", 
                                     icon = icon("arrow-alt-circle-down"), 
                                     onclick ="location.href='https://github.com/mwkoomen/tree2_metadata/raw/main/data/Test.pdf';")
                        )                     

            )
        )
    )
)
server <- function(input,output,session) {
    output$tree <- renderTree({
        theme_list
    })
    output$treeprint <- renderPrint({
        print(get_selected(input$tree))
    })   
    conceptd <- reactive({
        req(input$theme3_rows_selected)
        theme3d <- themes %>%
            filter(theme3==theme3d()$theme3[input$theme3_rows_selected]) %>%
            group_by(concept_text_long)%>%
            tally()%>%
            select(concept_text_long)
    })
    theme3d <- reactive({
        req(input$theme2_rows_selected)
        theme3d <- themes %>% 
            filter(theme2==theme2d()$theme2[input$theme2_rows_selected]) %>% 
            group_by(theme3)%>%
            tally()%>%
            select(theme3)
    })     
    theme2d <- reactive({
        req(input$theme1_rows_selected)
        theme2d <- themes %>% 
            filter(theme1==gtheme$theme1[input$theme1_rows_selected]) %>% 
                              group_by(theme2)%>%
                              tally()%>%
                              select(theme2)
    })  
    data2 <- reactive({
             req(input$sufs)
             suf_data <- tabdata %>% 
                 dplyr::filter(suf_name %in% input$sufs)
    })
    data <- reactive({
        req(input$wave)
        req(input$format)
        df_data <- tabdata %>% 
            dplyr::filter(wave %in% input$wave & format %in% input$format)
    }) 
    theme_data <- reactive({
        req(input$theme1_rows_selected)
        if (length(input$theme2_rows_selected)>0){
            if (length(input$theme3_rows_selected)>0){
                if (length(input$concept_rows_selected)>0){
                    theme_data <- tabdata %>% 
                        dplyr::filter(theme1 %in% gtheme$theme1[input$theme1_rows_selected] &
                                      theme2 %in% theme2d()$theme2[input$theme2_rows_selected] &
                                      theme3 %in% theme3d()$theme3[input$theme3_rows_selected] &
                                      concept_text_long %in% conceptd()$concept_text_long[input$concept_rows_selected])                    
                }
                else {
                    theme_data <- tabdata %>% 
                        dplyr::filter(theme1 %in% gtheme$theme1[input$theme1_rows_selected] &
                                      theme2 %in% theme2d()$theme2[input$theme2_rows_selected] &
                                      theme3 %in% theme3d()$theme3[input$theme3_rows_selected])
                }
            }
            else {
                theme_data <- tabdata %>% 
                    dplyr::filter(theme1 %in% gtheme$theme1[input$theme1_rows_selected] &
                                      theme2 %in% theme2d()$theme2[input$theme2_rows_selected])
            }
        }
        else {
            theme_data <- tabdata %>% 
            dplyr::filter(theme1 %in% gtheme$theme1[input$theme1_rows_selected])
        }
    })     
    measurew <- reactive({
        req(input$items_rows_selected)
        mw <- x %>% 
            filter(item_id==data()$item_id[input$items_rows_selected]) %>% 
            group_by(wave) %>% 
            tally() %>% 
            select(wave) %>% 
            pull(wave)
    })
    suf_measurew <- reactive({
        req(input$suf_items_rows_selected)
        mw <- x %>% 
            filter(item_id==data2()$item_id[input$suf_items_rows_selected]) %>% 
            group_by(wave) %>% 
            tally() %>% 
            select(wave) %>% 
            pull(wave)
    }) 
    suf_resp_values <- reactive({
        req(input$suf_items_rows_selected)
        values <- x %>% 
            dplyr::filter(item_id==data2()$item_id[input$suf_items_rows_selected] & 
                              wave==data2()$wave[input$suf_items_rows_selected]) %>%
            select(response_value, value_text_e, value_text_d, value_text_f, value_text_i)  
    })
    resp_values <- reactive({
        req(input$items_rows_selected)
        values <- x %>% 
            dplyr::filter(item_id==data()$item_id[input$items_rows_selected] & 
                              wave==data()$wave[input$items_rows_selected]) %>%
            select(response_value, value_text_e, value_text_d, value_text_f, value_text_i)
    }) 
    output$meta2 <- DT::renderDataTable(theme_data()[c(3,9,5)],
                                        options = list(
                                            #lengthMenu = c(15, 25, 50), 
                                            pageLength = 8, 
                                            autoWidth=F, 
                                            dom='ft',
                                            scrollY = '400px', 
                                            paging = FALSE, 
                                            scrollX = TRUE,
                                            initComplete = JS(
                                                "function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                "}")
                                        ),            
                                        selection = list(mode = 'single'),
                                        colnames = c('Variable', 'Grid text [EN]', 'Item text [EN]'), 
                                        rownames=F
    )    
    output$items <- DT::renderDataTable(data()[c(3,4,14)],
                             options = list(
                                 #lengthMenu = c(15, 25, 50), 
                                 pageLength = 8, 
                                 autoWidth=F, 
                                 dom='ft',
                                 scrollY = '400px', 
                                 paging = FALSE, 
                                 scrollX = TRUE,
                                 initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                     "}")
                                 ),            
                             selection = list(mode = 'single'),
                             colnames = c('Variable', 'Wave', 'Data collection'), 
                             rownames=F
    )
    output$suf_items <- DT::renderDataTable(data2()[c(3,4,14)],
                                        options = list(
                                            #lengthMenu = c(15, 25, 50), 
                                            pageLength = 15, 
                                            autoWidth=F, 
                                            dom='ft',
                                            scrollY = '400px', 
                                            paging = FALSE, 
                                            scrollX = TRUE,                                            
                                            initComplete = JS(
                                                "function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                "}")
                                        ),            
                                        selection = list(mode = 'single'),
                                        colnames = c('Variable', 'Wave', 'Data collection'), 
                                        rownames=F
    )
    output$theme1 <- DT::renderDataTable(gtheme,
                                            options = list(
                                                #lengthMenu = c(15, 25, 50), 
                                                pageLength = 15, 
                                                autoWidth=F, 
                                                dom='ft',
                                                scrollY = '400px', 
                                                paging = FALSE, 
                                                scrollX = TRUE,                                                
                                                initComplete = JS(
                                                    "function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                    "}")
                                            ),            
                                            selection = list(mode = 'single'),
                                            colnames = c('Global themes'), 
                                            rownames=F
    )
    output$theme2 <- DT::renderDataTable(theme2d(),
                                         options = list(
                                             #lengthMenu = c(15, 25, 50), 
                                             pageLength = 15, 
                                             autoWidth=F, 
                                             dom='ft',
                                             scrollY = '400px', 
                                             paging = FALSE, 
                                             scrollX = TRUE,                                                
                                             initComplete = JS(
                                                 "function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                 "}")
                                         ),            
                                         selection = list(mode = 'single'),
                                         colnames = c('Meso-themes'), 
                                         rownames=F
    )    
    output$theme3 <- DT::renderDataTable(theme3d(),
                                         options = list(
                                             #lengthMenu = c(15, 25, 50), 
                                             pageLength = 15, 
                                             autoWidth=F, 
                                             dom='ft',
                                             scrollY = '400px', 
                                             paging = FALSE, 
                                             scrollX = TRUE,                                                
                                             initComplete = JS(
                                                 "function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                 "}")
                                         ),            
                                         selection = list(mode = 'single'),
                                         colnames = c('Sub-themes'), 
                                         rownames=F
    )
    output$concept <- DT::renderDataTable(conceptd(),
                                         options = list(
                                             #lengthMenu = c(15, 25, 50), 
                                             pageLength = 15, 
                                             autoWidth=F, 
                                             dom='ft',
                                             scrollY = '400px', 
                                             paging = FALSE, 
                                             scrollX = TRUE,                                                
                                             initComplete = JS(
                                                 "function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                 "}")
                                         ),            
                                         selection = list(mode = 'single'),
                                         colnames = c('Variable concepts'), 
                                         rownames=F
    )    
    output$suf_meta = renderPrint({
        if(length(input$suf_items_rows_selected) > 0){
            cat("<b>Variable</b>",
                as.character(data2()$variable_name[input$suf_items_rows_selected]))
        }
        else{cat("<b>Variable</b>")} 
    })
    output$suf_meta1 = renderPrint({
        if(length(input$suf_items_rows_selected) > 0){
            cat("<table style=\"width:90%\">
              <tr>
                <th><b>Measured in waves</b></th>
                <th><b>Variable type</b></th>
                <th><b>Survey mode</b></th>
                <th><b>Subsample</b></th>
              </tr>
              <tr>
                <td>",suf_measurew(),"</td>
                <td>",as.character(data2()$variable_type[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$mode_a[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$subsample[input$suf_items_rows_selected]),"</td>
              </tr>
            </table>"
            )
        }
        else{cat("<b>Meta information</b>")
        } 
    })    
    output$suf_gridtext = renderPrint({
        if(length(input$suf_items_rows_selected) > 0){
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
                <td>",as.character(data2()$grid_text_e[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$grid_text_d[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$grid_text_f[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$grid_text_i[input$suf_items_rows_selected]),"</td>
              </tr>
            </table>"
            )
        }
        else{cat("<b>Grid</b>")} 
    })
    output$suf_itemtext = renderPrint({
        if(length(input$suf_items_rows_selected) > 0){
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
                <td>",as.character(data2()$item_text_e[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$item_text_d[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$item_text_f[input$suf_items_rows_selected]),"</td>
                <td>",as.character(data2()$item_text_i[input$suf_items_rows_selected]),"</td>
              </tr>
            </table>"
            )            
        }
        else{cat("<b>Item</b>")} 
    })
    output$suf_response = renderPrint({
        if(length(input$suf_items_rows_selected) > 0){
            cat("<b>Response values</b>")
        }
        else{cat("")} 
    })    
    output$suf_values = DT::renderDataTable(suf_resp_values(),
                                        options=list(
                                            dom='t',
                                            initComplete = JS(
                                                "function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': '#7F7F7F', 'color': '#fff'});",
                                                "}")
                                        ),
                                        colnames = c('Value','Label [EN]','Label [DE]','Label [FR]','Label [IT]'), 
                                        rownames=F)
    
    output$meta = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<b>Variable</b>",
                  as.character(data()$variable_name[input$items_rows_selected]))
        }
        else{cat("<b>Variable</b>")} 
    })
    output$meta1 = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<table style=\"width:90%\">
              <tr>
                <th><b>Measured in waves</b></th>
                <th><b>Variable type</b></th>
                <th><b>Survey mode</b></th>
                <th><b>Subsample</b></th>
              </tr>
              <tr>
                <td>",measurew(),"</td>
                <td>",as.character(data()$variable_type[input$items_rows_selected]),"</td>
                <td>",as.character(data()$mode_a[input$items_rows_selected]),"</td>
                <td>",as.character(data()$subsample[input$items_rows_selected]),"</td>
              </tr>
            </table>"
            )
        }
        else{cat("<b>Meta information</b>")
            } 
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
        else{cat("<b>Grid</b>")} 
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
        else{cat("<b>Item</b>")} 
    })
    output$response = renderPrint({
        if(length(input$items_rows_selected) > 0){
            cat("<b>Response values</b>")
        }
        else{cat("")} 
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
            cat("<font size=3> <b>Select a Variable</b></font>")
    }) 
    output$suf_intro = renderPrint({
        if(input$sufs == ''){
            cat("")
        }    
        else {
        cat("<font size=3> <b>Select a Variable</b></font>")
        }
    })  
    output$theme_intro = renderPrint({
        if(input$theme1 == ''){
        cat("")
        }
        else{cat("<font size=3> <b>Select a Variable</b></font>")} 
    })     
    output$intro1 = renderPrint({
         cat("<font size=3><b>Quick select filters:</b></font>")
    })     
}

# Run the application 
shinyApp(ui = ui, server = server)
