
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title="TREE 2 Codebook",titleWidth = 300),
    dashboardSidebar(tags$style(HTML(".main-sidebar{width: 200px;}")),
                     sidebarMenu(
                         menuItem("Overview", tabName = "home", icon = icon("house-user")),
                         menuItem("Exploration", tabName = "exp", icon = icon("atom"))
                     )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "home",
                    textOutput("intro_text"),
                    br(),
                    textOutput("intro_body"),
                    br(),
                    textOutput("body_intro"),
                    br(),
                    textOutput("list1"),
                    textOutput("list2"),
                    br(),
                    textOutput("body_tabs"),
                    br(),
                    textOutput("body_outro"),
                    br(),
                    textOutput("body_outro2"),
                    br(),                    
                    textOutput("outro")
            ),
            tabItem(tabName = "exp",
                    textOutput("exp_intro"),
                    br(),                    
                    dataTableOutput("items")
            )
        )
    )
)    

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$items <- renderDataTable(
        test    
    )
    
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
