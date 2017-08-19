
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(lubridate)

shinyUI(
    navbarPage(
        
        # Navbar title
        title = "UITF Charts",
        
        # Navbar color
        inverse = TRUE,
        
        # Analytics Tab
        tabPanel(
            title = "Analytics", 
            icon = icon("line-chart"),
            sidebarLayout(
                # Sidebar: Selection and Basic Info
                sidebarPanel(
                    width = 4,
                    # Fund Name
                    h3(textOutput("analytics_fund_name")),
                    h4(textOutput("analytics_bank_name")),
                    br(),
                    tabsetPanel(
                        type = "tabs",
                        # Controls Tab
                        tabPanel(
                            title = "Controls",
                            br(),
                            # Fund Selection
                            selectizeInput(
                                inputId = "analytics_select_fund",
                                label = "Select UITF",
                                choices = list("None")
                            ),
                            # Comparison Selection
                            selectizeInput(
                                inputId = "analytics_comparison",
                                label = "Add Comparison",
                                choices = list("None"),
                                multiple = TRUE,
                                width = "100%"
                            )
                        ),
                        # Fund Profile Tab
                        tabPanel(
                            title = "Profile",
                            DT::dataTableOutput("analytics_fund_profile")
                        )
                    )
                ),
                # Main Panel: Controls and Charts
                mainPanel(
                    width = 8,
                    fluidRow(
                        # Date Range Selection
                        # Date Range Action Buttons
                        column(
                            width = 3,
                            radioGroupButtons(
                                inputId = "analytics_quickDateRange",
                                label = "Quick Date Range",
                                choices = list("1M","1Y","5Y","All"),
                                selected = NULL,
                                size = "sm",
                                justified = TRUE
                            )
                        ),
                        column(
                            width = 5,
                            dateRangeInput(
                                inputId = "analytics_dateRange",
                                label = "Date Range",
                                width = "100%",
                                start = Sys.Date()-years(1),
                                end = Sys.Date()
                            )
                        ),
                        # Control for Adding Indicator Charts
                        column(
                            width = 4,
                            selectizeInput(
                                inputId = "analytics_indicators",
                                label = "Indicators",
                                choices = list("RSI","MACD"),
                                multiple = TRUE,
                                width = "100%"
                            )
                        )
                    ),
                    # Composite Charts
                    fluidRow(
                        column(
                            width = 12,
                            plotlyOutput("analytics_composite_chart",
                                         height = "900px")
                        )
                    )
                )
            )
        ),
        
        
        # Fund Matrix Tab
        tabPanel(
            title = "Fund Matrix", 
            icon = icon("table"),
            DT::dataTableOutput("fund_matrix")
        )#,
        
        
        # Best Funds Tab
#        tabPanel(
#            title = "Best Funds",
#            icon = icon("money")
#        ),
        
        
#        # Calculator Tab
#        tabPanel(
#            title = "Calculator",
#            icon = icon("calculator")
#        )
        
    )
)
