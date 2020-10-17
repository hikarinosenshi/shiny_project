######################
# setup
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(scales)
packageVersion("scales")

listings_2020 <- read_csv('listings_2020.csv')
cases_by_date <- read_csv('CasesByDate.csv')
reviews_2020 <- read_csv('reviews_2020.csv')
reviews2_2020 <- read_csv('reviews2_2020.csv')
newreviews_2020 <- read_csv('newreviews_2020.csv')

cases_by_date$month <- factor(x = cases_by_date$month, levels = cases_by_date$month)


#######################
# ui
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = ""
                    ),
                    dashboardSidebar(
                        sidebarMenu(
                            br(),br(),
                            menuItem("Introduction", tabName = "introduction", icon = icon("angle-right"))
                        ),
                        sidebarMenu(
                            br(),
                            menuItem(text = HTML("Massachusetts<br/>&nbsp;&nbsp;&nbsp;&nbsp;COVID-19 cases"), tabName = "boston_cases", icon = icon("angle-double-right"))
                        ),
                        sidebarMenu(
                            br(),
                            menuItem(text = HTML("Airbnb Listings<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Boston area"), tabName = "boston_listings", icon = icon("angle-double-right"))
                        ),
                        sidebarMenu(
                            br(),
                            menuItem(text = HTML("Listings Reviews<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Boston area"), tabName = "boston_reviews", icon = icon("angle-double-right"))
                        ),
                        sidebarMenu(
                            br(),
                            menuItem(text = HTML("Reviews of Listings<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Present Whole Period<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Boston area"), tabName = "boston_reviews2", icon = icon("angle-double-right"))
                        ),
                        sidebarMenu(
                            br(),
                            menuItem(text = HTML("New Reviews<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Present Whole Period<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Boston area"), tabName = "boston_newreviews", icon = icon("angle-double-right"))
                        ),
                        sidebarMenu(
                            br(),
                            menuItem(text = 'Conclusions', tabName = "conclusions", icon = icon("angle-double-right"))
                        ),
                        sidebarMenu(
                            br(),br(),
                            menuItem(text = HTML('&nbsp;<i>About<i/>'), tabName = "about", icon = icon("info"))
                        )
                    ),
                    dashboardBody(
                        tags$head(tags$style(HTML('
                          .main-header .logo {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 24px;
                          }
                          .main-sidebar {
                            font-size: 20px; 
                          }
                        '))),
                        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #f4b943;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #f4b943;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #f4b943;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #f4b943;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ff0000;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #ff69b4;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff69b4;
                                }
                                
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #7da2d1;
                                }

                                '))),
                        
                        br(),br(),br(),br(),
                        tabItems(
                            tabItem(tabName = "introduction",
                                    
                                    fluidRow(
                                        (
                                            column(
                                                width = 12, 
                                                br(),
                                                tags$div(
                                                    tags$h2("We are looking to understand how the Covid 19 Pandemic affected the Airbnb market in the city of Boston."),
                                                    br(),
                                                    img(src = "osman-rana-xJueGJJHnWs-unsplash.jpg", width = "800", height = "500"),
                                                    HTML('</br>Aerial View of Boston (Photo by <a href="https://unsplash.com/@osmanrana?utm_source=unsplash&amp;utm_medium=referral&amp;utm_content=creditCopyText">Osman Rana</a> on <a href="https://unsplash.com/s/photos/boston-covid-19?utm_source=unsplash&amp;utm_medium=referral&amp;utm_content=creditCopyText">Unsplash</a>)'),
                                                    style="text-align: center;"
                                                )
                                            )
                                            
                                        )
                                    ),
                                    fluidRow(
                                        br(),br(),br(),
                                        column(width = 1),
                                        column(width = 6,HTML('<div><p><em>* Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a>.</em></p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_cases",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            plotOutput("cases"),
                                            HTML('<br/><div><p><em>* Source <a href="https://www.mass.gov/info-details/covid-19-response-reporting">Mass.gov</a>.</em></p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_listings",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            plotOutput("listings"),
                                            HTML('<div><p><em>* Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a>.</em></p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_reviews",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            plotOutput("reviews"),
                                            HTML('<div><p><em>* Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a>.</em></p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_reviews2",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            plotOutput("reviews2"),
                                            HTML('<div><p><em>* Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a>.</em></p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_newreviews",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            align = "center",
                                            # tags$div(
                                            #     tags$h2("We are looking to understand better how the Covid 19 Pandemic affected the Airbnb market in the city of Boston."),
                                            #     br(),
                                            #     plotOutput("newreviews",width = 800),
                                            #     HTML('<div><p><em>* Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a>.</em></p></div>'),
                                            #     style="text-align: center;"
                                            # )
                                            plotOutput("newreviews",width = 1000)
                                        )
                                    ),
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            HTML('<div><p><em>* Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a>.</em></p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "conclusions",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            HTML('<div><p>The COVID-19 pandemic has affected&nbsp;</p></div>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "about",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            HTML('
                                            <h1>Author: Ariel Perez<br/>
                                            Email: arielperez@acm.org<br/>
                                            Created: October 12, 2020<br/><br/>
                                            Packages:<br/>
                                            RStudio Version 1.3.1073<br/>
                                            Shiny version 1.5.0<br/>
                                            Shinydashboard version 0.7.1<br/>
                                            Tidyverse version 1.3.0<br/>
                                            Lubridate version 1.7.9<br/>
                                            Scales version 1.1.1<br/><br/>
                                            Data used in this project:<br/>
                                            <a href="http://insideairbnb.com/index.html">Inside Airbnb</a><br/>
                                            <a href="https://www.mass.gov/info-details/covid-19-response-reporting">Mass.gov</a></h1>
                                            ')
                                        )
                                    )
                            )
                        )
                    )
)



server <- function(input, output) {
    
    output$cases <- renderPlot({
        cases_by_date %>% 
            ggplot(aes(x = month, y = total.cases)) +
            geom_bar(stat = 'identity', color = 'blue', fill = 'blue') +
            labs(title = 'Reported COVID-19 Cases in the State of Massachusetts in 2020', x = 'Month', y = 'Number of Cases') +
            geom_text(aes(label=format(total.cases,big.mark = ',')), position=position_dodge(width=0.9), vjust=-0.25) +
            scale_y_continuous(labels = comma)
    })
    output$listings <- renderPlot({
        listings_2020 %>%
            ggplot(aes(x = last_scrape_date, y = total_listings)) +
            geom_line(aes(group = 1), color = 'red') +
            labs(title = 'Number of Listings in the city of Boston', x = 'Date data was gathered', y = 'Number of Listings')+
            scale_y_continuous(labels = comma) +
            scale_x_date(date_labels = '%Y/%m/%d', breaks = listings_2020$last_scrape_date)
    })
    output$reviews <- renderPlot({
        reviews_2020 %>% 
            ggplot(aes(x = last_scrape_date, y = total_reviews_count)) +
            geom_line(aes(group = 1), color = 'blue') +
            labs(title = 'Total Number of Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
            scale_y_continuous(labels = comma) +
            scale_x_date(date_labels = '%Y/%m/%d', breaks = reviews_2020$last_scrape_date)
    })
    output$reviews2 <- renderPlot({
        reviews2_2020 %>% 
            ggplot(aes(x = last_scrape_date, y = total_reviews_count)) +
            geom_line(aes(group = 1), color = 'blue') +
            labs(title = 'Total Number of Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
            scale_y_continuous(labels = comma) +
            scale_x_date(date_labels = '%Y/%m/%d', breaks = reviews2_2020$last_scrape_date)
    })
    output$newreviews <- renderPlot({
        newreviews_2020 %>% 
            ggplot(aes(x = last_scrape_date, y = new_reviews)) +
            geom_line(aes(group = 1), color = 'blue') +
            labs(title = 'Total Number of New Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
            scale_y_continuous(labels = comma) +
            scale_x_date(date_labels = '%Y/%m/%d', breaks = newreviews_2020$last_scrape_date)
        
    })
}


shinyApp(ui, server)