######################
# setup
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(scales)

listings_2020 <- read_csv('listings_2020.csv')
cases_by_date <- read_csv('CasesByDate.csv')
reviews_2020 <- read_csv('reviews_2020.csv')
reviews2_2020 <- read_csv('reviews2_2020.csv')
newreviews_2020 <- read_csv('newreviews_2020.csv')
newreviews_2019 <- read_csv('newreviews_2019.csv')

cases_by_date$month <- factor(x = cases_by_date$month, levels = cases_by_date$month)

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
                        tags$head(tags$style(HTML(".small-box {height: 35px; width: 130px}"))),
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
                                                    tags$h2("The goal of this website is to gain an understanding of how the COVID-19 Pandemic affected the Airbnb marketplace in the city of Boston."),
                                                    br(),
                                                    img(src = "osman-rana-xJueGJJHnWs-unsplash.jpg", width = "700", height = "400"),
                                                    HTML('</br>Aerial View of Boston (Photo by <a href="https://unsplash.com/@osmanrana?utm_source=unsplash&amp;utm_medium=referral&amp;utm_content=creditCopyText">Osman Rana</a> on <a href="https://unsplash.com/s/photos/boston-covid-19?utm_source=unsplash&amp;utm_medium=referral&amp;utm_content=creditCopyText">Unsplash</a>)'),
                                                    style="text-align: center;"
                                                )
                                            )
                                            
                                        )
                                    ),
                                    fluidRow(
                                        br(),br(),br(),
                                        column(width = 2),
                                        column(width = 8,
                                               
                                               HTML('
                                               <h3>&nbsp;</h3>
<ul style="list-style-type: circle;">
<li>
<h3>First, we analyzed the number of reported COVID-19 cases in the state of Massachusetts. This helped us understand and identify the worst time of the pandemic in the year 2020.</h3>
</li>
<li>
<h3>Second, we analyzed the number of listings in the Boston area to understand how the pandemic affected the Airbnb marketplace.</h3>
</li>
<li>
<h3>Third, we took a look at the number of reviews posted in order to get an idea of the occupancy rate.</h3>
</li>
<li>
<h3>Fourth, we took a look at new reviews for listings that were available in the Airbnb marketplace from January to October in 2020.</h3>
</li>
<li>
<h3>Last, a summary of our findings.</h3>
</li>
</ul>
<p>&nbsp;</p>
                                                    '),
                                               HTML('<h3>&rArr; Use the sidebar to navigate the site.</h3>
<h3>&nbsp;</h3>
<h3>Thank you for taking the time to visit this website!</h3>')
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_cases",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 8,
                                            align = "right",
                                            plotOutput("cases",width = 1000, height = 700)
                                        ),
                                        column(
                                            align = 'left',
                                            width = 4,
                                            box(
                                                title = 'Analysis',
                                                height = 700,
                                                width = 8,
                                                HTML('<p>This bar graph displays the number of cases of COVID-19<sup>1</sup> in the state of Massachusetts. Since this report was created in October 2020, the data in the bar graph is only available until October 13, 2020.</p>
<p>Starting in early March cases of COVID-19 started showing up in considerable numbers, but it was in the months of April and May that the number of cases reached the highest levels.</p>
<p>Starting in late March the governor of Massachusetts issued several emergency orders<sup>2</sup> to try to manage the infection rate. Among those orders was a mandate to wear a mask in public places, limit to the number of people that can gather together, and asking non-essential workers to stay home. These actions and others helped bring the number of cases down to a manageable level in the months of June and July.</p>
<p>Starting in late May the governor of Massachusetts started implementing a plan to reopen the economy<sup>3</sup>. Initially, the number of cases was kept under a manageable level (June and July) but starting in August the number of cases started increasing.</p>
<p>As of the date of this report(October 19, 2020), the number of cases in Massachusetts continues to increase.&nbsp;</p>
<p><br /><br /></p>
<p><small><sup>1</sup>Source <a href="https://www.mass.gov/info-details/covid-19-response-reporting">Mass.gov</a><br /> <sup>2,3</sup>Source <a href="https://www.boston.gov/departments/public-health-commission/coronavirus-timeline">boston.gov</a></small></p>')
                                            )
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_listings",
                                    br(),br(),
                                    fluidRow(
                                        column(
                                            width = 8,
                                            align = "right",
                                            plotOutput("listings",width = 1000, height = 550)
                                        ),
                                        column(
                                            align = 'left',
                                            width = 4,
                                            box(
                                                title = 'Analysis',
                                                height = 550,
                                                width = 8,
                                                HTML('<p>This line graph displays the number of Airbnb listings<sup>1</sup> in the Boston area for 2020.</p>
<p>As expected, the number of listings started going down mid-march, around the time the COVID-19 started to spread in Massachusetts in considerable numbers.</p>
<p>April and May had the highest number of reported cases in Massachusetts (see ?), and at that time the state took several emergency actions to lower the number of cases making it harder for people to visit Boston. Hence, the number of listings available in Airbnb started going down in the Boston area.</p>
<p>The number of listings reached its lower level in early June, but after the number of COVID-19 cases came down to a manageable level in July, it started to increase slightly and shows an upward trend by the end of August.</p>
<br/><br/><br/><br/><p><small><sup>1</sup>Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a></small></p>')
                                            )
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_reviews",
                                    fluidRow(
                                        column(
                                            width = 8,
                                            align = "right",
                                            plotOutput("reviews",width = 1000, height = 400)
                                        ),
                                        column(
                                            align = 'left',
                                            width = 4,
                                            box(
                                                title = 'Analysis',
                                                height = 400,
                                                width = 8,
                                                HTML('<p>This line graph displays the cumulative number of reviews posted to Airbnb listings<sup>1</sup> in the Boston area in 2020.&nbsp;</p>
<p>Starting mid-February we can see a downward trend in the number of reviews. This may lead us to believe that the occupancy rate has decreased, but it is hard to tell from the information displayed in the graph.&nbsp; It would be more reasonable to think that since the number of listings has come down, that the number of reviews in this line graphs reflects the decrease in the number of listings.</p>
')
                                            )
                                        )
                                    ),
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 8,
                                            align = "right",
                                            plotOutput("reviews2",width = 1000, height = 400)
                                        ),
                                        column(
                                            align = 'left',
                                            width = 4,
                                            box(
                                                height = 400,
                                                width = 8,
                                                HTML('<p>This line graph displays the same information as the line graph above except that it only includes listings that were available in Airbnb from Jan through October.</p>
<p>As we can see, the number of reviews kept increasing during the COVID-19 pandemic. This is an interesting find and indicates that during the COVID-19 pandemic people stayed at Airbnb locations in the Boston area.&nbsp;</p>
<p>It is important to understand that this line graph is displaying cumulative data. Therefore, we are not able to discern if the number of people staying in Airbnb locations increased or decreased but only that people were using the Airbnb listings.</p>
<br/><br/><p><small><sup>1</sup>Source <a href="http://insideairbnb.com/index.html">Inside Airbnb</a></small></p>')
                                            )
                                        )
                                    )
                            ),
                            tabItem(tabName = "boston_newreviews",
                                    fluidRow(
                                        column(
                                            width = 1,
                                            br(),br(),br(),br(),br(),
                                            valueBoxOutput("minBox",width = NULL),
                                            br(),
                                            valueBoxOutput("maxBox",width = NULL),
                                            br(),
                                            valueBoxOutput("averageBox",width = NULL)
                                        ),
                                        column(
                                            width = 7,
                                            align = "left",
                                            plotOutput("newreviews",width = 975, height = 400)
                                        ),
                                        column(
                                            align = 'left',
                                            width = 4,
                                            box(
                                                title = 'Analysis',
                                                height = 400,
                                                width = 8,
                                                HTML('<p>The line charts to the left are showing the total numbers of new reviews added to the existing listings from January to August in 2020 and 2019. Only listings that were present for the entire period were included in the charts.</p>
<p>If we consider that most guests usually write a review days after staying at a property, we can make the assumption that the number of new reviews is a good indicator of the occupancy rate.</p>
<p>Comparing both figures, it is easy to see that the minimum number of new reviews in 2019 was higher than the maximum number of reviews in 2020. This indicates that 2020 had a considerably lower occupancy rate than 2019.</p>')
                                            )
                                        )
                                    ),
                                    fluidRow(
                                        column(
                                            width = 1,
                                            br(),br(),br(),br(),br(),
                                            valueBoxOutput("minBox2",width = NULL),
                                            br(),
                                            valueBoxOutput("maxBox2",width = NULL),
                                            br(),
                                            valueBoxOutput("averageBox2",width = NULL)
                                        ),
                                        column(
                                            width = 7,
                                            align = "left",
                                            plotOutput("newreviews2",width = 975, height = 400)
                                        ),
                                        column(
                                            width = 4,
                                            box(
                                                height = 400,
                                                width = 8,
                                                HTML('<p>Taking a look at the 2020 line chart we can see that between the months of April and the beginning of July the occupancy rate took a dip compared to the rest of the year. This reflects the months that the COVID-19 pandemic was at its worst in the state of Massachusetts.</p>
<p>The COVID-19 pandemic has considerably affected the Airbnb marketplace in Boston.&nbsp;A positive sign is that by the end of August 2020 the number of new reviews is on the rise, albeit, still below the 2019 levels.</p>
')
                                            )
                                        )
                                    ),
                                    fluidRow(
                                        br(),br(),
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
                                            box(
                                                HTML('<h3 class="box-title">Conclusion</h3>
<p>The COVID-19 pandemic has clearly affected the number of Airbnb listings in the Boston area in a negative way. It is easy to see from the data that the number of listings has decreased, and this implies fewer properties available for rent in the marketplace.&nbsp; That said, the number of reviews indicates that people are still renting Airbnb locations, albeit in fewer numbers but still it is a positive sign.</p>
<p>As of the date of this report, the infection rate in Massachusetts is increasing. This could mean that the small upward trend visible in the number of listings and reviews in the month of August may not stay that way for long.</p>
<p>We have only gained a small understanding of this issue, there still a lot of questions unanswered that may be good topics for future research.&nbsp;</p>
<ul>
<li>For those hosts that have been able to rent their properties, what are they doing differently than the others? Maybe the listing description may tell us something.</li>
<li>Has the price of the properties changed? Would cheaper prices entice people to stay at an Airbnb location during the pandemic?</li>
<li>How has a lower occupancy rate affected the economy of Boston?</li>
<li>How many listings are for rooms and full apartments? Would the risk of infection deter people from listing their rooms?</li>
</ul>
                                                     ')
                                            )
                                        )
                                    )
                            ),
                            tabItem(tabName = "about",
                                    fluidRow(
                                        br(),br(),br(),
                                        column(
                                            width = 12,
                                            box(
                                                HTML('
                                            <h3>Author: Ariel Perez<br /> Email: arielperez@acm.org<br /> Created: October 12, 2020<br />
                                            Git URL: <a href="https://github.com/hikarinosenshi/shiny_project">https://github.com/hikarinosenshi/shiny_project</a><br/>
LinkedIn: <a class="pv-contact-info__contact-link link-without-visited-state t-14" href="https://www.linkedin.com/in/ariel-perez-9508213">linkedin.com/in/ariel-perez-9508213</a></p></h3>
<p>&nbsp;</p>
<h4>Packages used:<br /> RStudio Version 1.3.1073<br />
Shiny version 1.5.0<br /> Shinydashboard version 0.7.1<br />
Tidyverse version 1.3.0<br /> Lubridate version 1.7.9<br />
Scales version 1.1.1<br /><br /> Data used in this project:<br />
<a href="http://insideairbnb.com/index.html">Inside Airbnb</a><br />
<a href="https://www.mass.gov/info-details/covid-19-response-reporting">Mass.gov</a></h4>
                                            ')
                                            )
                                            
                                            
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
            labs(title = 'Total Number of New Reviews (2020)', x = 'Date data was gathered', y = 'Number of Reviews')+
            scale_y_continuous(labels = comma) +
            scale_x_date(date_labels = '%Y/%m/%d', breaks = newreviews_2020$last_scrape_date)
        
    })
    output$newreviews2 <- renderPlot({
        newreviews_2019 %>% 
            ggplot(aes(x = last_scrape_date, y = new_reviews)) +
            geom_line(aes(group = 1), color = 'blue') +
            labs(title = 'Total Number of New Reviews (2019)', x = 'Date data was gathered', y = 'Number of Reviews')+
            scale_y_continuous(labels = comma) +
            scale_x_date(date_labels = '%Y/%m/%d', breaks = newreviews_2019$last_scrape_date)
        
    })
    
    output$minBox <- renderValueBox({
        valueBox(
            value = tags$p(paste('Min:',format(round(min(newreviews_2020$new_reviews)), big.mark = ',')),style = "font-size: 40%;"),
            subtitle = NULL
        )
    })
    output$maxBox <- renderValueBox({
        valueBox(
            value = tags$p(paste('Max:',format(round(max(newreviews_2020$new_reviews)), big.mark = ',')),style = "font-size: 40%;"),
            subtitle = NULL
        )
    })
    output$averageBox <- renderValueBox({
        valueBox(
            value = tags$p(paste('Average:',format(round(mean(newreviews_2020$new_reviews)), big.mark = ',')),style = "font-size: 40%;"),
            subtitle = NULL
            
        )
    })
    output$minBox2 <- renderValueBox({
        valueBox(
            value = tags$p(paste('Min:',format(round(min(newreviews_2019$new_reviews)), big.mark = ',')), style = "font-size: 40%;"),
            subtitle = NULL
        )
    })
    output$maxBox2 <- renderValueBox({
        valueBox(
            value = tags$p(paste('Max:',format(round(max(newreviews_2019$new_reviews)), big.mark = ',')), style = "font-size: 40%;"),
            subtitle = NULL
        )
    })
    output$averageBox2 <- renderValueBox({
        valueBox(
            value = tags$p(paste('Average:',format(round(mean(newreviews_2019$new_reviews)), big.mark = ',')), style = "font-size: 40%; text-align: left;"),
            subtitle = NULL
        )
    })
    
    
}

shinyApp(ui, server)