#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(shinydashboard)

us_region <- thanksgiving_db %>% distinct(us_region)
community <- thanksgiving_db %>% distinct(community_type)
age <- thanksgiving_db %>% distinct(age) 
gender <- thanksgiving_db %>% distinct(gender)
family_income <- thanksgiving_db %>% distinct(family_income)
type <- thanksgiving_db %>% distinct(type)


ui <- dashboardPage(
    dashboardHeader(title = "Thanksgiving Meal Statistics in US", titleWidth = 500),
    dashboardSidebar(
        selectInput("v_us_region", "US Region", choices = us_region, selected = "New England")
#        selectInput("v_community", "Community", choices = community, selected = "Suburban"),
#        selectInput("v_age", "Age", choices = age, selected = "18 - 29"),
#        selectInput("v_gender", "Gender", choices = gender, selected = "Male"),
#        selectInput("v_family_income", "Family Income", choices = family_income, selected = "Don't want to disclose"),
#        selectInput("v_type", "Food Type", choices = type, selected = "side")
        
    ),
    dashboardBody(
        fluidRow(box(plotOutput("gender_ratio")),
                 box(plotOutput("community_type"))),
        fluidRow(box(plotOutput("age")),
                 box(plotOutput("family_income")))
        
    )
)


server <- function(input, output) {
    
    output$gender_ratio <- renderPlot({
        thanksgiving_db %>% 
            filter(us_region == input$v_us_region) %>% 
            count(gender) %>% 
            mutate(percent = n/sum(n)) %>% 
            ggplot(aes(fct_reorder(gender,percent), percent, fill = gender)) +
            geom_col(show.legend = F) +
            scale_y_continuous(labels = percent_format()) +
            labs(title = "Gender Ratio",
                 x = "",
                 y = "") +
            coord_flip()
    })
    
    output$community_type <- renderPlot({
        thanksgiving_db %>% 
            filter(us_region == input$v_us_region) %>% 
            count(community_type) %>% 
            mutate(percent = n/sum(n)) %>% 
            ggplot(aes(fct_reorder(community_type, percent), percent, fill = community_type)) +
            geom_col(show.legend = F) +
            scale_y_continuous(labels = percent_format()) +
            labs(title = "Community Types",
                 x = "",
                 y = "") +
            coord_flip()
    })
    
    output$age <- renderPlot({
        thanksgiving_db %>% 
            filter(us_region == input$v_us_region) %>% 
            count(age) %>% 
            mutate(percent = n/sum(n)) %>% 
            ggplot(aes(fct_reorder(age, percent), percent, fill = age)) +
            geom_col(show.legend = F) +
            scale_y_continuous(labels = percent_format()) +
            labs(title = "Various Age Groups",
                 x = "",
                 y = "") +
            coord_flip()
    })
    
    output$family_income <- renderPlot({
        thanksgiving_db %>% 
            filter(us_region == input$v_us_region) %>% 
            count(family_income) %>% 
            mutate(percent = n/sum(n)) %>% 
            ggplot(aes(fct_reorder(family_income,percent), percent, fill = family_income)) +
            geom_col(show.legend = F) +
            scale_y_continuous(labels = percent_format()) +
            labs(title = "Family Income",
                 x = "",
                 y = "") +
            coord_flip()
    })
}


shinyApp(ui, server)
