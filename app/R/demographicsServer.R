# DEMOGRAPHICS SERVER MODULES 

# Date Selector
dateServer <- function(id, covid.race) {
  moduleServer(
    id,
    function(input, output, session) {
      # Prepare Data
      date_val <- covid.race$date
      date.min <- min(date_val)
      date.max <- max(date_val)
      
      # Making Date Selector
      output$date_ui <- renderUI({
        dateInput("user_date", "Select date:",
                  min = date.min,
                  max = date.max,
                  value = date.max,
                  format = "mm/dd/yy")
      })
    }
  )
}
# Data Switcher for Population Adjustment
adjServer <- function(id, covid.age, covid.race, covid.sex) {
  moduleServer(
    id,
    function(input, output, session) {
      # Create adjusted population variables
      #  Age
      pop.age <- pop %>%
        group_by(age) %>%
        summarize(pop = sum(pop))
      
      covid.age <- covid.age %>%
        inner_join(pop.age, by = "age") %>%
        mutate(
          cases.adj = cases * 100000 / pop,
          hospts.adj = hospts * 100000 / pop,
          deaths.adj = deaths * 100000 /pop
        )
      
      rm(pop.age)
      #  Race
      pop.race <- pop %>%
        group_by(race) %>%
        summarize(pop = sum(pop))
      
      covid.race <- covid.race %>%
        inner_join(pop.race, by = "race") %>%
        mutate(
          cases.adj = cases * 100000 / pop,
          hospts.adj = hospts * 100000 / pop,
          deaths.adj = deaths * 100000 /pop
        )
      
      rm(pop.race)
      #  Sex
      pop.sex <- pop %>%
        group_by(sex) %>%
        summarize(pop = sum(pop))
      
      covid.sex <- covid.sex %>%
        inner_join(pop.sex, by = "sex") %>%
        mutate(
          cases.adj = cases * 100000 / pop,
          hospts.adj = hospts * 100000 / pop,
          deaths.adj = deaths * 100000 /pop
        )
      
      rm(pop.sex)
      
      # Actual Data Switcher
      target_name <- reactive({
        paste0(
          input$target,
          if(input$pop_adj) {
            ".adj"
          }
        )
      })
      #  Age
      age_target <- reactive({
        age %>%
          select(ends_with(target_name())) %>%
          .[[1]]
      })
      #  Race
      race_target <- reactive({
        race %>%
          select(ends_with(target_name())) %>%
          .[[1]]
      })
      #  Sex
      sex_target <- reactive({
        sex %>%
          select(ends_with(target_name())) %>%
          .[[1]]
      })
    }
  )
}
# Date Variables
dateVarServer <- function(id, covid.age, covid.race, covid.sex) {
  moduleServer(
    id,
    function(input, output, session) {
      # Age
      date_val <- covid.age$date
      date.max <- max(date_val)
      
      age_date_sel <- reactive({
        if(length(input$user_date) == 0) {
          subset(age_target(), date == date.max)
        }
        else {
          subset(age_target(), date == input$user_date)
        }
      })

      # Race
      date_val <- covid.race$date
      date.max <- max(date_val)
      
      race_date_sel <- reactive({
        if(length(input$user_date) == 0) {
          subset(race_target(), date == date.max)
        }
        else {
          subset(race_target(), date == input$user_date)
        }
      })

      # Sex
      date_val <- covid.sex$date
      date.max <- max(date_val)
      
      
      sex_date_sel <- reactive({
        if(length(input$user_date) == 0) {
          subset(sex_target(), date == date.max)
        }
        else {
          subset(sex_target(), date == input$user_date)
        }
      })

    }
  )
}
# Mode Chosen by User
modeServer <- function(id, covid.age, covid.race, covid.sex) {
  moduleServer(
    id,
    function(input, output, session) {
      # Age
      age_mode <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = age_date_sel()$cases.adj,
                 hospts = age_date_sel()$hospts.adj,
                 deaths = age_date_sel()$deaths.adj)
        }
        else {
          switch(input$mode,
                 cases = age_date_sel()$cases,
                 hospts = age_date_sel()$hospts,
                 deaths = age_date_sel()$deaths)
        }
      })
      # Race
      race_mode <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = race_date_sel()$cases.adj,
                 hospts = race_date_sel()$hospts.adj,
                 deaths = race_date_sel()$deaths.adj)
        }
        else {
          switch(input$mode,
                 cases = race_date_sel()$cases,
                 hospts = race_date_sel()$hospts,
                 deaths = race_date_sel()$deaths)
        }
      })
      # Sex
      sex_mode <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = sex_date_sel()$cases.adj,
                 hospts = sex_date_sel()$hospts.adj,
                 deaths = sex_date_sel()$deaths.adj)
        }
        else {
          switch(input$mode,
                 cases = sex_date_sel()$cases,
                 hospts = sex_date_sel()$hospts,
                 deaths = sex_date_sel()$deaths)
        }
      })
    }
  )
}

# Plot Customization
customServer <- function(id, covid.age, covid.race, covid.sex) {
  moduleServer(
    id,
    function(input, output, session) {
      # Age
      age_title <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = "Cases by Age\n(Population Adjusted)",
                 hospts = paste0("Hospitalizations by Age\n",
                                 "(Population Adjusted)"),
                 deaths = "Deaths by Age\n(Population Adjusted)")
        }
        else {
          switch(input$mode,
                 cases = "Cases by Age\n(Total)",
                 hospts = paste0("Hospitalizations by Age\n",
                                 "(Total)"),
                 deaths = "Deaths by Age\n(Total)")
        }
      })
      
      age_y_label <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = "Cases per 100k",
                 hospts = "Hospitalizations per 100k",
                 deaths = "Deaths per 100k")
        }
        else {
          switch(input$mode,
                 cases = "Cases",
                 hospts = "Hospitalizations",
                 deaths = "Deaths")
        }
      })
      
      age_color <- reactive({
        switch(input$mode,
               cases = list(color = '#d47d3d',
                            line = list(color = '#c23719',
                                        width = 1.5)),
               hospts = list(color = '#a7a3cd',
                             line = list(color = '#7760ab',
                                         width = 1.5)),
               deaths = list(color = '#808080',
                             line = list(color = '#3a3a3a',
                                         width = 1.5))
               )
      })
      
      age_tooltips <- reactive({
        if(input$pop_adj) {
          paste0(
            "Years of Age: ", age_date_sel()$age_group, "\n",
            "Date: ", format(
              age_date_sel()$date, "%D"), "\n\n",
            "Cases per 100k: ", floor(
              age_date_sel()$cases.adj), "\n",
            "Hospitalizations per 100k: ", floor(
              age_date_sel()$hospts.adj), "\n",
            "Deaths per 100k: ", floor(
              age_date_sel()$deaths.adj), "\n"
          )
        }
        else {
          paste0(
          "Years of Age: ", age_date_sel()$age_group, "\n",
          "Date: ", format(
            age_date_sel()$date, "%D"), "\n\n",
          "Total Cases: ", floor(
            age_date_sel()$cases), "\n",
          "Total Hospitalizations: ", floor(
            age_date_sel()$hospts), "\n",
          "Total Deaths: ", floor(
            age_date_sel()$deaths), "\n"
          )
        }
      })
      # Race
      race_title <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = "Cases by Minority<br />(Population Adjusted)<br />",
                 hospts = paste0("Hospitalizations by Minority<br />",
                                 "(Population Adjusted)<br />"),
                 deaths = "Deaths by Minority<br />(Population Adjusted)<br />")
        }
        else {
          switch(input$mode,
                 cases = "Cases by Minority<br />(Total)<br />",
                 hospts = paste0("Hospitalizations by Minority<br />",
                                 "(Total)<br />"),
                 deaths = "Deaths by Minority<br />(Total)<br />")
        }
      })
      
      race_tooltips <- reactive({
        if(input$pop_adj) {
          paste0(
            "Race: ", race_date_sel()$race, "\n",
            "Date: ", format(
              race_date_sel()$date, "%D"), "\n\n",
            "Cases per 100k: ", floor(
              race_date_sel()$cases.adj), "\n",
            "Hospitalizations per 100k: ", floor(
              race_date_sel()$hospts.adj), "\n",
            "Deaths per 100k: ", floor(
              race_date_sel()$deaths.adj), "\n"
          )
        }
      })
      # Sex
      sex_title <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = "Cases by Sex\n(Population Adjusted)",
                 hospts = paste0("Hospitalizations by Sex\n",
                                 "(Population Adjusted)"),
                 deaths = "Deaths by Sex\n(Population Adjusted)"
                 )
        }
        else {
          switch(input$mode,
                 cases = "Cases by Sex\n(Total)",
                 hospts = paste0("Hospitalizations by Sex\n",
                                 "(Total)"),
                 deaths = "Deaths by Sex\n(Total)"
          )
        }
      })
      
      sex_y_label <- reactive({
        if(input$pop_adj) {
          switch(input$mode,
                 cases = "Cases per 100k",
                 hospts = "Hospitalizations per 100k",
                 deaths = "Deaths per 100k")
        }
        else {
          switch(input$mode,
                 cases = "Cases",
                 hospts = "Hospitalizations",
                 deaths = "Deaths")
        }
      })
      
      sex_tooltips <- reactive({
        if(input$pop_adj) {
          paste0(
            "Sex :", sex_date_sel()$sex, "\n",
            "Date: ", format(
              sex_date_sel()$date, "%D"), "\n\n",
            "Cases per 100k: ", floor(
              sex_date_sel()$cases.adj), "\n",
            "Hospitalizations per 100k: ", floor(
              sex_date_sel()$hospts.adj), "\n",
            "Deaths per 100k: ", floor(
              sex_date_sel()$deaths.adj), "\n"
            )
        }
        else {
          paste0(
            "Sex :", sex_date_sel()$sex, "\n",
            "Date: ", format(
              sex_date_sel()$date, "%D"), "\n\n",
            "Total Cases: ", floor(
              sex_date_sel()$cases), "\n",
            "Total Hospitalizations: ", floor(
              sex_date_sel()$hospts), "\n",
            "Total Deaths: ", floor(
              sex_date_sel()$deaths), "\n"
          )
        }
      })
    }
  )
}
# The Actual Plots
plotServer <- function(id, covid.age, covid.race, covid.sex) {
  moduleServer(
    id,
    function(input, output, session) {
      # Age
      output$age <- renderPlotly({
        plot_ly(
          x = ~age_date_sel()$age,
          y = ~age_mode(),
          type = "bar",
          marker = ~age_color(),
          hoverinfo = ~'text',
          text = ~age_tooltips()
        ) %>% layout(
          title = ~age_title(),
          xaxis = list(title = "Age Groups",
                       showgrid = FALSE, fixedrange = TRUE),
          yaxis = list(title = ~age_y_label(),
                       showgrid = FALSE, fixedrange = TRUE)
        ) %>% config(
          displayModeBar = FALSE,
          displaylogo = FALSE,
          showTips = FALSE
        )
      })
      # Race
      output$race <- renderPlotly({
        plot_ly(
          covid.race,
          labels = ~race_date_sel()$race,
          values = ~race_date_sel(),
          type = "pie",
          textinfo = 'percent',
          marker = list(
            colors = c(
              'rgb(102,194,165)',
              'rgb(252,141,98)',
              'rgb(141,160,203)',
              'rgb(231,138,195)',
              'rgb(166,216,84)',
              'rgb(255,217,47)',
              'rgb(179,179,179)',
              'rgb(229,196,148)'
            ),
            line = list(color = 'rgb(255, 255, 255)', width = 1)
          ),
          sort = T,
          hoverinfo = 'text',
          textposition = 'inside',
          text = ~race_tooltips(),
          showlegend = T
        ) %>% layout(
          title = ~race_title(),
          legend = list(x = 0,
                        y = -100,
                        orientation = 'h'),
          margin = list(t = 75)
        ) %>% config(
          displayModeBar = FALSE,
          displaylogo = FALSE,
          showTips = FALSE
        )
      })
      # Sex
      output$sex <- renderPlotly({
        plot_ly(
          x = ~sex_date_sel()$sex,
          y = ~sex_date_sel(),
          type = "bar",
          marker = list(
            color = c(
              'rgb(237, 119, 215)', 
              'rgb(6, 98, 204)',  
              'rgb(107, 107, 107)'
            ),
            line = list(color = c(
              'rgb(168, 50, 146)',
              'rgb(18, 69, 128)',
              'rgb(54, 54, 54)'
            ),
            width = 1)
          ),
          hoverinfo = 'text',
          text = ~sex_tooltips()
        ) %>% layout(
          title = ~sex_title(),
          yaxis = list(title = "Gender", showgrid = FALSE, fixedrange = TRUE),
          xaxis = list(title = ~sex_y_label(),
                       showgrid = FALSE, fixedrange = TRUE)
        ) %>% config(
          displayModeBar = FALSE,
          displaylogo = FALSE,
          showTips = FALSE
        )
      })
    }
  )
}
