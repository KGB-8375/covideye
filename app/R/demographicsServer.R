# DEMOGRAPHICS SERVER MODULES 

inputServer <- function(id, min_date, max_date) {
  moduleServer(
    id,
    function(input, output, session) {
      output$date_ui <- renderUI({
        ns <- session$ns
        dateInput(ns("user_date"), "Select Date",
                  min    = min_date,
                  max    = max_date,
                  value  = max_date,
                  format = "m/d/yy"
        )
      })
      
      return(
        list(
          date = reactive({input$user_date}),
          mode = reactive({input$mode}),
          adj  = reactive({input$pop_adj})
        )
      )
    }
  )
}

ageServer <- function(id, covid.age, inputs, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      # Fancy Num Setup
      fancy_num <- function(x) {
        x %>%
          round() %>%
          format(
            big.mark   = ",",
            scientific = FALSE,
            trim       = TRUE
          )
      }
      
      # Age Data Setup
      pop.age <- pop %>%
        group_by(ages) %>%
        summarise(pop = sum(pop))
      
      covid.age <- covid.age %>%
        inner_join(pop.age, by = "ages") %>%
        mutate(
          cases.adj  = cases  * 100000 / pop,
          hospts.adj = hospts * 100000 / pop,
          deaths.adj = deaths * 100000 / pop
        )
      
      rm(pop.age)
      
      # Age Variable Creation
      date_sel <- reactive({
        req(inputs$date())
        
        covid.age %>% 
          filter(date == inputs$date())
      })
      
      target <- reactive({
        date_sel() %>%
          select(
            ends_with(paste0(
              inputs$mode(),
              if(inputs$adj()) {
                ".adj"
              }
            ))
          ) %>%
          .[[1]]
      })
      
      # Age Plot Customization
      age_title <- reactive({
        if(inputs$adj()) {
          switch(
            inputs$mode(),
            cases  = "Cases by Age\n(Population Adjusted)",
            hospts = "Hospitalizations by Age\n(Population Adjusted)",
            deaths = "Deaths by Age\n(Population Adjusted)"
          )
        }
        else {
          switch(
            inputs$mode(),
            cases  = "Cases by Age\n(Total)",
            hospts = "Hospitalizations by Age\n(Total)",
            deaths = "Deaths by Age\n(Total)"
          )
        }
      })
      
      age_y_label <- reactive({
        if(inputs$adj()) {
          switch(
            inputs$mode(),
            cases  = "Cases per 100k",
            hospts = "Hospitalizations per 100k",
            deaths = "Deaths per 100k"
          )
        }
        else {
          switch(
            inputs$mode(),
            cases  = "Cases",
            hospts = "Hospitalizations",
            deaths = "Deaths"
          )
        }
      })
      
      age_color <- reactive({
        switch(
          inputs$mode(),
          cases = list(
            color = '#d47d3d',
            line  = list(
              color = '#c23719',
              width = 1.5)
          ),
          hospts = list(
            color = '#a7a3cd',
            line  = list(
              color = '#7760ab',
              width = 1.5)
          ),
          deaths = list(
            color = '#808080',
            line  = list(
              color = '#3a3a3a',
              width = 1.5)
          )
        )
      })
      
      age_tooltips <- reactive({
        if(inputs$adj()) {
          paste0(
            "Years of Age: ", date_sel()$ages, "\n",
            "Date: ", format(date_sel()$date, "%D"), "\n\n",
            "Cases per 100k: ", fancy_num(date_sel()$cases.adj), "\n",
            "Hospitalizations per 100k: ", fancy_num(date_sel()$hospts.adj), "\n",
            "Deaths per 100k: ", fancy_num(date_sel()$deaths.adj), "\n"
          )
        }
        else {
          paste0(
            "Years of Age: ", date_sel()$ages, "\n",
            "Date: ", format(date_sel()$date, "%D"), "\n\n",
            "Total Cases: ", fancy_num(date_sel()$cases), "\n",
            "Total Hospitalizations: ", fancy_num(date_sel()$hospts), "\n",
            "Total Deaths: ", fancy_num(date_sel()$deaths), "\n"
          )
        }
      })
      
      # Age Plotly
      output$age <- renderPlotly({
        plot_ly(
          x         = ~date_sel()$ages,
          y         = ~target(),
          type      = "bar",
          marker    = ~age_color(),
          hoverinfo = ~'text',
          text      = ~age_tooltips()
        ) %>% layout(
          title = ~age_title(),
          xaxis = list(
            title      = "Age Groups",
            showgrid   = FALSE,
            fixedrange = TRUE
          ),
          yaxis = list(
            title      = ~age_y_label(),
            showgrid   = FALSE,
            fixedrange = TRUE
          ),
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          font = list(
            color = getCurrentOutputInfo()$fg()
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          )
        ) %>% config(
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      }) %>% bindCache(
        getCurrentOutputInfo()$fg(),
        inputs$date(),
        inputs$mode(),
        inputs$adj()
      )
    }
  )
}

sexServer <- function(id, covid.sex, inputs, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      # Fancy Num Setup
      fancy_num <- function(x) {
        x %>%
          round() %>%
          format(
            big.mark   = ",",
            scientific = FALSE,
            trim       = TRUE
          )
      }
      
      # Sex Data Setup
      pop.sex <- pop %>%
        group_by(sex) %>%
        summarise(pop = sum(pop))
      
      covid.sex <- covid.sex %>%
        inner_join(pop.sex, by = "sex") %>%
        mutate(
          cases.adj  = cases  * 100000 / pop,
          hospts.adj = hospts * 100000 / pop,
          deaths.adj = deaths * 100000 / pop
        )
      
      rm(pop.sex)
      
      # Sex Variable Creation
      date_sel <- reactive({
        req(inputs$date())
        
        covid.sex %>% 
          filter(date == inputs$date())
      })
      
      target <- reactive({
        date_sel() %>%
          select(
            ends_with(paste0(
              inputs$mode(),
              if(inputs$adj()) {
                ".adj"
              }
            ))
          ) %>%
          .[[1]]
      })
      
      # Sex Plot Customization
      sex_title <- reactive({
        if(inputs$adj()) {
          switch(
            inputs$mode(),
            cases  = "Cases by Sex\n(Population Adjusted)",
            hospts = "Hospitalizations by Sex\n(Population Adjusted)",
            deaths = "Deaths by Sex\n(Population Adjusted)"
          )
        }
        else {
          switch(
            inputs$mode(),
            cases  = "Cases by Sex\n(Total)",
            hospts = "Hospitalizations by Sex\n(Total)",
            deaths = "Deaths by Sex\n(Total)"
          )
        }
      })
      
      sex_y_label <- reactive({
        if(inputs$adj()) {
          switch(
            inputs$mode(),
            cases  = "Cases per 100k",
            hospts = "Hospitalizations per 100k",
            deaths = "Deaths per 100k")
        }
        else {
          switch(
            inputs$mode(),
            cases = "Cases",
            hospts = "Hospitalizations",
            deaths = "Deaths")
        }
      })
      
      sex_tooltips <- reactive({
        if(inputs$adj()) {
          paste0(
            "Sex :", date_sel()$sex, "\n",
            "Date: ", format(date_sel()$date, "%D"), "\n\n",
            "Cases per 100k: ", fancy_num(date_sel()$cases.adj), "\n",
            "Hospitalizations per 100k: ", fancy_num(date_sel()$hospts.adj), "\n",
            "Deaths per 100k: ", fancy_num(date_sel()$deaths.adj), "\n"
          )
        }
        else {
          paste0(
            "Sex :", date_sel()$sex, "\n",
            "Date: ", format(date_sel()$date, "%D"), "\n\n",
            "Total Cases: ", fancy_num(date_sel()$cases), "\n",
            "Total Hospitalizations: ", fancy_num(date_sel()$hospts), "\n",
            "Total Deaths: ", fancy_num(date_sel()$deaths), "\n"
          )
        }
      })
      
      # Sex Plotly
      output$sex <- renderPlotly({
        plot_ly(
          x = ~date_sel()$sex,
          y = ~target(),
          type = "bar",
          marker = list(
            color = c(
              'rgb(237, 119, 215)',
              'rgb(6, 98, 204)',
              'rgb(107, 107, 107)'
            ),
            line = list(
              color = c(
                'rgb(168, 50, 146)',
                'rgb(18, 69, 128)',
                'rgb(54, 54, 54)'
              ),
              width = 1
            )
          ),
          hoverinfo = 'text',
          text      = ~sex_tooltips()
        ) %>% layout(
          title = ~sex_title(),
          yaxis = list(
            title      = "Gender",
            showgrid   = FALSE,
            fixedrange = TRUE
          ),
          xaxis = list(
            title      = ~sex_y_label(),
            showgrid   = FALSE,
            fixedrange = TRUE
          ),
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          font = list(
            color = getCurrentOutputInfo()$fg()
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          )
        ) %>% config(
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      }) %>% bindCache(
        getCurrentOutputInfo()$fg(),
        inputs$date(),
        inputs$mode(),
        inputs$adj()
      )
    }
  )
}

raceServer <- function(id, covid.race, inputs, pop) {
  moduleServer(
    id,
    function(input, output, session) {
      fancy_num <- function(x) {
        x %>%
          round() %>%
          format(
            big.mark   = ",",
            scientific = FALSE,
            trim       = TRUE
          )
      }
      
      # Race Data Setup
      pop.race <- pop %>%
        group_by(race) %>%
        summarise(pop = sum(pop))
      
      covid.race <- covid.race %>%
        inner_join(pop.race, by = "race") %>%
        mutate(
          cases.adj  = cases  * 100000 / pop,
          hospts.adj = hospts * 100000 / pop,
          deaths.adj = deaths * 100000 / pop
        )
      
      rm(pop.race)
      
      # Race Variable Creation
      date_sel <- reactive({
        req(inputs$date())
        
        covid.race %>% 
          filter(date == inputs$date())
      })
      
      target <- reactive({
        date_sel() %>%
          select(
            ends_with(paste0(
              inputs$mode(),
              if(inputs$adj()) {
                ".adj"
              }
            ))
          ) %>%
          .[[1]]
      })
      
      # Race Plot Customization
      race_title <- reactive({
        if(inputs$adj()) {
          switch(
            inputs$mode(),
            cases  = "Cases by Race\n(Population Adjusted)\n",
            hospts = "Hospitalizations by Race\n(Population Adjusted)\n",
            deaths = "Deaths by Race\n(Population Adjusted)\n"
          )
        }
        else {
          switch(
            inputs$mode(),
            cases  = "Cases by Race\n(Total)\n",
            hospts = "Hospitalizations by Race\n(Total)\n",
            deaths = "Deaths by Race\n(Total)\n"
          )
        }
      })
      
      race_tooltips <- reactive({
        if(inputs$adj()) {
          paste0(
            "Race: ", date_sel()$race, "\n",
            "Date: ", format(date_sel()$date, "%D"), "\n\n",
            "Cases per 100k: ", fancy_num(date_sel()$cases.adj), "\n",
            "Hospitalizations per 100k: ", fancy_num(date_sel()$hospts.adj), "\n",
            "Deaths per 100k: ", fancy_num(date_sel()$deaths.adj), "\n"
          )
        }
        else {
          paste0(
            "Race: ", date_sel()$race, "\n",
            "Date: ", format(date_sel()$date, "%D"), "\n\n",
            "Total Cases: ", fancy_num(date_sel()$cases), "\n",
            "Total Hospitalizations: ", fancy_num(date_sel()$hospts), "\n",
            "Total Deaths: ", fancy_num(date_sel()$deaths), "\n"
          )
        }
      })
      
      # Race Plotly
      output$race <- renderPlotly({
        plot_ly(
          covid.race,
          labels = ~date_sel()$race,
          values = ~target(),
          type     = "pie",
          textinfo = 'percent',
          marker   = list(
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
            line = list(
              color = 'rgb(255, 255, 255)', 
              width = 1
            )
          ),
          sort         = T,
          hoverinfo    = 'text',
          textposition = 'inside',
          text         = ~race_tooltips(),
          showlegend = T
        ) %>% layout(
          title = ~race_title(),
          legend = list(
            x           = 0,
            y           = -100,
            orientation = 'h'
          ),
          margin = list(t = 75),
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          font = list(
            color = getCurrentOutputInfo()$fg()
          ),
          hoverlabel = list(
            bordercolor = getCurrentOutputInfo()$fg(),
            bgcolor     = getCurrentOutputInfo()$bg()
          )
        ) %>% config(
          displayModeBar = FALSE,
          displaylogo    = FALSE,
          showTips       = FALSE
        )
      }) %>% bindCache(
        getCurrentOutputInfo()$fg(),
        inputs$date(),
        inputs$mode(),
        inputs$adj()
      )
    }
  )
}

demographicsServer <- function(id, covid.age, covid.race, covid.sex, pop) {
  moduleServer (
    id,
    function(input, output, session) {
      min_date <- max(
        min(covid.age$date),
        min(covid.sex$date),
        min(covid.race$date)
      )
      max_date <- min(
        max(covid.age$date),
        max(covid.sex$date),
        max(covid.race$date)
      )
      
      inputs <- inputServer("input", min_date, max_date)
      ageServer("age", covid.age, inputs, pop)
      sexServer("sex", covid.sex, inputs, pop)
      raceServer("race", covid.race, inputs, pop)
    }
  )
}
