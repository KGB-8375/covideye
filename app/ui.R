## Main client-side app

# Fancier navbarPage
navbarPageWithInputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form", inputs)
    navbar[[4]][[1]]$children[[1]] <- tagAppendChild(
        navbar[[4]][[1]]$children[[1]], form
    )
    navbar
}

# Main UI for website
tagList(
    useShinyjs(),
    extendShinyjs(script = "navigate.js", functions = c("updateHistory")),
    tags$head(tags$script(src = "matomo.js")),
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    
    navbarPageWithInputs(
        title       = div(img(src="favicon.png", height = "30", style = "margin-right:5px"), "CovidEye"),
        windowTitle = "CovidEye",
        theme       = light,
        id          = "navbar",
        # Dashboard
        tabPanel(
            title = "Dashboard",
            value = "dashboard",
            dashboardUI("dashboard")
        ),
        # By County
        # tabPanel(
        #     title = "By County",
        #     value = "by-county",
        #     byCountyUI("byCounty")
        # ),
        # Demographics
        tabPanel(
            title = "Demographics",
            value = "demographics",
            demographicsUI("demographics")
        ),
        # About
        tabPanel(
            title = "About",
            value = "about",
            includeMarkdown("about.md")
        ),
        # Privacy Policy
        tabPanel(
            title = "Privacy Policy",
            value = "privacy-policy",
            includeMarkdown("privacy_policy.md")
        ),
        # Dark Mode
        inputs = tagList(
            switchInput(
                "dark_mode",
                size = "small",
                onLabel = '<i class="fa fa-moon"></i>',
                offLabel = '<i class="fa fa-sun"></i>',
                offStatus = "warning",
                onStatus = "info",
                handleWidth = "20px"
            ),
            tags$style(".bootstrap-switch-id-dark_mode {position: absolute; right: 20px; top: 12px; }")
        ),
        # Disclaimer
        footer = tagList(
            hr(),
            wellPanel(includeMarkdown("disclaimer.md"), style = "font-size:8pt")
        )
    )
)
