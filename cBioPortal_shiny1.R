# **************************************************************************************
#
# 		NAME:		cBioPortal_shiny1.R
#
#
# 		SUMMARY: 	The script creates the interactive Shiny dashboard ("cBioPortal Data - Single Study")
#   	  			to explore clinical data for a single TCGA study using 
#   	  			the cBioPortal API. The data is available on https://www.cbioportal.org/datasets.
#
#
#		UI STRUCTURE:
#
#		Sidebar:
#		  • Static menu ("Browse")
#		  • Displays selected study ID
#		  • Source info
#
#		Tab:
#		  • Browse:
#		    - KPIs (cases, median age, gender %, alive %)
#		    - Age histogram, gender distribution
#		    - Tumor stage and primary diagnosis bars
#		    - Sample cases table
#
#
#		SERVER LOGIC:
#
#		- cases: reactiveVal holding study cases.
#		- On startup:
#		  • Fetches data for DEFAULT_STUDY_ID using cbio_cases_for_study()
#		  • Stores in cases()
#		  • Shows warning if no data returned
#		- Outputs:
#		  • Value boxes (KPIs)
#		  • Plotly visuals (histogram, bar plots)
#		  • Data table (DT) with clinical case attributes
#
#
#		WORKFLOW:
#
#		1) App launches and fetches clinical data for DEFAULT_STUDY_ID.
#		2) Data stored in cases() reactiveVal.
#		3) KPIs + plots + table rendered from this dataset.
#		4) User can browse but not filter or export data.
#
#
#		USER-DEFINED FUNCTIONS:
#
# 		- empty_cases(): returns empty data frame (defined in helpers.R).
# 		- cbio_cases_for_study(): fetches clinical data for study (helpers.R).
# 		- fmt_pct0(): formats percentages (helpers.R).
#
#
#		REACTIVE VALUES:
#
# 		- cases: holds complete dataset for DEFAULT_STUDY_ID.
#
#
#		BASE R FUNCTIONS:
#
# 		- if, else, return, is.null, is.na, is.finite
# 		- paste, paste0, format, round, median, nrow, rownames
# 		- suppressWarnings, as.character, tolower, NA, TRUE, FALSE
#
#
#		PACKAGE FUNCTIONS:
#
# 		[shiny]
# 		- shinyApp(), reactiveVal(), observe(), renderValueBox(),
#   	  renderPlotly(), renderDT(), withProgress(),
#   	  incProgress(), showNotification(), icon(), helpText()
#
# 		[shinydashboard]
# 		- dashboardPage(), dashboardHeader(), dashboardSidebar(),
#   	  dashboardBody(), sidebarMenu(), menuItem(), tabItems(),
#   	  tabItem(), valueBoxOutput(), valueBox(), box()
#
# 		[dplyr]
# 		- mutate(), filter(), count(), if_else(), transmute(),
#   	  slice_head()
#
# 		[tidyr]
# 		- replace_na()
#
# 		[purrr]
# 		- map functions (light use if any)
#
# 		[stringr]
# 		- str_to_title()
#
# 		[DT]
# 		- DTOutput(), datatable()
#
# 		[plotly]
# 		- plotlyOutput(), renderPlotly(), ggplotly()
#
# 		[ggplot2]
# 		- ggplot(), aes(), geom_histogram(), geom_col(),
#   	  coord_flip(), labs()
#
#
#		REQUIRED LIBRARIES:
#
# 		- httr
# 		- jsonlite
# 		- rjson
# 		- shiny
# 		- shinydashboard
# 		- dplyr
# 		- tidyr
# 		- purrr
# 		- stringr
# 		- DT
# 		- plotly
# 		- ggplot2
# 		- BiocManager + cBioPortalData
#
#		DEPENDENCIES:
#
# 		- helpers.R (defines DEFAULT_STUDY_ID, empty_cases(), 
#   	  cbio_cases_for_study(), fmt_pct0()) must be in the same folder.




# 1. Install the packages if necessary

# install.packages(c("httr","jsonlite","dplyr","tidyr","purrr","stringr",
#                    "shiny","shinydashboard","DT","plotly"))

# 2. Load the packages

library(httr)
library(jsonlite)
install.packages("rjson")
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("cBioPortalData")

# 3. Read in the helpers file

source("helpers.R")

# 4. Create the ui parameter for the shiny app

ui <- dashboardPage(
  dashboardHeader(title = "cBioPortal Data - Single"),
  dashboardSidebar(
    sidebarMenu(menuItem("Browse", tabName = "browse", icon = icon("flask"))),
    br(),
    helpText(paste("Study:", DEFAULT_STUDY_ID)),
    helpText("Source: cBioPortal clinical data")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "browse",
        fluidRow(
          valueBoxOutput("vb_cases", width = 3),
          valueBoxOutput("vb_median_age", width = 3),
          valueBoxOutput("vb_gender_f", width = 3),
          valueBoxOutput("vb_alive", width = 3)
        ),
        fluidRow(
          box(width = 6, title = "Age at diagnosis (years)", status = "primary", solidHeader = TRUE,
              plotlyOutput("age_hist", height = 300)),
          box(width = 6, title = "Gender distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("gender_bar", height = 300))
        ),
        fluidRow(
          box(width = 6, title = "Tumor stage (top categories)", status = "info", solidHeader = TRUE,
              plotlyOutput("stage_bar", height = 300)),
          box(width = 6, title = "Primary diagnosis (top 10)", status = "info", solidHeader = TRUE,
              plotlyOutput("diag_bar", height = 300))
        ),
        fluidRow(
          box(width = 12, title = "Cases (sample)", status = "warning", solidHeader = TRUE,
              DTOutput("cases_tbl"))
        )
      )
    )
  )
)


# 4. Create the server parameter for the shiny app

server <- function(input, output, session) {

  # Fetch once on startup for the chosen study
  cases <- reactiveVal(empty_cases())
  observe({
    withProgress(message = paste("Fetching clinical for", DEFAULT_STUDY_ID, "..."), value = 0.2, {
      df <- cbio_cases_for_study(DEFAULT_STUDY_ID)
      cases(df)
      incProgress(0.8)
      if (nrow(df) == 0) showNotification("No clinical rows returned.", type = "warning")
    })
  })

  # ---- value boxes ----
  output$vb_cases <- renderValueBox({
    valueBox(value = format(nrow(cases()), big.mark = ","), subtitle = "Cases",
             icon = icon("list"), color = "teal")
  })

  output$vb_median_age <- renderValueBox({
    med <- suppressWarnings(median(cases()$age_years, na.rm = TRUE))
    valueBox(value = ifelse(is.finite(med), paste0(med, " y"), "NA"),
             subtitle = "Median age at diagnosis", icon = icon("user-clock"), color = "purple")
  })

  output$vb_gender_f <- renderValueBox({
    p <- mean(tolower(cases()$gender) == "female", na.rm = TRUE)
    valueBox(value = fmt_pct0(p), subtitle = "Female proportion",
             icon = icon("venus"), color = "fuchsia")
  })

  output$vb_alive <- renderValueBox({
    p <- mean(tolower(cases()$vital_status) %in% c("alive","living","0:living"), na.rm = TRUE)
    valueBox(value = fmt_pct0(p), subtitle = "Alive at last follow-up",
             icon = icon("heart"), color = "olive")
  })

  # ---- plots ----
  output$age_hist <- renderPlotly({
    df <- cases() %>% filter(!is.na(age_years))
    p <- ggplot(df, aes(x = age_years)) +
      geom_histogram(binwidth = 5, fill = "steelblue") +
      labs(x = "Age (years)", y = "Cases", title = "Age at diagnosis (years)")
    ggplotly(p)
  })

  output$gender_bar <- renderPlotly({
    df <- cases() %>%
      mutate(gender = if_else(is.na(gender) | gender == "", "(unknown)", str_to_title(gender))) %>%
      count(gender)
    p <- ggplot(df, aes(x = reorder(gender, n), y = n)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(x = "Gender", y = "Cases", title = "Gender distribution")
    ggplotly(p)
  })

  output$stage_bar <- renderPlotly({
    df <- cases() %>%
      mutate(stage = if_else(is.na(tumor_stage) | tumor_stage == "", "(unknown)", as.character(tumor_stage))) %>%
      count(stage, sort = TRUE) %>%
      slice_head(n = 12)
    p <- ggplot(df, aes(x = reorder(stage, n), y = n)) +
      geom_col(fill = "seagreen") +
      coord_flip() +
      labs(x = "Tumor stage", y = "Cases", title = "Tumor stage (top categories)")
    ggplotly(p)
  })

  output$diag_bar <- renderPlotly({
    df <- cases() %>%
      mutate(pd = if_else(is.na(primary_diagnosis) | primary_diagnosis == "", "(unknown)", as.character(primary_diagnosis))) %>%
      count(pd, sort = TRUE) %>%
      slice_head(n = 10)
    p <- ggplot(df, aes(x = reorder(pd, n), y = n)) +
      geom_col(fill = "mediumpurple") +
      coord_flip() +
      labs(x = "Primary diagnosis", y = "Cases", title = "Primary diagnosis (top 10)")
    ggplotly(p)
  })

  # ---- table ----
  output$cases_tbl <- renderDT({
    cases() %>%
      transmute(
        CaseID = case_id,
        Gender = str_to_title(gender),
        `Vital status` = str_to_title(vital_status),
        `Age (y)` = age_years,
        `Tumor stage` = tumor_stage,
        `Primary diagnosis` = primary_diagnosis,
        `Primary site` = primary_site,
        `Disease type` = disease_type
      ) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
}
# 5. Run the shiny app

shinyApp(ui, server)
