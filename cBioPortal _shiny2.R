
# **************************************************************************************
#
# 		NAME:		cBioPortal_shiny2.R
#
#
# 		SUMMARY: 	The script creates the interactive Shiny dashboard ("cBioPortal Data")
#   	  			to explore clinical data for several TCGA studies using 
#   	  			the cBioPortal API. The data is available on https://www.cbioportal.org/datasets.
#
#
#		UI STRUCTURE:
#
#		Sidebar:
#		  • Study selector
#		  • Filters: gender, age, stage, vital status
#		  • Apply filters button
#		Tabs:
#		  • Overview: KPIs (cases, median age, gender %, alive %),
#		    donut + violin/box plots
#		  • Demography: age histogram, gender distribution
#		  • Staging: tumor stage counts, stage × vital status
#		  • Data: filtered cases table + CSV download
#
#
#		SERVER LOGIC:
#
#		- `%||%`: fallback operator (x if not NULL else y).
#		- raw_cases: reactiveVal holding raw data.
#		- filtered: reactiveVal holding filtered data.
#		- Observers:
#		  • Fetch data when study changes
#		  • Build dynamic filter UIs
#		  • Apply filters on button press or new data
#		- Outputs:
#		  • Value boxes (KPI stats)
#		  • Plotly visuals (donut, violin/box, histogram, bar)
#		  • Data table (DT) with CSV download
#
#
#		WORKFLOW:
#
#		1) User selects a study → fetch data via cbio_cases_for_study()
#		2) Filters applied (gender, age, stage, vital)
#		3) Filtered dataset updates dynamically
#		4) KPIs + charts update
#		5) User can view/export filtered case table
#
#		
#		USER-DEFINED FUNCTIONS:
#
# 		- `%||%`: infix helper, returns right-hand value if 
#   	  left-hand side is NULL (simple fallback operator).
# 		- apply_filters(): internal server function to apply 
#   	  gender/age/stage/vital filters to the dataset.
#
#
#		REACTIVE VALUES:
#
# 		- raw_cases: holds raw data from cbio_cases_for_study().
# 		- filtered: holds filtered subset of cases after applying 
#   	  user-selected filters.
#
#
#		BASE R FUNCTIONS:
#
# 		- if, else, return, is.null, is.na, is.finite, length
# 		- paste, paste0, format, round, median, nrow, rownames
# 		- suppressWarnings, character, as.character, as.numeric
# 		- tolower, toupper, NA, TRUE, FALSE
# 		- write.csv
#
#
#		PACKAGE FUNCTIONS:
#
# 		[shiny]
# 		- shinyApp(), reactiveVal(), observeEvent(), renderUI(),
#   	  renderValueBox(), renderPlotly(), renderDT(), 
#   	  downloadHandler(), showNotification(), withProgress(),
#   	  incProgress(), icon(), helpText(), selectInput(), 
#   	  sliderInput(), checkboxGroupInput(), actionButton()
#
# 		[shinydashboard]
# 		- dashboardPage(), dashboardHeader(), dashboardSidebar(),
#   	  dashboardBody(), sidebarMenu(), menuItem(), tabItems(),
#   	  tabItem(), valueBoxOutput(), valueBox(), box()
#
# 		[dplyr]
# 		- mutate(), filter(), count(), case_when(), if_else(),
#   	  between(), distinct(), transmute(), group_by(), ungroup(),
#   	  slice_head(), pull()
#
# 		[tidyr]
# 		- replace_na()
#
# 		[purrr]
# 		- discard()
#
# 		[stringr]
# 		- str_to_title()
#
# 		[DT]
# 		- DTOutput(), datatable()
#
# 		[plotly]
# 		- plotlyOutput(), renderPlotly(), plot_ly(), layout(),
#   	  ggplotly()
#
# 		[ggplot2]  (implicitly required for visuals)
# 		- ggplot(), aes(), geom_violin(), geom_boxplot(),
#   	  geom_histogram(), geom_col(), coord_flip(), labs()
#
#
#		REQUIRED LIBRARIES:
#
# 		- shiny
# 		- shinydashboard
# 		- dplyr
# 		- tidyr
# 		- purrr
# 		- stringr
# 		- DT
# 		- plotly
# 		- ggplot2
#
#		DEPENDENCIES:
#
# 		- helpers.R (defines DEFAULT_STUDY_ID and 
#   	  cbio_cases_for_study()) must be in the same folder.
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DT)
library(plotly)

source("helpers.R")  # uses DEFAULT_STUDY_ID and cbio_cases_for_study()

# ---- cBioPortal studies ----
study_choices <- c(
  "BRCA (Breast)"        = "brca_tcga",
  "LUAD (Lung Adeno)"    = "luad_tcga",
  "LUSC (Lung Squamous)" = "lusc_tcga",
  "COADREAD (Colon/Rect)"= "coadread_tcga",
  "HNSC (Head & Neck)"   = "hnsc_tcga",
  "SKCM (Melanoma)"      = "skcm_tcga",
  "PAAD (Pancreas)"      = "paad_tcga"
)
default_choice <- if (DEFAULT_STUDY_ID %in% unname(study_choices)) DEFAULT_STUDY_ID else "brca_tcga"

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = span("cBioPortal Data", style="font-weight:600")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",   tabName = "overview", icon = icon("dashboard")),
      menuItem("Demography", tabName = "demo",     icon = icon("people-group")),
      menuItem("Staging",    tabName = "staging",  icon = icon("diagram-project")),
      menuItem("Data",       tabName = "data",     icon = icon("table"))
    ),
    hr(),
    selectInput("study_id", "Study", choices = study_choices, selected = default_choice),
    br(),
    box(
      title = tagList(icon("filter"), "Filters"),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE,
      uiOutput("ui_gender"),
      uiOutput("ui_age"),
      uiOutput("ui_stage"),
      checkboxGroupInput("vital", "Vital status",
                         choices = c("Alive","Dead","Unknown"),
                         selected = c("Alive","Dead")),
      actionButton("apply_filters", "Apply filters", icon = icon("wand-magic-sparkles"))
    ),
    hr(),
    helpText(HTML("Source: <b>cBioPortal</b> • Visuals: Plotly"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-note { font-size: 12px; color: #888; margin-top: -8px; }
      .box-title { font-weight: 600; }
    "))),
    tabItems(
      # -------------------- OVERVIEW --------------------
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_cases", width = 3),
          valueBoxOutput("vb_median_age", width = 3),
          valueBoxOutput("vb_gender_f", width = 3),
          valueBoxOutput("vb_alive", width = 3)
        ),
        fluidRow(
          box(width = 5, title = "Vital status (donut)", status = "primary", solidHeader = TRUE,
              plotlyOutput("vital_donut", height = 320)),
          box(width = 5, title = "Age distribution by gender (violin + box)", status = "info", solidHeader = TRUE,
              plotlyOutput("age_violin", height = 340))
        )
      ),
      # -------------------- DEMOGRAPHY --------------------
      tabItem(tabName = "demo",
        fluidRow(
          box(width = 6, title = "Age at diagnosis (years)", status = "primary", solidHeader = TRUE,
              plotlyOutput("age_hist", height = 320)),
          box(width = 6, title = "Gender distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("gender_bar", height = 320))
        )
      ),
      # -------------------- STAGING --------------------
      tabItem(tabName = "staging",
        fluidRow(
          box(width = 6, title = "Tumor stage (top categories)", status = "info", solidHeader = TRUE,
              plotlyOutput("stage_bar", height = 320)),
          box(width = 6, title = "Stage vs Vital status (proportions)", status = "info", solidHeader = TRUE,
              plotlyOutput("stage_vital", height = 320))
        )
      ),
      # -------------------- DATA --------------------
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, title = "Filtered cases", status = "warning", solidHeader = TRUE,
              downloadButton("dl_csv", "Download CSV", class = "btn btn-success"),
              div(class="small-note","Tip: Download reflects current filters."),
              br(), br(),
              DTOutput("cases_tbl"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  `%||%` <- function(x, y) if (is.null(x)) y else x

  # 1) Load study data on selection
  raw_cases <- reactiveVal(empty_cases())
  observeEvent(input$study_id, {
    withProgress(message = paste("Fetching clinical for", input$study_id, "..."), value = 0.2, {
      df <- cbio_cases_for_study(input$study_id)
      raw_cases(df)
      incProgress(0.8)
      if (nrow(df) == 0) showNotification("No clinical rows returned for this study.", type = "warning")
    })
  }, ignoreInit = FALSE)

  # 2) Dynamic filter UIs based on raw data
  output$ui_gender <- renderUI({
    df <- raw_cases()
    lev <- df$gender %>% as.character() %>% tolower() %>% unique() %>% discard(is.na) %>% sort()
    lev <- stringr::str_to_title(lev)
    if (length(lev) == 0) lev <- c("Female","Male")
    checkboxGroupInput("gender", "Gender", choices = lev, selected = lev)
  })

  output$ui_age <- renderUI({
    df <- raw_cases()
    rng <- suppressWarnings(range(df$age_years, na.rm = TRUE))
    if (!is.finite(rng[1])) rng <- c(0, 100)
    sliderInput("age", "Age range (years)", min = floor(rng[1]), max = ceiling(rng[2]),
                value = c(floor(rng[1]), ceiling(rng[2])), step = 1)
  })

  output$ui_stage <- renderUI({
    df <- raw_cases()
    st <- df$tumor_stage %>% as.character() %>% replace_na("(Unknown)")
    top <- st %>% as_tibble() %>% count(value, sort = TRUE) %>% slice_head(n = 15) %>% pull(value)
    selectInput("stage", "Tumor stage (multi-select)",
                choices = unique(top), selected = unique(top),
                multiple = TRUE, selectize = TRUE)
  })

  # 3) Apply filters
  filtered <- reactiveVal(empty_cases())
  apply_filters <- function() {
    df <- raw_cases()
    if (nrow(df) == 0) { filtered(df); return(invisible(NULL)) }

    g_sel <- input$gender %||% character(0)
    s_sel <- input$stage  %||% character(0)
    v_sel <- (input$vital %||% character(0)) %>% tolower()

    df2 <- df %>%
      mutate(
        gender_label = str_to_title(as.character(gender)),
        stage_label  = replace_na(as.character(tumor_stage), "(Unknown)"),
        vital_label  = case_when(
          tolower(vital_status) %in% c("alive","living","0:living") ~ "alive",
          tolower(vital_status) %in% c("dead","deceased","1:deceased") ~ "dead",
          TRUE ~ "unknown"
        )
      ) %>%
      filter(
        if (length(g_sel)) gender_label %in% g_sel else TRUE,
        if (length(s_sel)) stage_label %in% s_sel else TRUE,
        if (length(v_sel)) vital_label %in% v_sel else TRUE,
        if (!is.null(input$age)) is.na(age_years) | dplyr::between(age_years, input$age[1], input$age[2]) else TRUE
      )
    filtered(df2)
  }
  observeEvent(input$apply_filters, apply_filters(), ignoreInit = TRUE)
  observeEvent(raw_cases(), apply_filters(), ignoreInit = FALSE)

  # 4) KPIs
  output$vb_cases <- renderValueBox({
    valueBox(value = format(nrow(filtered()), big.mark = ","), subtitle = "Cases (filtered)",
             icon = icon("list"), color = "teal")
  })
  output$vb_median_age <- renderValueBox({
    med <- suppressWarnings(median(filtered()$age_years, na.rm = TRUE))
    valueBox(value = ifelse(is.finite(med), paste0(med, " y"), "NA"),
             subtitle = "Median age at diagnosis", icon = icon("user-clock"), color = "purple")
  })
  output$vb_gender_f <- renderValueBox({
    p <- mean(tolower(filtered()$gender) == "female", na.rm = TRUE)
    valueBox(value = ifelse(is.finite(p), paste0(round(100*p), "%"), "NA"),
             subtitle = "Female proportion", icon = icon("venus"), color = "fuchsia")
  })
  output$vb_alive <- renderValueBox({
    p <- mean(tolower(filtered()$vital_status) %in% c("alive","living","0:living"), na.rm = TRUE)
    valueBox(value = ifelse(is.finite(p), paste0(round(100*p), "%"), "NA"),
             subtitle = "Alive at last follow-up", icon = icon("heart"), color = "olive")
  })

  # 5) Overview visuals
  output$vital_donut <- renderPlotly({
    df <- filtered() %>%
      mutate(vital = case_when(
        tolower(vital_status) %in% c("alive","living","0:living") ~ "Alive",
        tolower(vital_status) %in% c("dead","deceased","1:deceased") ~ "Dead",
        TRUE ~ "Unknown"
      )) %>%
      count(vital)
    plot_ly(df, labels = ~vital, values = ~n, type = "pie", hole = 0.55) |>
      layout(showlegend = TRUE)
  })

  output$age_violin <- renderPlotly({
    df <- filtered() %>%
      filter(!is.na(age_years)) %>%
      mutate(gender_label = str_to_title(as.character(gender)))
    p <- ggplot(df, aes(x = gender_label, y = age_years, fill = gender_label)) +
      geom_violin(trim = TRUE, alpha = 0.8) +
      geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.6) +
      labs(x = "Gender", y = "Age (years)")
    ggplotly(p)
  })

  # 6) Graphics
  output$age_hist <- renderPlotly({
    df <- filtered() %>% filter(!is.na(age_years))
    p <- ggplot(df, aes(x = age_years)) +
      geom_histogram(binwidth = 5, fill = "steelblue") +
      labs(x = "Age (years)", y = "Cases")
    ggplotly(p)
  })

  output$gender_bar <- renderPlotly({
    df <- filtered() %>%
      mutate(gender = if_else(is.na(gender) | gender == "", "(Unknown)", str_to_title(gender))) %>%
      count(gender)
    p <- ggplot(df, aes(x = reorder(gender, n), y = n)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(x = "Gender", y = "Cases")
    ggplotly(p)
  })

  output$stage_bar <- renderPlotly({
    df <- filtered() %>%
      mutate(stage = if_else(is.na(tumor_stage) | tumor_stage == "", "(Unknown)", as.character(tumor_stage))) %>%
      count(stage, sort = TRUE) %>% slice_head(n = 15)
    p <- ggplot(df, aes(x = reorder(stage, n), y = n)) +
      geom_col(fill = "seagreen") +
      coord_flip() +
      labs(x = "Tumor stage", y = "Cases")
    ggplotly(p)
  })

  output$stage_vital <- renderPlotly({
    df <- filtered() %>%
      mutate(
        stage = if_else(is.na(tumor_stage) | tumor_stage == "", "(Unknown)", as.character(tumor_stage)),
        vital = case_when(
          tolower(vital_status) %in% c("alive","living","0:living") ~ "Alive",
          tolower(vital_status) %in% c("dead","deceased","1:deceased") ~ "Dead",
          TRUE ~ "Unknown"
        )
      ) %>% count(stage, vital) %>%
      group_by(stage) %>% mutate(prop = n / sum(n)) %>% ungroup()
    p <- ggplot(df, aes(x = stage, y = prop, fill = vital)) +
      geom_col(position = "fill") +
      coord_flip() +
      labs(x = "Tumor stage", y = "Proportion", fill = "Vital")
    ggplotly(p)
  })

  # 7) Data table + download option
  output$cases_tbl <- renderDT({
    filtered() %>%
      transmute(
        CaseID = case_id,
        Gender = str_to_title(gender),
        `Vital status` = str_to_title(vital_status),
        `Age (y)` = age_years,
        `Tumor stage` = tumor_stage,
        `Primary diagnosis` = primary_diagnosis,
        `Primary site` = primary_site,
        `Disease type` = disease_type,
        Project = project_id
      ) %>%
      datatable(rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0(input$study_id, "_filtered_cases.csv"),
    content = function(file) {
      out <- filtered() %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
