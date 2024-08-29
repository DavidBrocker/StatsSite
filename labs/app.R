
# TODO ----------------------------------------------------------------------------------------

# Add Progress Bar
# Add default questions to non-dynamic lab entries
# Change some aesthetics


# Load Packages -------------------------------------------------------------------------------
library(bslib)
library(shiny)
library(lorem)
library(stringr)
library(shinyWidgets)
library(bsicons)
library(httr)
library(jsonlite)
library(openai)
library(ShinyEditor)
library(shinyscreenshot)

# CSS Elements --------------------------------------------------------------------------------
fsc_green <- "#006456"
fsc_white <- "#FFFFFF"
fsc_yellow <- "#FFCC37"
fsc_blue <- "#003969"

# API Key GPT
api_key <- "no!"

# API Tiny MCE
tiny_api <- "no!"


# Sample Text
samp <- "<h3>Understanding Research Design in Psychology</h3>

<p><strong>Scenario:</strong><br>
Imagine you are a psychologist interested in studying the effect of sleep on memory performance in college students. You design an experiment where one group of students sleeps for 8 hours before taking a memory test, while another group sleeps for only 4 hours before taking the same test. Both groups are tested under the same conditions, and you compare their memory performance scores to see if there is a significant difference between the two groups.
</p>

<p><strong>Question:</strong></p>
<ol>
    <li><strong>Identify the type of research design used in this study (e.g., experimental, quasi-experimental, or observational).</strong></li>
    <li><strong>Explain why this design is appropriate for studying the effect of sleep on memory performance.</strong></li>
    <li><strong>Discuss one potential advantage and one potential limitation of this research design in the context of psychological research.</strong></li>
</ol>"



# Function for Dynamic Prompts ----------------------------------------------------------------
stat_lab <- function(user_interests,core_concept){
  create_chat_completion(
    model = "gpt-4o-mini",
    # System Role
    messages = list(
      list(
        "role" = "system",
        "content" = paste0(
        "You are statGPT, an expert in 
        statistics education. I am working with a 
        student whose interests include",
        user_interests,
        "Create a statistics question that 
        is tailored to these interests and addresses 
        the core concept of",core_concept, 
        "The question should be engaging, 
        relevant to the student’s interests, 
        and clearly demonstrate the application of 
        this core statistical concept. In your response, be direct
        and list out the questions in a numbered list using appropriate HTML <h3> syntax. Do not begin with 'Certainly! and do not
        reiterate the original question. Format your response with HTML elements
        such as <strong> to bold items <ul> to create a list, <em> for emphasis, and $$ and to render LaTeX elements. In your number responses, can you 
        use HTML such as <details> and <summary> to make each point collapsible."
        )
    ),
    # User Input
    list(
      "role" = "user",
      "content" = paste0("I like ",user_interests, "can you create a statistics question that addresses the core concept of", core_concept,"?")
    )
  ),
  openai_api_key = api_key
)
}

labs <- 
  c(
  "Lab 1: Research Design", 
   "Lab 2: Operationalization",
   "Lab 3: Standard Deviations and Z-Scores",
   "Lab 4: Descriptive Statistics",
   "Lab 5: Hypothesis Testing",
   "Lab 6: Correlation",
   "Lab 7: Linear Regression",
   "Lab 8: ANOVA",
   "Lab 9: Follow-up Comparisons in ANOVA",
   "Lab 10: Within-subjects ANOVA",
   "Lab 11: Reliability and Validity",
   "Lab 12: Results Section"
  )

# UI ------------------------------------------------------------------------------------------
ui <- page_sidebar(
  use_editor(tiny_api),
  tags$head(
    tags$style(
      ".callout{
      text-align: center;
      }.hobbies {
      padding: 24px;
      width: 251px;
      }.hob-but.action-button {
      background-color: red;
      } .navbar.navbar-static-top {
    background-color: #006456;
    color: white;}"
    ),
    HTML("<script src='https://cdn.tiny.cloud/1/1sc3mgn4wtesun77kmblhsruja36lrmhsbfrrhzefkqtn5ck/tinymce/7/tinymce.min.js' referrerpolicy='origin'></script>"),
    HTML("<script type=”text/javascript” src=”http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML”></script>"),
  ),
  title = 
    tagList(
      tags$h4("PSY 348: Lab Manual"),
      tags$a(
        bs_icon("book"),
        href = "https://www.crumplab.com/statistics/",
        target = "new",
        style = "color: white;"
        )
      ),
  window_title = "PSY 348: Lab Manual",
  sidebar = 
    sidebar(
      open = TRUE,
      width = 300,
      selectInput("lab", 
                  label = "Select Lab", 
                  choices = labs, 
                  selected = "Lab 1"),
      shinyscreenshot::screenshotButton(
        label = "Save Progress",
        file = "PSY348Lab"),
      accordion(
        id = "cust",
        open = FALSE,
        accordion_panel(
          title = "Customizations",
          icon = bs_icon("palette"),
          # Dynamic Input
            input_switch(
              id = "int",
              label = "Dynamic Questions",
              FALSE),
          splitLayout(
            cellWidths = c("15%","85%"),
            input_dark_mode(mode = "light"),
            "Visuals"
            ),
          )
        )
      ),
  layout_column_wrap(
    card(
      card_header(
        # Lab Name
        uiOutput("lab_chosen"),
        ),
      card_body(
        # Lab Content
        uiOutput("lab_content")
        )
      ),
    editor(
      id = "wysi",
      text = "Write your responses here")
    )
)

server <- function(input, output, session) {
  
    output$lab_chosen <- renderUI({
      
      ch <- input$lab
      
      ch <- str_remove(ch,"(.*)(?>:)")
      
      HTML(
        paste0("<h1 class='display-6'>",
               ch,"</h1>"))
      })
    
    observeEvent(input$int, {
      if(input$int){
        accordion_panel_insert(
          id = "cust",
          target = "int",
          position = "before",
          div(class = "in-group",
              div(class = "hobbies", 
                  style = "display: inline-block; vertical-align: top; width: 80%;",
                  textInput(
                    "hob",
                    label = tooltip(
                      trigger = list(
                        "Hobbies/Interest",
                        bs_icon("info-circle")
                      ),
                      "Response generated with GPT-4o-Mini"
                    )
                  )
              ),
              div(class = "hob_but", 
                  style = "display: inline-block; vertical-align: top; padding-top: 19%;",
                  actionBttn(
                    inputId = "int_but",
                    label = "",
                    icon = bs_icon("robot"),
                    size = "sm"
                  )
              )
          )
        )
      }
    })
    
      output$lab_content <- renderUI({
      
      if(input$int == FALSE) {
        
        lab_c <- observe({input$lab})
          
        HTML(samp)
        
      } else {

        hobby <- 
          eventReactive(
            input$int_but,{
              input$hob}
          )
        # Get Lab Core Concept
        ch <- str_remove(input$lab,"(.*)(?>:)")
        
        # Pass to SGPT
        qu <- stat_lab(hobby(),ch)
        
        # Extract message
        HTML(qu$choices$message.content)
      }
    })
    
}

shinyApp(ui = ui, server = server)