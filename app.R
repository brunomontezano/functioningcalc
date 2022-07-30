library(fresh)

mytheme <- create_theme(
    adminlte_color(
        light_blue = "#434C5E"
    ),
    adminlte_sidebar(
        width = "400px",
        dark_bg = "#D8DEE9",
        dark_hover_bg = "#81A1C1",
        dark_color = "#2E3440"
    ),
    adminlte_global(
        content_bg = "#FFF",
        box_bg = "#D8DEE9", 
        info_box_bg = "#D8DEE9"
    )
)

library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)
library(dash)

model <- readRDS("data/model.rds")

ui <- dashboardPage(
    dashboardHeader(title = "Functional Impairment Risk Calculator"),
    
    dashboardSidebar(
        menuItem(
            "Functional Impairment",
            tabName = "func_tab"
        ),
        disable = TRUE
    ),
    dashboardBody(
        use_theme(mytheme),
        tags$head(tags$style(HTML(
            '.abovetext { 
                line-height: 20px;
                text-align: center;
                font-family: "Arial";
                padding: 0 15px;
                color: white;
                font-size: 1em;
                    }
                @media (min-width: 1200px) {
                  .myClass {
                    line-height: 20px;
                    text-align: center;
                    font-family: "Arial";
                    padding: 0 15px;
                    color: white;
                    font-size: x-large
                  }
                }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="abovetext">Authors: Kyara Rodrigues Aguiar, Bruno Braga Montezano, Jacson Gabriel Feiten, Karen Jansen, Ives Cavalcante Passos.<br>&emsp;This application should only be used for educational and research purposes.</span>\');
      })
     ')),
        tabItem(
            tabName = "func_tab",
            box(valueBoxOutput("func_prediction")),
            box(selectInput("abep3_t1",
                            label = "Socioeconomic Status",
                            choices = list("Upper" = "X1", "Middle" = "X2", "Lower" = "X3"))),
            box(selectInput("estano_t1",
                            label = "Currently studying",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("religdic_t1",
                            label = "Has a religion",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("escol_t1",
                            label = "Education",
                            choices = list("Incomplete high school" = "X1",
                                           "Complete high school" = "X2",
                                           "Complete higher education" = "X3"))),
            box(selectInput("pais_doencapsi",
                            label = "Parental psychiatric disease",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("medpsi",
                            label = "Psychiatric medication",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("internacao_vida",
                            label = "Lifetime psychiatric hospitalization",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("trabatu_t1",
                            label = "Currently working",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("trat_t1",
                            label = "Psychological or psychiatric treatment",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("interr_t1",
                            label = "Interrupted treatment before completion",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("pais_faleceu",
                            label = "Parents passed away",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("aldtenta_t1",
                            label = "Knows someone who tried suicide",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("distat_t1",
                            label = "Current dysthymia",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("agoraat_t1",
                            label = "Current agoraphobia",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("fobsoa_t1",
                            label = "Current social anxiety disorder",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("panico_lifetime",
                            label = "Lifetime panic disorder",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(selectInput("srq3_t1",
                            label = "SRQ Item 3",
                            choices = list("No" = "X0", "Yes" = "X1"))),
            box(sliderInput("somabdi_t1",
                            label = "Beck Depression Inventory score",
                            min = 0, max = 100, value = 10)),
            box(sliderInput("somasrq_t1",
                            label = "Self-Regulation Questionnaire score",
                            min = 0, max = 100, value = 10)),
            box(sliderInput("abuso_emocional",
                            label = "Emotional abuse CTQ domain score",
                            min = 0, max = 100, value = 10)),
            box(sliderInput("abuso_fisico",
                            label = "Physical abuse CTQ domain score",
                            min = 0, max = 100, value = 10)),
            box(sliderInput("abuso_sexual",
                            label = "Sexual abuse CTQ domain score",
                            min = 0, max = 100, value = 10)),
            box(sliderInput("neg_emocional",
                            label = "Emotional neglect CTQ domain score",
                            min = 0, max = 100, value = 10)),
            box(sliderInput("neg_fisica",
                            label = "Physical neglect CTQ domain score",
                            min = 0, max = 100, value = 10)),
            box(selectInput("any_ilicit_drug",
                            label = "Any illicit drug use",
                            choices = list("No" = "No", "Yes" = "Yes")))
    )
    )
)

server <- function(input, output) { 
    
    output$func_prediction <- renderValueBox({
        
        prediction <- predict(
            model,
            tibble("abep3_t1" = input$abep3_t1,
            "estano_t1" = input$estano_t1,
            "religdic_t1" = input$religdic_t1,
            "escol_t1" = input$escol_t1,
            "pais_doencapsi" = input$pais_doencapsi,
            "medpsi" = input$medpsi,
            "internacao_vida" = input$internacao_vida,
            "trabatu_t1" = input$trabatu_t1,     
            "trat_t1" = input$trat_t1,
            "interr_t1" = input$interr_t1,
            "pais_faleceu" = input$pais_faleceu,
            "aldtenta_t1" = input$aldtenta_t1,    
            "distat_t1" = input$distat_t1,
            "agoraat_t1" = input$agoraat_t1,
            "fobsoa_t1" = input$fobsoa_t1,
            "panico_lifetime" = input$panico_lifetime,
            "srq3_t1" = input$srq3_t1,
            "somabdi_t1" = input$somabdi_t1,
            "somasrq_t1" = input$somasrq_t1,
            "abuso_emocional" = input$abuso_emocional,
            "abuso_fisico" = input$abuso_fisico,
            "abuso_sexual" = input$abuso_sexual,
            "neg_emocional" = input$neg_emocional,
            "neg_fisica" = input$neg_fisica,     
            "any_ilicit_drug" = input$any_ilicit_drug)
        )
        
        prediction_prob <- predict(
            model,
            tibble("abep3_t1" = input$abep3_t1,
            "estano_t1" = input$estano_t1,
            "religdic_t1" = input$religdic_t1,
            "escol_t1" = input$escol_t1,
            "pais_doencapsi" = input$pais_doencapsi,
            "medpsi" = input$medpsi,
            "internacao_vida" = input$internacao_vida,
            "trabatu_t1" = input$trabatu_t1,     
            "trat_t1" = input$trat_t1,
            "interr_t1" = input$interr_t1,
            "pais_faleceu" = input$pais_faleceu,
            "aldtenta_t1" = input$aldtenta_t1,    
            "distat_t1" = input$distat_t1,
            "agoraat_t1" = input$agoraat_t1,
            "fobsoa_t1" = input$fobsoa_t1,
            "panico_lifetime" = input$panico_lifetime,
            "srq3_t1" = input$srq3_t1,
            "somabdi_t1" = input$somabdi_t1,
            "somasrq_t1" = input$somasrq_t1,
            "abuso_emocional" = input$abuso_emocional,
            "abuso_fisico" = input$abuso_fisico,
            "abuso_sexual" = input$abuso_sexual,
            "neg_emocional" = input$neg_emocional,
            "neg_fisica" = input$neg_fisica,     
            "any_ilicit_drug" = input$any_ilicit_drug),
            type = "prob"
        ) %>% 
            gather() %>% 
            arrange(desc(value)) %>% 
            slice(1) %>% 
            select(value)
        
        prediction_color <- if_else(prediction$.pred_class == "Yes", "red", "green")
        
        valueBox(
            value = paste0(round(100*prediction_prob$value, 0), "%"),
            subtitle = paste0("Functional impairment: ", prediction$.pred_class),
            color = prediction_color
        )
        
    })
    
}

shinyApp(ui, server)