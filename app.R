library(shiny)
library(bslib)
library(crosstalk)
library(plotly)
library(DT)
library(tidyverse)

color_primary <- "#78c2ad"
# color_primary <- "#2196f3"

ui <- shiny::navbarPage(
    footer = tagList(
        "Jinping Zhao"
    ),
    
    title = '客户生命周期价值',
    theme = bs_theme(version = 4, bootswatch = 'lux'),
    
    tabPanel(
        title = "分析和预测",
        # SIDEBAR ----
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h1("给用户未来的消费评分"),
                p("目的：预测消费者未来90天消费的概率和消费金额"),
                p("根据最终的结果，针对不同用户群体制定方案，最大化收益"),
                br(),
                hr(),
                # h3("数据展示比例"),
                sliderInput("sample_prop",
                            "数据展示比例:",
                            min = 0, max = 1,
                            value = 0.05, step = 0.05),
                hr(),
                
            ),
            mainPanel(
                width = 9,
                
                tabsetPanel(
                    tabPanel(
                        "可视化展示",
                        h1("未来消费概率"),
                        plotlyOutput("probability_plot"),
                        h1("关键指标", tags$small("未来消费")),
                        plotlyOutput("feature_plot")
                    ),
                    
                    tabPanel(
                        "具体数据",
                        h1("客户行为"),
                        p("这些客户将会消费得更多."),
                        dataTableOutput("spend_more_table")
                    )
                    
                )
                
                
                
            )
        )
    )
)

server <- function(input, output, session) {
    
    rv <- reactiveValues()
    
    observe({
        
        # IMPORT PREDICTIONS
        rv$prediction_tbl <- read_rds("artifacts/predictions_all_tbl.rds") %>%
            mutate(
                spend_actual_vs_pred = spend_90_total - (.pred_total + 0.0001)
            ) %>%
            mutate(text = str_glue("
                                   客户ID: {customer_id}
                                   未来90天消费概率预测: {scales::percent(.pred_prob, accuracy = 0.1)}
                                   未来90天消费金额预测: {scales::dollar(.pred_total)}
                                   未来90天实际消费金额: {scales::dollar(spend_90_total)}
                                   实际VS预测: {scales::dollar(spend_actual_vs_pred)}
                                   ---
                                   Recency: {recency}
                                   历史消费总额: {scales::dollar(price_sum)}
                                   平均购买额: {scales::dollar(price_mean)}
                                   "))
        
        set.seed(123)
        rv$prediction_sample <- rv$prediction_tbl %>%
            sample_frac(size = input$sample_prop)
        
        rv$shared_predictions <- SharedData$new(rv$prediction_sample, key = ~customer_id, group = "customer_id")
        
        rv$features_tbl <- rv$prediction_sample %>%
            select(customer_id, .pred_prob, .pred_total, spend_90_total, spend_actual_vs_pred,
                   recency, frequency, price_sum) %>%
            pivot_longer(cols = -c(customer_id, .pred_prob, .pred_total, spend_90_total, spend_actual_vs_pred), names_to = "feature", values_to = "value") %>%
            group_by(feature) %>%
            mutate(value_scaled = scale(value) %>% as.numeric()) %>%
            ungroup() %>%
            mutate(text = str_glue("客户 ID: {customer_id}
                                    未来90天消费概率预测: {scales::percent(.pred_prob, accuracy = 0.1)}
                                    未来90天消费金额预测: {scales::dollar(.pred_total)}
                                    未来90天实际消费金额: {scales::dollar(spend_90_total)}
                                    实际VS预测: {scales::dollar(spend_actual_vs_pred)}
                                    ---
                                    {feature}: {round(value,2)}"))
        
        rv$shared_features <- SharedData$new(rv$features_tbl, key = ~customer_id, group = "customer_id")
        
    })
    
    output$probability_plot <- renderPlotly({
        
        req(rv$prediction_sample)
        
        g <- rv$shared_predictions %>%
            
            ggplot(aes(frequency, .pred_prob, color = spend_actual_vs_pred)) +
            geom_point(aes(text = text), size = 4) +
            geom_smooth(se = F, color = 'black', method = "gam") +
            theme_minimal() +
            scale_color_gradient2(low = "red", mid = color_primary, high = 'black', midpoint = 0,
                                  name = "实际消费Vs预测") +
            labs(x = "购买频次", y = "未来90天购买概率")
        
        ggplotly(g, tooltip = "text") %>%
            highlight()
    })
    
    output$feature_plot <- renderPlotly({
        
        req(rv$prediction_sample)
        
        g <- rv$shared_features %>%
            
            ggplot(aes(feature, value_scaled)) +
            geom_violin() +
            geom_jitter(aes(text = text, color = spend_actual_vs_pred), size = 2, alpha = 0.75) +
            coord_flip() +
            theme_minimal() +
            scale_color_gradient2(low = "red", mid = color_primary, high = 'black', midpoint = 0,
                                  name = "实际消费Vs预测") +
            labs(x = "", y = "未来90天购买概率")
        
        ggplotly(g, tooltip = "text") %>%
            highlight()
    })
    
    output$spend_more_table <- renderDataTable({
        
        req(rv$prediction_tbl)
        
        rv$prediction_tbl %>%
            arrange(desc(.pred_total)) %>%
            filter(spend_90_total < 10) %>%
            filter(.pred_total > 10) %>%
            select(spend_actual_vs_pred, spend_90_total, .pred_total, customer_id, recency, frequency, price_sum, price_mean) %>%
            
            rename(
                "实际VS预测"   = spend_actual_vs_pred,
                "未来90天总消费预测" = .pred_total,
                "未来90天实际消费"  = spend_90_total,
                "客户 ID"                = customer_id,
                "消费总额"       = price_sum,
                "平均消费额"         = price_mean
            ) %>%
            rename_all(
                .funs = ~ str_replace_all(., "_", " ") %>%
                    str_to_title()
                
            ) %>%
            
            DT::datatable(
                extensions = 'Buttons',
                options   = list(
                    scrollX = TRUE,
                    dom     = "tBp",
                    buttons = c('copy', 'csv', 'excel')
                )
            ) %>%
            
            DT::formatCurrency(
                columns = c("实际Vs预测",
                            "未来90天总消费预测", "未来90天实际消费",
                            "消费总额", "平均消费额"),
                digits  = 0
            )
        
    })
    
    
}


shinyApp(ui, server)