# qPCR分析模块

qpcr_analysis_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    conditionalPanel(
      condition = "output['qpcr_data_available']", ns = ns,
      
      box(
        title = "qPCR分析设置", status = "primary", solidHeader = TRUE,
        width = 4,
        
        h4("参考基因设置"),
        uiOutput(ns("ref_genes_ui")),
        
        h4("目标基因设置"),
        uiOutput(ns("target_genes_ui")),
        
        h4("对照组设置"),
        uiOutput(ns("control_group_ui")),
        
        hr(),
        
        actionButton(ns("run_qpcr_analysis"), "执行qPCR分析",
                     class = "btn-success",
                     icon = icon("flask"),
                     style = "width: 100%;"),
        
        hr(),
        
        h4("分析方法说明"),
        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h5("2^(-ΔΔCt)方法:"),
          p("ΔCt = Ct(目标基因) - Ct(参考基因)"),
          p("ΔΔCt = ΔCt(实验组) - ΔCt(对照组)"),
          p("相对表达量 = 2^(-ΔΔCt)"),
          h5("统计检验:"),
          p("对ΔCt值进行统计检验，比较各组间差异")
        )
      ),
      
      box(
        title = "qPCR分析结果", status = "info", solidHeader = TRUE,
        width = 8,
        
        tabsetPanel(
          tabPanel("分析结果",
                   DT::dataTableOutput(ns("qpcr_sample_details")),
                   br(),
                   h5("计算过程说明:"),
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                     p("1. ΔCt = Ct(目标基因) - Ct(参考基因)"),
                     p("2. ΔΔCt = ΔCt(样本) - ΔCt(对照组均值)"),
                     p("3. 相对表达量 = 2^(-ΔΔCt)")
                   ),
                   br(),
                   DT::dataTableOutput(ns("qpcr_results_table")),
                   br(),
                   h5("组间比较结果说明:"),
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                     p("组间比较结果显示了每个组的均值、标准差、标准误差和95%置信区间")
                   )
          ),
          
          tabPanel("统计检验",
                   verbatimTextOutput(ns("qpcr_test_results"))
          ),
          
          tabPanel("事后比较",
                   verbatimTextOutput(ns("qpcr_posthoc_results"))
          ),
          
          tabPanel("数据下载",
                   downloadButton(ns("download_qpcr_results"), "下载Excel分析结果",
                                  class = "btn-primary"),
                   br(),
                   br(),
                   downloadButton(ns("download_qpcr_pdf"), "下载PDF分析报告",
                                  class = "btn-primary")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "!output['qpcr_data_available']", ns = ns,
      
      box(
        title = "qPCR分析", status = "warning", solidHeader = TRUE,
        width = 12,
        
        div(style = "text-align: center; padding: 50px;",
          h3("请先上传荧光定量数据"),
          p("qPCR分析需要4列数据：样本ID, 基因名称, CT值, 分组"),
          icon("dna", style = "font-size: 48px; color: #3c8dbc;")
        )
      )
    )
  )
}

qpcr_analysis_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间函数
    
    # qPCR数据可用性
    output$qpcr_data_available <- reactive({
      !is.null(values$data) && values$data_type == "qpcr"
    })
    outputOptions(output, "qpcr_data_available", suspendWhenHidden = FALSE)
    
    # 动态生成参考基因选择UI
    output$ref_genes_ui <- renderUI({
      if (!is.null(values$data) && values$data_type == "qpcr") {
        unique_genes <- unique(values$data$Gene_Name)
        
        tagList(
          selectInput(ns("ref_genes"), "选择参考基因:",
                      choices = unique_genes,
                      selected = unique_genes[1],
                      multiple = TRUE,
                      selectize = FALSE),
          
          conditionalPanel(
            condition = "input.ref_genes.length > 1", ns = ns,
            div(style = "background-color: #fcf8e3; padding: 10px; border-radius: 5px; margin-top: 10px;",
              icon("info-circle"), " 多个参考基因将使用平均值进行计算"
            )
          )
        )
      }
    })
    
    # 动态生成目标基因选择UI
    output$target_genes_ui <- renderUI({
      if (!is.null(values$data) && values$data_type == "qpcr") {
        unique_genes <- unique(values$data$Gene_Name)
        ref_genes <- input$ref_genes %||% unique_genes[1]
        target_choices <- setdiff(unique_genes, ref_genes)
        
        tagList(
          selectInput(ns("target_genes"), "选择目标基因:",
                      choices = target_choices,
                      selected = if(length(target_choices) > 0) target_choices[1] else NULL,
                      multiple = TRUE,
                      selectize = FALSE),
          
          conditionalPanel(
            condition = "input.target_genes.length > 1", ns = ns,
            div(style = "background-color: #fcf8e3; padding: 10px; border-radius: 5px; margin-top: 10px;",
              icon("info-circle"), " 多个目标基因将分别进行分析"
            )
          )
        )
      }
    })
    
    # 动态生成对照组选择UI
    output$control_group_ui <- renderUI({
      if (!is.null(values$data) && values$data_type == "qpcr") {
        unique_groups <- unique(values$data$Group)
        
        selectInput(ns("control_group"), "选择对照组:",
                    choices = unique_groups,
                    selected = unique_groups[1])
      }
    })
    
    # 执行qPCR分析
    observeEvent(input$run_qpcr_analysis, {
      if (is.null(values$data) || values$data_type != "qpcr") {
        show_error_notification("请先上传荧光定量数据")
        return()
      }
      
      if (is.null(input$ref_genes) || length(input$ref_genes) == 0) {
        show_error_notification("请选择参考基因")
        return()
      }
      
      if (is.null(input$target_genes) || length(input$target_genes) == 0) {
        show_error_notification("请选择目标基因")
        return()
      }
      
      if (is.null(input$control_group)) {
        show_error_notification("请选择对照组")
        return()
      }
      
      # 显示进度
      withProgress(message = '正在执行qPCR分析...', value = 0, {
        # 执行qPCR分析
        result <- safe_execute(
          analyze_qpcr(
            data = values$data,
            ref_genes = input$ref_genes,
            target_genes = input$target_genes,
            control_group = input$control_group
          ),
          "qPCR分析失败"
        )
        
        if (!result$success) {
          show_error_notification(result$message)
          return()
        }
        
        # 存储结果
        values$qpcr_results <- result$result
        
        # 显示成功消息
        show_success_notification("qPCR分析完成！")
      })
    })
    
    # 显示qPCR结果表格
    output$qpcr_results_table <- renderDT({
      if (!is.null(values$qpcr_results)) {
        datatable(values$qpcr_results$group_results,
                  options = list(pageLength = 10, scrollX = TRUE),
                  caption = "qPCR分析结果（组间比较）") %>%
          formatRound(columns = c("mean_fold_change", "se_fold_change", "lower_ci", "upper_ci"), digits = 4) %>%
          formatStyle('mean_fold_change', 
                     background = styleColorBar(values$qpcr_results$group_results$mean_fold_change, 'lightblue'),
                     backgroundSize = '98% 88%',
                     backgroundRepeat = 'no-repeat',
                     backgroundPosition = 'center')
      }
    })
    
    # 显示每个样本的详细计算过程
    output$qpcr_sample_details <- renderDT({
      if (!is.null(values$qpcr_results)) {
        datatable(values$qpcr_results$sample_results,
                  options = list(pageLength = 20, scrollX = TRUE),
                  caption = "qPCR分析结果（每个样本计算过程）") %>%
          formatRound(columns = c("delta_ct", "delta_delta_ct", "fold_change"), digits = 4) %>%
          formatStyle('fold_change', 
                     background = styleColorBar(values$qpcr_results$sample_results$fold_change, 'lightgreen'),
                     backgroundSize = '98% 88%',
                     backgroundRepeat = 'no-repeat',
                     backgroundPosition = 'center')
      }
    })
    
    # 显示统计检验结果
    output$qpcr_test_results <- renderPrint({
      if (!is.null(values$qpcr_results)) {
        # 如果有多个基因，显示所有基因的统计检验结果
        if (!is.null(values$qpcr_results$test_results) && length(values$qpcr_results$test_results) > 1) {
          for (target_gene in names(values$qpcr_results$test_results)) {
            test_result <- values$qpcr_results$test_results[[target_gene]]$test_result
            test_info <- values$qpcr_results$test_results[[target_gene]]$test_info
            
            cat(paste("基因:", target_gene, "\n"))
            cat("统计检验结果:\n")
            cat("检验方法:", test_result$test, "\n")
            
            if (test_result$test == "Independent t-test") {
              cat("t值:", round(test_result$statistic, 4), "\n")
              cat("自由度:", round(test_result$df, 4), "\n")
              cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
            } else if (test_result$test == "One-way ANOVA") {
              cat("F值:", round(test_result$f_value, 4), "\n")
              cat("自由度:", round(test_result$df, 4), "\n")
              cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
            } else if (test_result$test == "Wilcoxon rank sum test") {
              cat("W值:", round(test_result$statistic, 4), "\n")
              cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
            } else if (test_result$test == "Kruskal-Wallis test") {
              cat("chi-squared值:", round(test_result$statistic, 4), "\n")
              cat("自由度:", round(test_result$df, 4), "\n")
              cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
            }
            
            cat("\n正态性检验结果:\n")
            if (nrow(test_info$normality) > 0) {
              print(test_info$normality)
            } else {
              cat("无法进行正态性检验（样本量不足）\n")
            }
            
            cat("\n方差齐性检验结果:\n")
            cat("检验方法:", test_info$variance$test, "\n")
            cat("统计量:", round(test_info$variance$statistic, 4), "\n")
            cat("p值:", ifelse(test_info$variance$p_value < 0.001, "< 0.001", round(test_info$variance$p_value, 4)), "\n")
            cat("方差齐性:", ifelse(test_info$variance$homogeneous, "是", "否"), "\n")
            
            cat("\n", paste(rep("=", 50), collapse = ""), "\n\n")
          }
        } else {
          # 单个基因或向后兼容的情况
          test_result <- values$qpcr_results$test_result
          test_info <- values$qpcr_results$test_info
          
          cat("统计检验结果:\n")
          cat("检验方法:", test_result$test, "\n")
          
          if (test_result$test == "Independent t-test") {
            cat("t值:", round(test_result$statistic, 4), "\n")
            cat("自由度:", round(test_result$df, 4), "\n")
            cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
          } else if (test_result$test == "One-way ANOVA") {
            cat("F值:", round(test_result$f_value, 4), "\n")
            cat("自由度:", round(test_result$df, 4), "\n")
            cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
          } else if (test_result$test == "Wilcoxon rank sum test") {
            cat("W值:", round(test_result$statistic, 4), "\n")
            cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
          } else if (test_result$test == "Kruskal-Wallis test") {
            cat("chi-squared值:", round(test_result$statistic, 4), "\n")
            cat("自由度:", round(test_result$df, 4), "\n")
            cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
          }
          
          cat("\n正态性检验结果:\n")
          if (nrow(test_info$normality) > 0) {
            print(test_info$normality)
          } else {
            cat("无法进行正态性检验（样本量不足）\n")
          }
          
          cat("\n方差齐性检验结果:\n")
          cat("检验方法:", test_info$variance$test, "\n")
          cat("统计量:", round(test_info$variance$statistic, 4), "\n")
          cat("p值:", ifelse(test_info$variance$p_value < 0.001, "< 0.001", round(test_info$variance$p_value, 4)), "\n")
          cat("方差齐性:", ifelse(test_info$variance$homogeneous, "是", "否"), "\n")
        }
      }
    })
    
    # 显示事后比较结果
    output$qpcr_posthoc_results <- renderPrint({
      if (!is.null(values$qpcr_results)) {
        # 如果有多个基因，显示所有基因的事后比较结果
        if (!is.null(values$qpcr_results$posthoc_results) && length(values$qpcr_results$posthoc_results) > 1) {
          for (target_gene in names(values$qpcr_results$posthoc_results)) {
            posthoc_result <- values$qpcr_results$posthoc_results[[target_gene]]
            
            if (!is.null(posthoc_result)) {
              cat(paste("基因:", target_gene, "\n"))
              cat("事后比较结果:\n")
              cat("方法:", posthoc_result$test, "\n\n")
              
              if (posthoc_result$test == "Tukey HSD") {
                print(posthoc_result$results)
              } else if (posthoc_result$test == "Dunn test with Bonferroni correction") {
                print(posthoc_result$results)
              }
              
              cat("\n", paste(rep("=", 50), collapse = ""), "\n\n")
            }
          }
        } else {
          # 单个基因或向后兼容的情况
          posthoc_result <- values$qpcr_results$posthoc_result
          
          if (!is.null(posthoc_result)) {
            cat("事后比较结果:\n")
            cat("方法:", posthoc_result$test, "\n\n")
            
            if (posthoc_result$test == "Tukey HSD") {
              print(posthoc_result$results)
            } else if (posthoc_result$test == "Dunn test with Bonferroni correction") {
              print(posthoc_result$results)
            }
          }
        }
      }
    })
    
    # 图表功能已移除 - 保留在常规指标分析中
    # 显示qPCR图表
    # output$qpcr_plot_ui <- renderUI({
    #   if (!is.null(values$qpcr_results)) {
    #     tagList(
    #       h4("基因相对表达量图"),
    #       
    #       div(style = "margin-bottom: 15px;",
    #         checkboxInput(ns("show_qpcr_significance"), "显示显著性标注", TRUE),
    #         conditionalPanel(
    #           condition = "input.show_qpcr_significance", ns = ns,
    #           numericInput(ns("qpcr_sig_level"), "显著性水平", 0.05, min = 0.01, max = 0.1, step = 0.01)
    #         ),
    #         checkboxInput(ns("show_interactive"), "启用交互式图表", FALSE),
    #         selectInput(ns("qpcr_plot_type"), "图表类型", 
    #                    choices = list("柱状图" = "bar", "点图" = "point", "小提琴图" = "violin", "柱状图+点图" = "both"),
    #                    selected = "bar"),
    #         checkboxInput(ns("show_ns_markers"), "显示ns标记", TRUE)
    #       ),
    #       
    #       plotOutput(ns("qpcr_plot"), height = "400px"),
    #       
    #       conditionalPanel(
    #         condition = "input.show_interactive", ns = ns,
    #         plotlyOutput(ns("qpcr_interactive_plot"), height = "400px")
    #       )
    #     )
    #   }
    # })
    
    # # 生成qPCR图表
    # output$qpcr_plot <- renderPlot({
    #   if (!is.null(values$qpcr_results)) {
    #     # 准备显著性标注数据
    #     sig_data <- NULL
    #     if (input$show_qpcr_significance && !is.null(values$qpcr_results$posthoc_result)) {
    #       sig_data <- prepare_significance_data(
    #         data = values$qpcr_results$group_results,
    #         test_result = values$qpcr_results$test_result,
    #         posthoc_result = values$qpcr_results$posthoc_result,
    #         x_col = "Group",
    #         y_col = "mean_fold_change",
    #         sig_level = input$qpcr_sig_level %||% 0.05
    #       )
    #     }
    #     
    #     # 创建图表
    #     p <- create_qpcr_plot(
    #       data = values$qpcr_results$group_results,
    #       x_col = "Group",
    #       y_col = "mean_fold_change",
    #       title = paste("基因相对表达量 -", values$qpcr_results$target_genes[1]),
    #       y_label = "相对表达量 (2^(-ΔΔCt))",
    #       show_significance = input$show_qpcr_significance,
    #       sig_data = sig_data,
    #       plot_type = input$qpcr_plot_type,
    #       show_ns = input$show_ns_markers
    #     )
    #     
    #     return(p)
    #   }
    # })
    
    # # 生成交互式qPCR图表
    # output$qpcr_interactive_plot <- renderPlotly({
    #   if (!is.null(values$qpcr_results)) {
    #     # 创建基础图表
    #     p <- create_qpcr_plot(
    #       data = values$qpcr_results$group_results,
    #       x_col = "Group",
    #       y_col = "mean_fold_change",
    #       title = paste("基因相对表达量 -", values$qpcr_results$target_genes[1]),
    #       y_label = "相对表达量 (2^(-ΔΔCt))",
    #       show_significance = FALSE,  # 交互式图表中不显示显著性标注
    #       plot_type = input$qpcr_plot_type,
    #       show_ns = FALSE  # 交互式图表中不显示ns标记
    #     )
    #     
    #     sig_data <- NULL
    #     
    #     # 转换为交互式图表
    #     interactive_p <- create_interactive_plot(
    #       p, 
    #       data = values$qpcr_results$group_results,
    #       x_col = "Group",
    #       y_col = "mean_fold_change"
    #     )
    #     
    #     return(interactive_p)
    #   }
    # })
    
    # 下载qPCR结果
    output$download_qpcr_results <- downloadHandler(
      filename = function() {
        paste0("qPCR分析结果_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (!is.null(values$qpcr_results)) {
          # 创建工作簿
          wb <- createWorkbook()
          
          # 添加每个样本的详细计算过程
          addWorksheet(wb, "样本计算过程")
          writeData(wb, "样本计算过程", values$qpcr_results$sample_results)
          
          # 添加组间比较结果
          addWorksheet(wb, "组间比较结果")
          writeData(wb, "组间比较结果", values$qpcr_results$group_results)
          
          # 添加统计检验结果
          addWorksheet(wb, "统计检验")
          test_data <- data.frame(
            参数 = c("检验方法", "p值", "参考基因", "目标基因", "对照组"),
            值 = c(
              values$qpcr_results$test_result$test,
              ifelse(values$qpcr_results$test_result$p_value < 0.001, "< 0.001", round(values$qpcr_results$test_result$p_value, 4)),
              paste(values$qpcr_results$ref_genes, collapse = ", "),
              paste(values$qpcr_results$target_genes, collapse = ", "),
              values$qpcr_results$control_group
            )
          )
          writeData(wb, "统计检验", test_data)
          
          # 添加事后比较结果
          if (!is.null(values$qpcr_results$posthoc_result)) {
            addWorksheet(wb, "事后比较")
            if (values$qpcr_results$posthoc_result$test == "Tukey HSD") {
              writeData(wb, "事后比较", values$qpcr_results$posthoc_result$results)
            }
          }
          
          # 保存工作簿
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      }
    )
    
    # 下载qPCR图表
    output$download_qpcr_plot <- downloadHandler(
      filename = function() {
        paste0("qPCR图表_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (!is.null(values$qpcr_results)) {
          # 准备显著性标注数据
          sig_data <- NULL
          if (input$show_qpcr_significance && !is.null(values$qpcr_results$posthoc_result)) {
            sig_data <- prepare_significance_data(
              data = values$qpcr_results$delta_ct,
              test_result = values$qpcr_results$test_result,
              posthoc_result = values$qpcr_results$posthoc_result,
              x_col = "Group",
              y_col = "delta_ct",
              sig_level = input$qpcr_sig_level %||% 0.05
            )
          }
          
          # 创建图表
          p <- create_qpcr_plot(
            data = values$qpcr_results$results,
            x_col = "Group",
            y_col = "fold_change",
            title = paste("基因相对表达量 -", values$qpcr_results$target_genes[1]),
            y_label = "相对表达量 (2^(-ΔΔCt))",
            show_significance = input$show_qpcr_significance,
            sig_data = sig_data
          )
          
          # 保存图表
          save_plot(p, file)
        }
      }
    )
    
    # 下载qPCR PDF分析报告
    output$download_qpcr_pdf <- downloadHandler(
      filename = function() {
        paste0("qPCR分析报告_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (!is.null(values$qpcr_results)) {
          # 创建PDF文件，使用cairo_pdf支持Unicode
          cairo_pdf(file, width = 12, height = 15, family = "SimHei")
          
          # 添加标题页
          par(mar = c(0, 0, 0, 0))
          plot.new()
          text(0.5, 0.5, "qPCR分析报告", cex = 3, font = 2)
          text(0.5, 0.4, paste0("生成日期: ", Sys.Date()), cex = 1.5)
          
          # 添加样本计算过程
          plot.new()
          par(mar = c(5, 4, 4, 2) + 0.1)
          text(0.5, 0.95, "样本计算过程", cex = 1.5, font = 2)
          gridExtra::grid.table(values$qpcr_results$sample_results)
          
          # 添加组间比较结果
          plot.new()
          text(0.5, 0.95, "组间比较结果", cex = 1.5, font = 2)
          gridExtra::grid.table(values$qpcr_results$group_results)
          
          # 添加统计检验结果
          plot.new()
          text(0.5, 0.95, "统计检验结果", cex = 1.5, font = 2)
          
          # 准备统计检验数据
          test_data <- data.frame(
            参数 = c("检验方法", "p值", "参考基因", "目标基因", "对照组"),
            值 = c(
              values$qpcr_results$test_result$test,
              ifelse(values$qpcr_results$test_result$p_value < 0.001, "< 0.001", round(values$qpcr_results$test_result$p_value, 4)),
              paste(values$qpcr_results$ref_genes, collapse = ", "),
              paste(values$qpcr_results$target_genes, collapse = ", "),
              values$qpcr_results$control_group
            )
          )
          gridExtra::grid.table(test_data)
          
          # 添加事后比较结果
          if (!is.null(values$qpcr_results$posthoc_result)) {
            plot.new()
            text(0.5, 0.95, "事后比较结果", cex = 1.5, font = 2)
            
            if (values$qpcr_results$posthoc_result$test == "Tukey HSD") {
              gridExtra::grid.table(values$qpcr_results$posthoc_result$results)
            } else if (values$qpcr_results$posthoc_result$test == "Dunn test with Bonferroni correction") {
              gridExtra::grid.table(values$qpcr_results$posthoc_result$results)
            }
          }
          
          # 关闭PDF设备
          dev.off()
        }
      }
    )
  })
}