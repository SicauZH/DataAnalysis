# 可视化模块

visualization_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    conditionalPanel(
      condition = "output['any_data_available']", ns = ns,
      
      box(
        title = "图表设置", status = "primary", solidHeader = TRUE,
        width = 4,
        
        h4("图表类型"),
        selectInput(ns("plot_type"), "选择图表类型:",
                    choices = list(
                      "柱状图" = "bar",
                      "箱线图" = "box",
                      "小提琴图" = "violin",
                      "点图" = "dot",
                      "密度图" = "density",
                      "QQ图" = "qq"
                    ),
                    selected = "box"),
        
        conditionalPanel(
          condition = "input.plot_type != 'qq'", ns = ns,
          
          h4("颜色设置"),
          selectInput(ns("color_palette"), "选择颜色方案:",
                      choices = list(
                        "Set1" = "Set1",
                        "Set2" = "Set2",
                        "Set3" = "Set3",
                        "Pastel1" = "Pastel1",
                        "Pastel2" = "Pastel2",
                        "Dark2" = "Dark2",
                        "Paired" = "Paired",
                        "viridis" = "viridis",
                        "magma" = "magma",
                        "inferno" = "inferno",
                        "plasma" = "plasma"
                      ),
                      selected = "Set1"),
          
          conditionalPanel(
            condition = "input.plot_type == 'bar'", ns = ns,
            
            h4("柱状图设置"),
            radioButtons(ns("bar_type"), "柱状图类型:",
                         choices = list("均值±标准差" = "mean_sd",
                                        "均值±标准误" = "mean_se",
                                        "中位数±四分位距" = "median_iqr"),
                         selected = "mean_sd")
          ),
          
          h4("显著性标注"),
          checkboxInput(ns("show_significance"), "显示显著性标注", TRUE),
          conditionalPanel(
            condition = "input.show_significance", ns = ns,
            numericInput(ns("sig_level"), "显著性水平", 0.05, min = 0.01, max = 0.1, step = 0.01)
          )
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'qq'", ns = ns,
          
          h4("QQ图设置"),
          selectInput(ns("qq_group"), "选择分组显示QQ图:",
                      choices = c("所有组合并" = "all"),
                      selected = "all")
        ),
        
        conditionalPanel(
        condition = "output.plot_type != 'qq'", ns = ns,
        h4("交互式图表"),
        checkboxInput(ns("show_interactive"), "启用交互式图表", TRUE)
      ),
        
        h4("图表标题和标签"),
        textInput(ns("plot_title"), "图表标题:", ""),
        textInput(ns("x_label"), "X轴标签:", ""),
        textInput(ns("y_label"), "Y轴标签:", ""),
        
        actionButton(ns("generate_plot"), "生成图表",
                     class = "btn-primary",
                     icon = icon("chart-area"),
                     style = "width: 100%;"),
        
        hr(),
        
        h4("图表说明"),
        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h5("柱状图:"),
          p("显示各组的均值或中位数，可添加误差线表示标准差、标准误或四分位距"),
          
          h5("箱线图:"),
          p("显示数据分布的五数概括（最小值、第一四分位数、中位数、第三四分位数、最大值）"),
          
          h5("小提琴图:"),
          p("结合箱线图和密度图的特点，显示数据分布的形状和密度"),
          
          h5("点图:"),
          p("显示每个数据点，可直观看出数据的分布和离散程度"),
          
          h5("密度图:"),
          p("显示数据分布的概率密度，可比较不同组的分布形状"),
          
          h5("QQ图:"),
          p("用于检验数据是否符合正态分布，点越接近直线，数据越符合正态分布")
        )
      ),
      
      box(
        title = "图表显示", status = "info", solidHeader = TRUE,
        width = 8,
        
        conditionalPanel(
          condition = "output['plot_generated']", ns = ns,
          
          div(style = "text-align: center; margin-bottom: 15px;",
            downloadButton(ns("download_plot"), "下载图表",
                           class = "btn-primary"),
            downloadButton(ns("download_normal_pdf"), "下载分析报告",
                           class = "btn-primary"),
            tags$style(type="text/css",
              "#download_plot, #download_normal_pdf { margin-left: 5px; }")
          ),
          
          conditionalPanel(
            condition = "!input.show_interactive || input.plot_type == 'qq'", ns = ns,
            plotOutput(ns("main_plot"), height = "500px")
          ),
          
          conditionalPanel(
            condition = "input.show_interactive && input.plot_type != 'qq'", ns = ns,
            plotlyOutput(ns("interactive_plot"), height = "500px")
          )
        ),
        
        conditionalPanel(
          condition = "!output['plot_generated']", ns = ns,
          
          div(style = "text-align: center; padding: 100px 20px;",
            h3("请设置图表参数并点击生成图表"),
            icon("chart-area", style = "font-size: 48px; color: #3c8dbc;"),
            p("选择图表类型、颜色方案等参数，然后点击生成图表按钮")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "!output['any_data_available']", ns = ns,
      
      box(
        title = "数据可视化", status = "warning", solidHeader = TRUE,
        width = 12,
        
        div(style = "text-align: center; padding: 50px;",
          h3("请先上传数据"),
          p("可视化需要上传数据后才能使用"),
          icon("chart-area", style = "font-size: 48px; color: #3c8dbc;")
        )
      )
    )
  )
}

visualization_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间函数
    
    # 数据可用性
    output$any_data_available <- reactive({
      !is.null(values$data)
    })
    outputOptions(output, "any_data_available", suspendWhenHidden = FALSE)
    
    # 动态更新QQ图分组选择
    observe({
      if (!is.null(values$data) && input$plot_type == "qq") {
        if (values$data_type == "regular") {
          unique_groups <- unique(values$data$Group)
          updateSelectInput(session, "qq_group", 
                           choices = c("所有组合并" = "all", unique_groups),
                           selected = "all")
        } else if (values$data_type == "qpcr") {
          unique_genes <- unique(values$data$Gene_Name)
          updateSelectInput(session, "qq_group", 
                           choices = c("所有基因合并" = "all", unique_genes),
                           selected = "all")
        }
      }
    })
    
    # 生成图表
    observeEvent(input$generate_plot, {
      if (is.null(values$data)) {
        show_error_notification("请先上传数据")
        return()
      }
      
      # 显示进度
      withProgress(message = '正在生成图表...', value = 0, {
        
        # 准备显著性标注数据
        sig_data <- NULL
        if (input$show_significance && input$plot_type != "qq") {
          if (values$data_type == "regular" && !is.null(values$normal_results)) {
            sig_data <- prepare_significance_data(
              data = values$data,
              test_result = values$normal_results$test_result,
              posthoc_result = values$normal_results$posthoc_result,
              x_col = "Group",
              y_col = "Value",
              sig_level = input$sig_level %||% 0.05
            )
          } else if (values$data_type == "qpcr" && !is.null(values$qpcr_results)) {
            sig_data <- prepare_significance_data(
              data = values$qpcr_results$delta_ct,
              test_result = values$qpcr_results$test_result,
              posthoc_result = values$qpcr_results$posthoc_result,
              x_col = "Group",
              y_col = "delta_ct",
              sig_level = input$sig_level %||% 0.05
            )
          }
        }
        
        # 根据数据类型和图表类型创建图表
        if (values$data_type == "regular") {
          if (input$plot_type == "bar") {
            values$current_plot <- create_bar_plot(
              data = values$data,
              x_col = "Group",
              y_col = "Value",
              title = if (input$plot_title != "") input$plot_title else "柱状图",
              x_label = if (input$x_label != "") input$x_label else "分组",
              y_label = if (input$y_label != "") input$y_label else "值",
              bar_type = input$bar_type,
              color_palette = input$color_palette,
              show_significance = input$show_significance,
              sig_data = sig_data
            )
          } else if (input$plot_type == "box") {
            values$current_plot <- create_box_plot(
              data = values$data,
              x_col = "Group",
              y_col = "Value",
              title = if (input$plot_title != "") input$plot_title else "箱线图",
              x_label = if (input$x_label != "") input$x_label else "分组",
              y_label = if (input$y_label != "") input$y_label else "值",
              color_palette = input$color_palette,
              show_significance = input$show_significance,
              sig_data = sig_data
            )
          } else if (input$plot_type == "violin") {
            values$current_plot <- create_violin_plot(
              data = values$data,
              x_col = "Group",
              y_col = "Value",
              title = if (input$plot_title != "") input$plot_title else "小提琴图",
              x_label = if (input$x_label != "") input$x_label else "分组",
              y_label = if (input$y_label != "") input$y_label else "值",
              color_palette = input$color_palette,
              show_significance = input$show_significance,
              sig_data = sig_data
            )
          } else if (input$plot_type == "dot") {
            values$current_plot <- create_dot_plot(
              data = values$data,
              x_col = "Group",
              y_col = "Value",
              title = if (input$plot_title != "") input$plot_title else "点图",
              x_label = if (input$x_label != "") input$x_label else "分组",
              y_label = if (input$y_label != "") input$y_label else "值",
              color_palette = input$color_palette,
              show_significance = input$show_significance,
              sig_data = sig_data
            )
          } else if (input$plot_type == "density") {
            values$current_plot <- create_density_plot(
              data = values$data,
              x_col = "Group",
              y_col = "Value",
              title = if (input$plot_title != "") input$plot_title else "密度图",
              x_label = if (input$x_label != "") input$x_label else "值",
              y_label = if (input$y_label != "") input$y_label else "密度",
              color_palette = input$color_palette
            )
          } else if (input$plot_type == "qq") {
            if (input$qq_group == "all") {
              values$current_plot <- create_qq_plot(
                data = values$data,
                value_col = "Value",
                title = if (input$plot_title != "") input$plot_title else "QQ图",
                group_col = NULL
              )
            } else {
              values$current_plot <- create_qq_plot(
                data = values$data[values$data$Group == input$qq_group, ],
                value_col = "Value",
                title = if (input$plot_title != "") input$plot_title else paste("QQ图 -", input$qq_group),
                group_col = NULL
              )
            }
          }
        } else if (values$data_type == "qpcr") {
          if (input$plot_type == "bar") {
            values$current_plot <- create_qpcr_plot(
              data = values$qpcr_results$results,
              x_col = "Group",
              y_col = "fold_change",
              title = if (input$plot_title != "") input$plot_title else "基因相对表达量",
              x_label = if (input$x_label != "") input$x_label else "分组",
              y_label = if (input$y_label != "") input$y_label else "相对表达量 (2^(-ΔΔCt))",
              color_palette = input$color_palette,
              show_significance = input$show_significance,
              sig_data = sig_data
            )
          } else if (input$plot_type == "box") {
            values$current_plot <- create_qpcr_box_plot(
              data = values$qpcr_results$delta_ct,
              x_col = "Group",
              y_col = "delta_ct",
              title = if (input$plot_title != "") input$plot_title else "ΔCt值箱线图",
              x_label = if (input$x_label != "") input$x_label else "分组",
              y_label = if (input$y_label != "") input$y_label else "ΔCt值",
              color_palette = input$color_palette,
              show_significance = input$show_significance,
              sig_data = sig_data
            )
          } else if (input$plot_type == "qq") {
            if (input$qq_group == "all") {
              values$current_plot <- create_qq_plot(
                data = values$qpcr_results$delta_ct,
                value_col = "delta_ct",
                title = if (input$plot_title != "") input$plot_title else "ΔCt值QQ图",
                group_col = NULL
              )
            } else {
              values$current_plot <- create_qq_plot(
                data = values$qpcr_results$delta_ct[values$qpcr_results$delta_ct$Gene_Name == input$qq_group, ],
                value_col = "delta_ct",
                title = if (input$plot_title != "") input$plot_title else paste("ΔCt值QQ图 -", input$qq_group),
                group_col = NULL
              )
            }
          } else {
            # 对于qPCR数据，其他图表类型使用ΔCt值
            if (input$plot_type == "violin") {
              values$current_plot <- create_violin_plot(
                data = values$qpcr_results$delta_ct,
                x_col = "Group",
                y_col = "delta_ct",
                title = if (input$plot_title != "") input$plot_title else "ΔCt值小提琴图",
                x_label = if (input$x_label != "") input$x_label else "分组",
                y_label = if (input$y_label != "") input$y_label else "ΔCt值",
                color_palette = input$color_palette,
                show_significance = input$show_significance,
                sig_data = sig_data
              )
            } else if (input$plot_type == "dot") {
              values$current_plot <- create_dot_plot(
                data = values$qpcr_results$delta_ct,
                x_col = "Group",
                y_col = "delta_ct",
                title = if (input$plot_title != "") input$plot_title else "ΔCt值点图",
                x_label = if (input$x_label != "") input$x_label else "分组",
                y_label = if (input$y_label != "") input$y_label else "ΔCt值",
                color_palette = input$color_palette,
                show_significance = input$show_significance,
                sig_data = sig_data
              )
            } else if (input$plot_type == "density") {
              values$current_plot <- create_density_plot(
                data = values$qpcr_results$delta_ct,
                x_col = "Group",
                y_col = "delta_ct",
                title = if (input$plot_title != "") input$plot_title else "ΔCt值密度图",
                x_label = if (input$x_label != "") input$x_label else "ΔCt值",
                y_label = if (input$y_label != "") input$y_label else "密度",
                color_palette = input$color_palette
              )
            }
          }
        }
        
        # 显示成功消息
        show_success_notification("图表生成完成！")
      })
    })
    
    # 图表生成状态
    output$plot_generated <- reactive({
      !is.null(values$current_plot)
    })
    outputOptions(output, "plot_generated", suspendWhenHidden = FALSE)
    
    # 显示静态图表
    output$main_plot <- renderPlot({
      if (!is.null(values$current_plot)) {
        return(values$current_plot)
      }
    })
    
    # 输出当前图表类型，用于conditionalPanel
    output$plot_type <- reactive({
      return(input$plot_type)
    })
    outputOptions(output, "plot_type", suspendWhenHidden = FALSE)
    
    # 显示交互式图表
    output$interactive_plot <- renderPlotly({
      # 如果是QQ图，不渲染交互式图表
      if (input$plot_type == "qq") {
        return(NULL)
      }
      
      if (!is.null(values$current_plot)) {
        return(create_interactive_plot(values$current_plot, values$data, "Group", "Value"))
      }
    })
    
    # 下载图表
    output$download_plot <- downloadHandler(
      filename = function() {
        plot_type_name <- switch(input$plot_type,
                                "bar" = "柱状图",
                                "box" = "箱线图",
                                "violin" = "小提琴图",
                                "dot" = "点图",
                                "density" = "密度图",
                                "qq" = "QQ图")
        paste0(plot_type_name, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (!is.null(values$current_plot)) {
          save_plot(values$current_plot, file)
        }
      }
    )
    
    # 下载PDF分析报告
    output$download_normal_pdf <- downloadHandler(
      filename = function() {
        paste0("常规指标分析报告_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (!is.null(values$normal_results) && values$data_type == "regular") {
          # 创建PDF文件，使用cairo_pdf支持Unicode
          cairo_pdf(file, width = 12, height = 15, family = "SimHei")
          
          # 添加标题页
          par(mar = c(0, 0, 0, 0))
          plot.new()
          text(0.5, 0.5, "常规指标分析报告", cex = 3, font = 2)
          text(0.5, 0.4, paste0("生成日期: ", Sys.Date()), cex = 1.5)
          
          # 添加原始数据
          plot.new()
          par(mar = c(5, 4, 4, 2) + 0.1)
          text(0.5, 0.95, "原始数据", cex = 1.5, font = 2)
          gridExtra::grid.table(values$data)
          
          # 添加异常值检测说明
          plot.new()
          text(0.5, 0.95, "异常值检测说明", cex = 1.5, font = 2)
          
          # 检查是否有异常值
          has_outliers <- !is.null(values$outlier_info) && length(values$outlier_info) > 0
          
          if (has_outliers) {
            # 获取第一个组的信息来显示方法和阈值
            first_group <- names(values$outlier_info)[1]
            
            # 创建异常值检测说明表
            outlier_summary <- data.frame(
              参数 = c("检测方法", "阈值", "是否检测到异常值", "异常值总数"),
              值 = c(
                values$outlier_info[[first_group]]$method,
                values$outlier_info[[first_group]]$threshold,
                "是",
                length(values$outlier_indices)
              )
            )
            gridExtra::grid.table(outlier_summary)
            
            # 添加各组异常值详情
            outlier_details <- data.frame(
              组别 = character(),
              异常值数量 = integer(),
              异常值索引 = character(),
              stringsAsFactors = FALSE
            )
            
            for (group in names(values$outlier_info)) {
              info <- values$outlier_info[[group]]
              if (nrow(info$outliers) > 0) {
                outlier_indices <- paste(info$outliers$Index, collapse = ", ")
                outlier_details <- rbind(outlier_details, data.frame(
                  组别 = group,
                  异常值数量 = nrow(info$outliers),
                  异常值索引 = outlier_indices,
                  stringsAsFactors = FALSE
                ))
              }
            }
            
            if (nrow(outlier_details) > 0) {
              text(0.5, 0.6, "各组异常值详情", cex = 1.2, font = 1)
              gridExtra::grid.table(outlier_details)
            }
          } else {
            # 没有检测到异常值的情况，但仍显示检测方法和阈值
            # 获取检测方法和阈值
            # 由于在可视化模块中无法直接访问normal模块的input，这里使用默认值
            method <- "IQR"
            threshold <- 1.5
            
            outlier_summary <- data.frame(
              参数 = c("检测方法", "阈值", "是否检测到异常值", "异常值总数"),
              值 = c(method, threshold, "否", 0)
            )
            gridExtra::grid.table(outlier_summary)
          }
          
          # 添加描述性统计
          plot.new()
          text(0.5, 0.95, "描述性统计", cex = 1.5, font = 2)
          desc_stats <- describe_data(values$data, "Value", "Group")
          gridExtra::grid.table(desc_stats)
          
          # 添加正态性检验
          plot.new()
          text(0.5, 0.95, "正态性检验", cex = 1.5, font = 2)
          normality <- test_normality(values$data, "Value", "Group")
          gridExtra::grid.table(normality)
          
          # 添加方差齐性检验
          plot.new()
          text(0.5, 0.95, "方差齐性检验", cex = 1.5, font = 2)
          variance <- test_variance_homogeneity(values$data, "Value", "Group")
          variance_data <- data.frame(
            参数 = c("检验方法", "统计量", "p值", "方差齐性"),
            值 = c(
              variance$test,
              round(variance$statistic, 4),
              ifelse(variance$p_value < 0.001, "< 0.001", round(variance$p_value, 4)),
              ifelse(variance$homogeneous, "是", "否")
            )
          )
          gridExtra::grid.table(variance_data)
          
          # 添加统计检验结果
          plot.new()
          text(0.5, 0.95, "统计检验", cex = 1.5, font = 2)
          test_result <- values$normal_results$test_result
          
          if (test_result$test == "Independent t-test") {
            test_data <- data.frame(
              参数 = c("检验方法", "t值", "自由度", "p值"),
              值 = c(
                test_result$test,
                round(test_result$statistic, 4),
                round(test_result$df, 4),
                ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4))
              )
            )
          } else if (test_result$test == "One-way ANOVA") {
            test_data <- data.frame(
              参数 = c("检验方法", "F值", "自由度", "p值"),
              值 = c(
                test_result$test,
                round(test_result$f_value, 4),
                round(test_result$df, 4),
                ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4))
              )
            )
          } else if (test_result$test == "Wilcoxon rank sum test") {
            test_data <- data.frame(
              参数 = c("检验方法", "W值", "p值"),
              值 = c(
                test_result$test,
                round(test_result$statistic, 4),
                ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4))
              )
            )
          } else if (test_result$test == "Kruskal-Wallis test") {
            test_data <- data.frame(
              参数 = c("检验方法", "chi-squared值", "自由度", "p值"),
              值 = c(
                test_result$test,
                round(test_result$statistic, 4),
                round(test_result$df, 4),
                ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4))
              )
            )
          }
          
          gridExtra::grid.table(test_data)
          
          # 添加事后比较结果
          if (!is.null(values$normal_results$posthoc_result)) {
            plot.new()
            text(0.5, 0.95, "事后比较", cex = 1.5, font = 2)
            posthoc_result <- values$normal_results$posthoc_result
            
            if (posthoc_result$test == "Tukey HSD") {
              # 将TukeyHSD对象转换为数据框
              tukey_results <- posthoc_result$results[[1]]
              posthoc_df <- data.frame(
                比较组 = rownames(tukey_results),
                差值 = round(tukey_results[, "diff"], 4),
                下限 = round(tukey_results[, "lwr"], 4),
                上限 = round(tukey_results[, "upr"], 4),
                校正p值 = ifelse(tukey_results[, "p adj"] < 0.001, "< 0.001", 
                                round(tukey_results[, "p adj"], 4)),
                显著性 = ifelse(tukey_results[, "p adj"] < 0.05, "显著", "不显著"),
                stringsAsFactors = FALSE
              )
              gridExtra::grid.table(posthoc_df)
            } else if (posthoc_result$test == "Dunn test with Bonferroni correction") {
              # 将Dunn检验结果转换为数据框
              dunn_results <- posthoc_result$results
              comparisons <- dunn_results$statistic
              p_values <- dunn_results$p.value
              
              posthoc_df <- data.frame(
                比较组 = names(comparisons),
                统计量 = round(as.numeric(comparisons), 4),
                校正p值 = ifelse(p_values < 0.001, "< 0.001", 
                                round(p_values, 4)),
                显著性 = ifelse(p_values < 0.05, "显著", "不显著"),
                stringsAsFactors = FALSE
              )
              gridExtra::grid.table(posthoc_df)
            }
          }
          
          # 添加可视化图表
          plot.new()
          text(0.5, 0.95, "数据可视化", cex = 1.5, font = 2)
          if (!is.null(values$current_plot)) {
            print(values$current_plot)
          }
          
          # 关闭PDF设备
          dev.off()
        } else {
          # 如果没有分析结果，显示错误信息
          cairo_pdf(file, width = 8, height = 6, family = "SimHei")
          plot.new()
          text(0.5, 0.5, "未找到分析结果，请先进行常规指标分析", cex = 1.5, col = "red")
          dev.off()
        }
      }
    )
  })
}