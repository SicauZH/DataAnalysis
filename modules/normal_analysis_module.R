# 常规指标分析模块

normal_analysis_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    conditionalPanel(
      condition = "output['normal_data_available']", ns = ns,
      
      box(
        title = "数据分析流程", status = "primary", solidHeader = TRUE,
        width = 4,
        
        h4("步骤1：数据描述性统计"),
        verbatimTextOutput(ns("desc_stats")),
        
        h4("步骤2：正态性检验"),
        verbatimTextOutput(ns("normality_test")),
        
        h4("步骤3：方差齐性检验"),
        verbatimTextOutput(ns("variance_test")),
        
        h4("步骤4：异常值处理"),
        p("IQR方法：适用于非正态分布或分布未知的数据，基于四分位数范围检测异常值"),
        p("标准差方法：适用于正态分布的数据，基于均值和标准差检测异常值"),
        
        radioButtons(ns("outlier_method"), "异常值检测方法：",
                     choices = list("IQR方法" = "iqr",
                                    "标准差方法" = "sd"),
                     selected = "iqr"),
        
        conditionalPanel(
          condition = "input.outlier_method == 'iqr'", ns = ns,
          p("阈值：1.5×IQR（温和异常值）或3×IQR（极端异常值）"),
          sliderInput(ns("iqr_threshold"), "IQR阈值倍数：", 
                      min = 1.5, max = 3, value = 1.5, step = 0.1)
        ),
        
        conditionalPanel(
          condition = "input.outlier_method == 'sd'", ns = ns,
          p("阈值：2×SD（温和异常值）或3×SD（极端异常值）"),
          sliderInput(ns("sd_threshold"), "标准差阈值倍数：", 
                      min = 2, max = 3, value = 2, step = 0.1)
        ),
        
        actionButton(ns("detect_outliers"), "检测异常值",
                     class = "btn-warning",
                     icon = icon("search")),
        
        verbatimTextOutput(ns("outlier_info")),
        
        p("建议：如果检测到异常值，请先检查数据记录是否正确，然后考虑剔除异常值后重新上传数据进行分析。"),
        
        h4("步骤5：统计检验"),
        selectInput(ns("test_type"), "选择检验方法：",
                    choices = list("自动选择" = "auto",
                                   "参数检验" = "parametric",
                                   "非参数检验" = "nonparametric"),
                    selected = "auto"),
        
        actionButton(ns("run_statistical_test"), "执行统计检验",
                     class = "btn-success",
                     icon = icon("flask")),
        
        h4("步骤6：事后比较方法说明"),
        p("Tukey HSD：用于方差分析后的多重比较，要求各组方差齐性，控制所有比较中的总错误率"),
        p("Dunn检验：用于Kruskal-Wallis检验后的多重比较，适用于非参数数据，使用Bonferroni方法调整p值")
      ),
      
      box(
        title = "检验结果", status = "info", solidHeader = TRUE,
        width = 8,
        
        tabsetPanel(
          tabPanel("检验结果", verbatimTextOutput(ns("test_results"))),
          tabPanel("事后比较", verbatimTextOutput(ns("posthoc_results"))),
          tabPanel("结论", verbatimTextOutput(ns("conclusions"))),
          tabPanel("数据下载", 
                   downloadButton(ns("download_normal_results"), "下载Excel分析报告",
                                  class = "btn-primary"))
        )
      )
    ),
    
    conditionalPanel(
      condition = "!output['normal_data_available']", ns = ns,
      
      box(
        title = "常规指标分析", status = "warning", solidHeader = TRUE,
        width = 12,
        
        div(style = "text-align: center; padding: 50px;",
          h3("请先上传常规指标数据"),
          p("常规指标分析需要3列数据：样本ID, 测量值, 分组"),
          icon("chart-bar", style = "font-size: 48px; color: #3c8dbc;")
        )
      )
    )
  )
}

normal_analysis_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间函数
    
    # 常规数据可用性
    output$normal_data_available <- reactive({
      !is.null(values$data) && values$data_type == "regular"
    })
    outputOptions(output, "normal_data_available", suspendWhenHidden = FALSE)
    
    # 描述性统计
    output$desc_stats <- renderPrint({
      if (!is.null(values$data) && values$data_type == "regular") {
        stats <- describe_data(values$data, "Value", "Group")
        cat("描述性统计:\n")
        print(stats)
      }
    })
    
    # 正态性检验
    output$normality_test <- renderPrint({
      if (!is.null(values$data) && values$data_type == "regular") {
        normality <- test_normality(values$data, "Value", "Group")
        cat("正态性检验 (Shapiro-Wilk):\n")
        print(normality)
        
        if (all(normality$Normal)) {
          cat("\n所有组数据均符合正态分布 (p > 0.05)\n")
        } else {
          cat("\n部分或全部组数据不符合正态分布 (p ≤ 0.05)\n")
        }
      }
    })
    
    # 方差齐性检验
    output$variance_test <- renderPrint({
      if (!is.null(values$data) && values$data_type == "regular") {
        variance <- test_variance_homogeneity(values$data, "Value", "Group")
        cat("方差齐性检验 (Levene检验):\n")
        cat("F值:", round(variance$statistic, 4), "\n")
        cat("p值:", ifelse(variance$p_value < 0.001, "< 0.001", round(variance$p_value, 4)), "\n")
        
        if (variance$homogeneous) {
          cat("各组方差齐性 (p > 0.05)\n")
        } else {
          cat("各组方差不齐 (p ≤ 0.05)\n")
        }
      }
    })
    
    # 检测异常值
    observeEvent(input$detect_outliers, {
      if (is.null(values$data) || values$data_type != "regular") {
        show_error_notification("请先上传常规指标数据")
        return()
      }
      
      # 显示进度
      withProgress(message = '正在检测异常值...', value = 0, {
        # 检测异常值
        threshold <- if (input$outlier_method == "iqr") input$iqr_threshold else input$sd_threshold
        
        outlier_result <- detect_outliers(
          data = values$data,
          method = input$outlier_method,
          threshold = threshold,
          group_col = "Group",
          value_col = "Value"
        )
        
        # 存储异常值信息
        values$outlier_indices <- outlier_result$indices
        values$outlier_info <- outlier_result$info
        
        # 显示结果
        if (outlier_result$count > 0) {
          show_warning_notification(paste("检测到", outlier_result$count, "个异常值"))
        } else {
          show_success_notification("未检测到异常值")
        }
      })
    })
    
    # 显示异常值信息
    output$outlier_info <- renderPrint({
      if (!is.null(values$outlier_info) && length(values$outlier_info) > 0) {
        cat("异常值检测结果:\n")
        # 获取第一个组的信息来显示方法和阈值
        first_group <- names(values$outlier_info)[1]
        cat("方法:", values$outlier_info[[first_group]]$method, "\n")
        cat("阈值:", values$outlier_info[[first_group]]$threshold, "\n")
        cat("异常值数量:", length(values$outlier_indices), "\n\n")
        
        for (group in names(values$outlier_info)) {
          info <- values$outlier_info[[group]]
          cat("分组:", group, "\n")
          
          if (info$method == "IQR") {
            cat("Q1:", round(info$q1, 4), "\n")
            cat("Q3:", round(info$q3, 4), "\n")
            cat("IQR:", round(info$iqr, 4), "\n")
            cat("下界:", round(info$lower_bound, 4), "\n")
            cat("上界:", round(info$upper_bound, 4), "\n")
          } else {
            cat("均值:", round(info$mean, 4), "\n")
            cat("标准差:", round(info$sd, 4), "\n")
            cat("下界:", round(info$lower_bound, 4), "\n")
            cat("上界:", round(info$upper_bound, 4), "\n")
          }
          
          if (nrow(info$outliers) > 0) {
            cat("异常值:\n")
            print(info$outliers)
          } else {
            cat("无异常值\n")
          }
          cat("\n")
        }
      } else {
        cat("未检测到异常值或数据不足以进行异常值检测\n")
      }
    })
    
    # 执行统计检验
    observeEvent(input$run_statistical_test, {
      if (is.null(values$data) || values$data_type != "regular") {
        show_error_notification("请先上传常规指标数据")
        return()
      }
      
      # 显示进度
      withProgress(message = '正在执行统计检验...', value = 0, {
        # 选择检验方法
        test_info <- select_statistical_test(
          data = values$data,
          value_col = "Value",
          group_col = "Group",
          test_type = input$test_type
        )
        
        # 执行统计检验
        test_result <- perform_statistical_test(
          data = values$data,
          value_col = "Value",
          group_col = "Group",
          test_info = test_info
        )
        
        # 执行事后比较
        posthoc_result <- post_hoc_test(
          data = values$data,
          value_col = "Value",
          group_col = "Group",
          test_info = test_info
        )
        
        # 存储结果
        values$normal_results <- list(
          test_info = test_info,
          test_result = test_result,
          posthoc_result = posthoc_result
        )
        
        # 显示成功消息
        show_success_notification("统计检验完成！")
      })
    })
    
    # 显示检验结果
    output$test_results <- renderPrint({
      if (!is.null(values$normal_results)) {
        test_result <- values$normal_results$test_result
        test_info <- values$normal_results$test_info
        
        cat("统计检验结果:\n")
        cat("检验方法:", test_result$test, "\n")
        
        if (test_result$test == "Independent t-test") {
          cat("t值:", round(test_result$statistic, 4), "\n")
          cat("自由度:", round(test_result$df, 4), "\n")
          cat("p值:", ifelse(test_result$p_value < 0.001, "< 0.001", round(test_result$p_value, 4)), "\n")
          
          if (!is.null(test_result$estimate)) {
            cat("均值差异:\n")
            print(test_result$estimate)
          }
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
        print(test_info$normality)
        
        cat("\n方差齐性检验结果:\n")
        cat("检验方法:", test_info$variance$test, "\n")
        cat("统计量:", round(test_info$variance$statistic, 4), "\n")
        cat("p值:", ifelse(test_info$variance$p_value < 0.001, "< 0.001", round(test_info$variance$p_value, 4)), "\n")
        cat("方差齐性:", ifelse(test_info$variance$homogeneous, "是", "否"), "\n")
      }
    })
    
    # 显示事后比较结果
    output$posthoc_results <- renderPrint({
      if (!is.null(values$normal_results) && !is.null(values$normal_results$posthoc_result)) {
        posthoc_result <- values$normal_results$posthoc_result
        
        cat("事后比较结果:\n")
        cat("方法:", posthoc_result$test, "\n\n")
        
        if (posthoc_result$test == "Tukey HSD") {
          print(posthoc_result$results)
        } else if (posthoc_result$test == "Dunn test with Bonferroni correction") {
          print(posthoc_result$results)
        }
      }
    })
    
    # 显示结论
    output$conclusions <- renderPrint({
      if (!is.null(values$normal_results)) {
        test_result <- values$normal_results$test_result
        test_info <- values$normal_results$test_info
        
        cat("分析结论:\n\n")
        
        # 数据分布情况
        if (all(test_info$normality$Normal)) {
          cat("1. 数据分布: 各组数据均符合正态分布 (Shapiro-Wilk检验, p > 0.05)\n")
        } else {
          cat("1. 数据分布: 部分或全部组数据不符合正态分布 (Shapiro-Wilk检验, p ≤ 0.05)\n")
        }
        
        # 方差齐性情况
        if (test_info$variance$homogeneous) {
          cat("2. 方差齐性: 各组方差齐性 (Levene检验, p > 0.05)\n")
        } else {
          cat("2. 方差齐性: 各组方差不齐 (Levene检验, p ≤ 0.05)\n")
        }
        
        # 检验方法选择
        cat("3. 检验方法:", test_result$test, "\n")
        
        # 统计检验结果
        if (test_result$p_value < 0.001) {
          cat("4. 统计检验: 各组间差异极显著 (p < 0.001)\n")
        } else if (test_result$p_value < 0.01) {
          cat("4. 统计检验: 各组间差异高度显著 (0.001 ≤ p < 0.01)\n")
        } else if (test_result$p_value < 0.05) {
          cat("4. 统计检验: 各组间差异显著 (0.01 ≤ p < 0.05)\n")
        } else {
          cat("4. 统计检验: 各组间差异不显著 (p ≥ 0.05)\n")
        }
        
        # 事后比较
        if (!is.null(values$normal_results$posthoc_result)) {
          cat("5. 事后比较: 已执行", values$normal_results$posthoc_result$test, "\n\n")
          
          # 添加事后比较结果表格
          cat("事后比较详细结果:\n\n")
          
          if (values$normal_results$posthoc_result$test == "Tukey HSD") {
            # 处理Tukey HSD结果
            tukey_results <- values$normal_results$posthoc_result$results[[1]]
            
            # 创建数据框用于展示
            comparison_df <- data.frame(
              比较组 = rownames(tukey_results),
              差值 = round(tukey_results[, "diff"], 4),
              下限 = round(tukey_results[, "lwr"], 4),
              上限 = round(tukey_results[, "upr"], 4),
              校正p值 = ifelse(tukey_results[, "p adj"] < 0.001, "< 0.001", 
                              round(tukey_results[, "p adj"], 4)),
              显著性 = ifelse(tukey_results[, "p adj"] < 0.05, "显著", "不显著"),
              stringsAsFactors = FALSE
            )
            
            # 重命名列名
            colnames(comparison_df) <- c("比较组", "差值", "95%CI下限", "95%CI上限", "校正p值", "显著性")
            
            # 打印表格
            print(comparison_df, row.names = FALSE)
            
            # 添加显著性总结
            sig_pairs <- sum(tukey_results[, "p adj"] < 0.05)
            total_pairs <- nrow(tukey_results)
            cat("\n显著性总结: 在", total_pairs, "组比较中，有", sig_pairs, "组差异显著\n")
            
          } else if (values$normal_results$posthoc_result$test == "Dunn test with Bonferroni correction") {
            # 处理Dunn检验结果
            dunn_results <- values$normal_results$posthoc_result$results
            
            # 提取比较结果
            comparisons <- dunn_results$statistic
            p_values <- dunn_results$p.value
            
            # 创建数据框用于展示
            comparison_df <- data.frame(
              比较组 = names(comparisons),
              统计量 = round(as.numeric(comparisons), 4),
              校正p值 = ifelse(p_values < 0.001, "< 0.001", 
                              round(p_values, 4)),
              显著性 = ifelse(p_values < 0.05, "显著", "不显著"),
              stringsAsFactors = FALSE
            )
            
            # 打印表格
            print(comparison_df, row.names = FALSE)
            
            # 添加显著性总结
            sig_pairs <- sum(p_values < 0.05)
            total_pairs <- length(p_values)
            cat("\n显著性总结: 在", total_pairs, "组比较中，有", sig_pairs, "组差异显著\n")
          }
        }
      }
    })
    
    # 下载分析结果
    output$download_normal_results <- downloadHandler(
      filename = function() {
        paste0("常规指标分析结果_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (!is.null(values$normal_results)) {
          # 创建工作簿
          wb <- createWorkbook()
          
          # 第一页：添加原始数据
          addWorksheet(wb, "原始数据")
          writeData(wb, "原始数据", values$data)
          
          # 第二页：添加异常值检测说明
          addWorksheet(wb, "异常值检测说明")
          
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
            writeData(wb, "异常值检测说明", outlier_summary, startRow = 1)
            
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
              writeData(wb, "异常值检测说明", outlier_details, startRow = 7)
            }
          } else {
          # 没有检测到异常值的情况，但仍显示检测方法和阈值
          # 获取检测方法和阈值
          method <- if (input$outlier_method == "iqr") "IQR" else "SD"
          threshold <- if (input$outlier_method == "iqr") input$iqr_threshold else input$sd_threshold
          
          outlier_summary <- data.frame(
            参数 = c("检测方法", "阈值", "是否检测到异常值", "异常值总数"),
            值 = c(method, threshold, "否", 0)
          )
          writeData(wb, "异常值检测说明", outlier_summary, startRow = 1)
        }
          
          # 添加描述性统计
          addWorksheet(wb, "描述性统计")
          desc_stats <- describe_data(values$data, "Value", "Group")
          writeData(wb, "描述性统计", desc_stats)
          
          # 添加正态性检验
          addWorksheet(wb, "正态性检验")
          normality <- test_normality(values$data, "Value", "Group")
          writeData(wb, "正态性检验", normality)
          
          # 添加方差齐性检验
          addWorksheet(wb, "方差齐性检验")
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
          writeData(wb, "方差齐性检验", variance_data)
          
          # 添加统计检验结果
          addWorksheet(wb, "统计检验")
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
          
          writeData(wb, "统计检验", test_data)
          
          # 添加事后比较结果
          if (!is.null(values$normal_results$posthoc_result)) {
            addWorksheet(wb, "事后比较")
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
              writeData(wb, "事后比较", posthoc_df)
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
              writeData(wb, "事后比较", posthoc_df)
            }
          }
          
          # 保存工作簿
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      }
    )
    
    # 下载PDF分析报告
    output$download_normal_pdf <- downloadHandler(
      filename = function() {
        paste0("常规指标分析报告_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (!is.null(values$normal_results)) {
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
            method <- if (input$outlier_method == "iqr") "IQR" else "SD"
            threshold <- if (input$outlier_method == "iqr") input$iqr_threshold else input$sd_threshold
            
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
          
          # 添加可视化图片（如果有）
          if (!is.null(values$current_plot)) {
            plot.new()
            text(0.5, 0.95, "数据可视化", cex = 1.5, font = 2)
            print(values$current_plot)
          }
          
          # 关闭PDF设备
          dev.off()
        }
      }
    )
  })
}
