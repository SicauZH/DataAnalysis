# 数据上传模块

data_upload_ui <- function(id) {
  ns <- NS(id)
  
  # 添加shinyjs
  shinyjs::useShinyjs()
  
  fluidRow(
    box(
      title = "数据上传", status = "primary", solidHeader = TRUE,
      width = 12,
      
      column(width = 6,
        h4("上传数据文件"),
        p("支持Excel (.xlsx, .xls) 和 CSV 格式文件"),
        
        fileInput(ns("data_file"), "选择文件",
                  accept = c(".xlsx", ".xls", ".csv")),
        
        conditionalPanel(
          condition = "output['data_uploaded']", ns = ns,
          
          h4("数据类型说明"),
          div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            h5("荧光定量CT值数据 (4列):"),
            tags$ul(
              tags$li("第1列: 样本ID"),
              tags$li("第2列: 基因名称"),
              tags$li("第3列: CT值"),
              tags$li("第4列: 分组")
            ),
            
            h5("常规实验指标数据 (3列):"),
            tags$ul(
              tags$li("第1列: 样本ID"),
              tags$li("第2列: 测量值"),
              tags$li("第3列: 分组")
            )
          )
        )
      ),
      
      column(width = 6,
        conditionalPanel(
          condition = "output['data_uploaded']", ns = ns,
          
          h4("数据预览"),
          DT::dataTableOutput(ns("data_preview")),
          
          div(style = "margin-top: 15px;",
            uiOutput(ns("data_info"))
          ),
          
          div(style = "margin-top: 15px;",
            uiOutput(ns("navigation_buttons"))
          )
        )
      )
    )
  )

} # nolint

data_upload_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间函数
    
    # 数据上传处理
    observeEvent(input$data_file, {
      req(input$data_file)
      
      # 显示加载动画
      withProgress(message = '正在读取数据...', value = 0, {
        # 读取数据
        result <- safe_read_data(input$data_file$datapath)
        
        if (!result$success) {
          show_error_notification(result$message)
          values$data <- NULL
          values$data_type <- NULL
          return()
        }
        
        # 检测数据类型
        type_result <- detect_data_type(result$result)
        
        if (type_result$type == "unknown") {
          show_error_notification("数据格式错误：支持3列(常规数据)或4列(qPCR数据)，不支持其他列数")
          values$data <- NULL
          values$data_type <- NULL
          return()
        }
        
        # 验证数据
        if (type_result$type == "regular") {
          validation <- validate_data(type_result$data, 
                                     required_columns = c("Sample_ID", "Value", "Group"),
                                     min_rows = 3)
        } else {
          validation <- validate_data(type_result$data, 
                                     required_columns = c("Sample_ID", "Gene_Name", "CT_Value", "Group"),
                                     min_rows = 3)
        }
        
        if (!validation$valid) {
          show_error_notification(paste("数据验证失败:", paste(validation$errors, collapse = "; ")))
          values$data <- NULL
          values$data_type <- NULL
          return()
        }
        
        # 存储数据
        values$data <- type_result$data
        values$data_type <- type_result$type
        
        # 显示成功消息
        if (type_result$type == "regular") {
          show_success_notification("常规指标数据上传成功！")
        } else {
          show_success_notification("荧光定量数据上传成功！")
        }
        
        # 添加调试信息
        print("数据处理完成，尝试启用处理按钮")
        print(paste("数据类型:", values$data_type))
        print(paste("数据行数:", nrow(values$data)))
      })
    })
    
    # 数据预览
    output$data_preview <- renderDT({
      if (!is.null(values$data)) {
        datatable(values$data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  caption = "数据预览") %>%
          formatStyle(columns = 0, backgroundColor = 'lightgray')
      }
    })
    
    # 数据信息
    output$data_info <- renderUI({
      if (!is.null(values$data)) {
        if (values$data_type == "regular") {
          div(
            h5("数据信息:"),
            tags$ul(
              tags$li(paste("数据类型: 常规指标数据")),
              tags$li(paste("样本数:", nrow(values$data))),
              tags$li(paste("分组数:", length(unique(values$data$Group)))),
              tags$li(paste("分组:", paste(unique(values$data$Group), collapse = ", ")))
            )
          )
        } else {
          div(
            h5("数据信息:"),
            tags$ul(
              tags$li(paste("数据类型: 荧光定量数据")),
              tags$li(paste("样本数:", nrow(values$data))),
              tags$li(paste("基因数:", length(unique(values$data$Gene_Name)))),
              tags$li(paste("基因:", paste(unique(values$data$Gene_Name), collapse = ", "))),
              tags$li(paste("分组数:", length(unique(values$data$Group)))),
              tags$li(paste("分组:", paste(unique(values$data$Group), collapse = ", ")))
            )
          )
        }
      }
    })
    
    # 数据上传状态
    output$data_uploaded <- reactive({
      !is.null(values$data)
    })
    outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
    
    # 导航按钮
    output$navigation_buttons <- renderUI({
      if (!is.null(values$data)) {
        div(class = "alert alert-info",
          h4("数据上传成功！"),
          p(if (values$data_type == "qpcr") {
            "请点击'荧光定量分析'标签页进行数据分析。"
          } else {
            "请点击'常规指标分析'标签页进行数据分析。"
          }),
          div(style = "margin-top: 10px;",
            if (values$data_type == "qpcr") {
              actionButton(ns("go_to_qpcr"), "前往荧光定量分析", 
                           class = "btn-primary", 
                           icon = icon("arrow-right"))
            } else {
              actionButton(ns("go_to_normal"), "前往常规分析", 
                           class = "btn-primary", 
                           icon = icon("arrow-right"))
            }
          )
        )
      }
    })
    
    # 跳转到qPCR分析页面按钮点击事件
    observeEvent(input$go_to_qpcr, {
      if (is.null(values$data) || values$data_type != "qpcr") {
        show_error_notification("请先上传qPCR数据")
        return()
      }
      
      # 跳转到qPCR分析页面
      session$sendCustomMessage(type = "redirectTab", message = list(tab = "qpcr"))
    })
    
    # 跳转到常规分析页面按钮点击事件
    observeEvent(input$go_to_normal, {
      if (is.null(values$data) || values$data_type != "regular") {
        show_error_notification("请先上传常规数据")
        return()
      }
      
      # 跳转到常规分析页面
      session$sendCustomMessage(type = "redirectTab", message = list(tab = "normal"))
    })
  })
}