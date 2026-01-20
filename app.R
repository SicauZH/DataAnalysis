# 生物数据统计分析平台
# 主应用文件

# 加载必要的库
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(RColorBrewer)
library(gridExtra)
library(scales)
library(purrr)
library(stats)
library(openxlsx)

# 加载工具函数
source("utils/error_handling.R", local = TRUE)
source("utils/data_processing.R", local = TRUE)
source("utils/plot_functions.R", local = TRUE)
source("utils/additional_plot_functions.R", local = TRUE)
source("utils/common_functions.R", local = TRUE)
source("utils/performance_optimization.R", local = TRUE)
source("utils/interactive_plots.R", local = TRUE)

# 加载服务器配置（如果存在）
if (file.exists("config/server_config.R")) {
  source("config/server_config.R", local = TRUE)
  
  # 初始化服务器环境
  if (exists("init_server_environment")) {
    init_server_environment()
  }
}

# 加载UI和Server组件
source("ui/ui_main.R", local = TRUE)
source("server/server_main.R", local = TRUE)

# 加载所有模块
source("modules/data_upload_module.R", local = TRUE)
source("modules/qpcr_analysis_module.R", local = TRUE)
source("modules/normal_analysis_module.R", local = TRUE)
source("modules/visualization_module.R", local = TRUE)

# 定义应用Server
server <- function(input, output, session) {
  # 调用主Server函数
  values <- server_main(input, output, session)
  
  # 调用各模块Server函数
  data_upload_server("upload_main", values)
  qpcr_analysis_server("qpcr_main", values)
  normal_analysis_server("normal_main", values)
  visualization_server("visualization_main", values)
}

# 定义应用UI
ui <- ui_main

# 运行应用
shinyApp(ui = ui, server = server)
