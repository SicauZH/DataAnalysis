# 加载必要的库
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(colourpicker)
library(DT)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(rlang)
library(patchwork)
library(ggpubr)
library(car)
library(PMCMRplus)
library(rstatix)
library(ggthemes)
library(viridis)
library(RColorBrewer)
library(cowplot)
library(shinyjs)
library(plotly)
library(data.table)
library(showtext) # 用于处理中文字体

# 设置中文字体
font_add("SimHei", "simhei.ttf")
font_add("Microsoft YaHei", "msyh.ttc")
font_add("SimSun", "simsun.ttc")
showtext_auto()

# 设置ggplot2主题以支持中文
theme_set(theme_minimal(base_family = "Microsoft YaHei"))

# 加载自定义模块
source("modules/data_upload_module.R")
source("modules/qpcr_analysis_module.R")
source("modules/normal_analysis_module.R")
source("modules/visualization_module.R")
source("utils/error_handling.R")
source("utils/data_processing.R")
source("utils/plot_functions.R")

# 全局变量
APP_VERSION <- "2.0"
APP_NAME <- "生物数据统计分析平台"

# 全局样式
GLOBAL_STYLES <- HTML("
  /* 全局样式 */
  body {
    font-family: 'Microsoft YaHei', 'Arial', sans-serif;
    font-size: 14px;
    line-height: 1.6;
    color: #495057;
  }
  
  /* 头部样式 */
  .main-header .logo {
    font-weight: bold;
    font-size: 18px;
    color: #fff !important;
    text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
  }
  
  .main-header .navbar {
    background: linear-gradient(90deg, #5C7CFA 0%, #364FC7 100%);
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
  }
  
  /* 侧边栏样式 */
  .main-sidebar {
    background: linear-gradient(180deg, #F8F9FC 0%, #E7EFFF 100%);
    box-shadow: 2px 0 5px rgba(0,0,0,0.05);
  }
  
  .sidebar-menu > li > a {
    color: #495057;
    font-weight: 500;
    transition: all 0.3s ease;
  }
  
  .sidebar-menu > li > a:hover {
    color: #364FC7;
    background-color: rgba(92, 124, 250, 0.1);
  }
  
  .sidebar-menu > li.active > a {
    color: #fff;
    background-color: #5C7CFA;
    border-left: 3px solid #364FC7;
  }
  
  /* 内容区域样式 */
  .content-wrapper {
    background-color: #F8F9FC;
    min-height: calc(100vh - 60px);
  }
  
  /* 盒子样式 */
  .box {
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    border: none;
    margin-bottom: 20px;
    transition: transform 0.3s ease, box-shadow 0.3s ease;
  }
  
  .box:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 15px rgba(0,0,0,0.12);
  }
  
  .box-header {
    font-weight: bold;
    font-size: 16px;
    border-radius: 8px 8px 0 0;
    padding: 15px;
  }
  
  .box-body {
    padding: 20px;
  }
  
  .box.box-primary {
    border-top-color: #5C7CFA;
  }
  
  .box.box-info {
    border-top-color: #339AF0;
  }
  
  .box.box-success {
    border-top-color: #51CF66;
  }
  
  .box.box-warning {
    border-top-color: #FFD43B;
  }
  
  .box.box-danger {
    border-top-color: #FF6B6B;
  }
  
  /* 按钮样式 */
  .btn {
    border-radius: 5px;
    font-weight: 500;
    transition: all 0.3s ease;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .btn:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(0,0,0,0.1);
  }
  
  .btn-primary {
    background-color: #5C7CFA;
    border-color: #364FC7;
  }
  
  .btn-primary:hover {
    background-color: #364FC7;
    border-color: #2E4BC7;
  }
  
  .btn-success {
    background-color: #51CF66;
    border-color: #37B24D;
  }
  
  .btn-success:hover {
    background-color: #008d4c;
    border-color: #006d3d;
  }
  
  .btn-warning {
    background-color: #f39c12;
    border-color: #e08e0b;
  }
  
  .btn-warning:hover {
    background-color: #e08e0b;
    border-color: #d58512;
  }
  
  /* 通知样式 */
  .shiny-notification {
    background: linear-gradient(135deg, #3c8dbc 0%, #367fa9 100%);
    color: white;
    border: none;
    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
    font-size: 16px;
    padding: 15px 20px;
    border-radius: 8px;
    font-weight: 500;
  }
  
  .shiny-notification-error {
    background: linear-gradient(135deg, #dd4b39 0%, #c23321 100%);
    color: white;
    border: none;
    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
    font-size: 16px;
    padding: 15px 20px;
    border-radius: 8px;
    font-weight: 500;
  }
  
  /* 表格样式 */
  .table {
    border-radius: 5px;
    overflow: hidden;
    box-shadow: 0 2px 5px rgba(0,0,0,0.05);
  }
  
  .table > thead > tr > th {
    background-color: #3c8dbc;
    color: white;
    font-weight: 600;
    border: none;
  }
  
  .table > tbody > tr:nth-of-type(odd) {
    background-color: rgba(0,0,0,0.02);
  }
  
  .table > tbody > tr:hover {
    background-color: rgba(60,141,188,0.1);
  }
  
  /* 标签页样式 */
  .nav-tabs {
    border-bottom: 2px solid #3c8dbc;
  }
  
  .nav-tabs > li > a {
    color: #777;
    border: none;
    border-radius: 5px 5px 0 0;
    margin-right: 5px;
    transition: all 0.3s ease;
  }
  
  .nav-tabs > li > a:hover {
    background-color: #f5f5f5;
    color: #3c8dbc;
  }
  
  .nav-tabs > li.active > a {
    background-color: #3c8dbc;
    color: white;
    border: none;
  }
  
  .tab-content {
    padding: 20px;
    background-color: white;
    border-radius: 0 0 5px 5px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.05);
  }
  
  /* 图标样式 */
  .fa {
    margin-right: 5px;
  }
  
  /* 动画效果 */
  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(20px); }
    to { opacity: 1; transform: translateY(0); }
  }
  
  .fade-in {
    animation: fadeIn 0.5s ease-out;
  }
  
  /* 加载动画 */
  .loading-spinner {
    display: inline-block;
    width: 20px;
    height: 20px;
    border: 3px solid rgba(60,141,188,0.3);
    border-radius: 50%;
    border-top-color: #3c8dbc;
    animation: spin 1s ease-in-out infinite;
  }
  
  @keyframes spin {
    to { transform: rotate(360deg); }
  }
")