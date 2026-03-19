library(dplyr)
library(ggplot2)
library(stringr)

draw_drug_target_plot <- function(df) {
  
  # ============================================================================
  # 1. 定义药物靶点与药物大类的映射关系 (根据您的样图)
  # ============================================================================
  # 您可以根据实际数据补充这个列表
  
  lipid_targets <- c("HMGCR", "NPC1L1","PCSK9", "APOB",
                     "PPARA"  ,   "ANGPTL3"  , "APOC3",'CTEP')
  glucose_targets <- c('SLC5A2','GLP1R','KCNJ11','ABCC8', 'PPARG')
  bp_targets <- c('CACNA1C ' , 'CACNA1D ' , 'CACNB2 ' , 'SLC12A1')
  
  # 定义因子顺序 (控制X轴排列)
  target_levels <- c(lipid_targets, glucose_targets, bp_targets)
  
  # ============================================================================
  # 2. 数据处理：计算 Shared / Unique
  # ============================================================================
  
  plot_data <- df %>%
    # (1) 映射药物大类
    mutate(Drug_Class = case_when(
      Target %in% lipid_targets ~ "Lipid-lowering drugs",
      Target %in% glucose_targets ~ "Glucose-lowering drugs",
      Target %in% bp_targets ~ "Antihypertensive drugs",
      TRUE ~ "Other" # 防止有未定义的靶点
    )) %>%
    filter(Drug_Class != "Other") %>%
    
    # (2) 简化 Source 标签
    mutate(Source_Simple = case_when(
      str_detect(source, regex("Meta", ignore_case = TRUE)) ~ "Meta",
      str_detect(source, regex("Ukb", ignore_case = TRUE)) ~ "Ukb",
      TRUE ~ "Other"
    )) %>%
    
    # (3) 核心逻辑：判断每个 SNP 的归属 (按 Target 和 SNP 分组)
    group_by(Drug_Class, Target, SNP) %>%
    summarise(
      Source_Combined = paste(sort(unique(Source_Simple)), collapse = "+"),
      .groups = "drop"
    ) %>%
    
    # (4) 生成分类标签 (Meta unique / Ukb unique / Shared)
    mutate(Category = case_when(
      str_detect(Source_Combined, "Meta") & str_detect(Source_Combined, "Ukb") ~ "Shared",
      Source_Combined == "Meta" ~ "Meta unique",
      Source_Combined == "Ukb" ~ "Ukb unique",
      TRUE ~ "Other"
    )) %>%
    
    # (5) 统计数量
    group_by(Drug_Class, Target, Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    
    # (6) 设定因子顺序
    mutate(Drug_Class = factor(Drug_Class, levels = c("Lipid-lowering drugs", "Glucose-lowering drugs", "Antihypertensive drugs"))) %>%
    mutate(Category = factor(Category, levels = c("Meta unique", "Ukb unique", "Shared"))) %>%
    mutate(Target = factor(Target, levels = target_levels)) # 保持特定顺序
  
  # ============================================================================
  # 3. 绘图
  # ============================================================================
  
  p <- ggplot(plot_data, aes(x = Target, y = Count, fill = Category)) +
    
    # 柱状图 (并排显示)
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    
    # 数字标签 (显示在柱子上方)
    geom_text(aes(label = Count), 
              position = position_dodge(width = 0.8), 
              vjust = -0.5, size = 3,family = "Times New Roman") +
    
    # 分面展示 (按药物大类，X轴比例自由)
    facet_grid(~ Drug_Class, scales = "free_x", space = "free_x") +
    
    # 颜色设置 (复刻样图颜色)
    scale_fill_manual(values = c("Meta unique" = "#ED0000FF", # 红
                                 "Ukb unique" = "#00468BFF",  # 蓝
                                 "Shared" = "#45C479"),     # 绿
                      name = "SNP Category") +
    
    # 坐标轴扩展 (防止数字被切)
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    
    # 标签
    labs(x = "", y = "SNP Numbers") +
    
    # 主题设置
    theme_bw() +
    my_theme+
    theme(
      # 图例位置
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      
      # 分面标题
      strip.background = element_rect(fill = NA, color = NA), # 透明背景
      strip.text = element_text(size = 12,color = "black"), # 黑色大字
      
      # 坐标轴
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12),
      
      # 网格线 (只留横向，淡灰色)
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey92"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey50")
    )
  
  return(p)
}

