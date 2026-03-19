library(dplyr)
library(ggplot2)
library(stringr)

plot_iv_source_overlap<- function(df) {
  
  # --- 1. 数据准备 (与之前一致) ---
  lipid_group <- c("LDLC", "TG", "TC", "HDLC", "Hyperlipidemia")
  glycemic_group <- c("HbA1c", "Glucose", "T2DM", "T1DM")
  bp_group <- c("SBP", "DBP", "Hypertension")
  exposure_levels <- c(lipid_group, glycemic_group, bp_group)
  
  plot_data <- df %>%
    mutate(Source_Simple = case_when(
      str_detect(source, regex("Meta", ignore_case = TRUE)) ~ "Meta",
      str_detect(source, regex("Ukb|UK Biobank", ignore_case = TRUE)) ~ "Ukb",
      TRUE ~ "Other"
    )) %>%
    group_by(Exposure, SNP) %>%
    summarise(
      Source_Combined = paste(sort(unique(Source_Simple)), collapse = "+"),
      .groups = "drop"
    ) %>%
    mutate(Category = case_when(
      str_detect(Source_Combined, "Meta") & str_detect(Source_Combined, "Ukb") ~ "Shared",
      Source_Combined == "Meta" ~ "Meta unique",
      Source_Combined == "Ukb" ~ "Ukb unique",
      TRUE ~ "Other"
    )) %>%
    group_by(Exposure, Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Group = case_when(
      Exposure %in% lipid_group ~ "Lipid Traits",
      Exposure %in% glycemic_group ~ "Glycemic Traits",
      Exposure %in% bp_group ~ "Blood Pressure",
      TRUE ~ "Other"
    )) %>%
    filter(Group != "Other") %>%
    mutate(Group = factor(Group, levels = c("Lipid Traits", "Glycemic Traits", "Blood Pressure"))) %>%
    mutate(Exposure = factor(Exposure, levels = exposure_levels)) %>%
    mutate(Category = factor(Category, levels = c("Meta unique", "Ukb unique", "Shared")))
  
  # --- 2. 绘图 (关键修改在 facet 部分) ---
  p <- ggplot(plot_data, aes(x = Exposure, y = Count, fill = Category)) +
    
    # 柱状图
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    
    # 数字标签
    geom_text(aes(label = Count), 
              position = position_dodge(width = 0.8), 
              vjust = -0.5, size = 3, family='Times New Roman') +
    
    # *** 关键修改：竖向分面 (3行1列) ***
    # ncol = 1: 强制只有一列，所以它们会竖着排
    # scales = "free": 让每个面板的 X 轴只显示属于自己的暴露，Y 轴高度也自适应
    facet_wrap(~ Group, ncol = 1, scales = "free") +
    
    # 颜色
    scale_fill_manual(values = c("Meta unique" = "#ED0000FF", 
                                 "Ukb unique" = "#00468BFF", 
                                 "Shared" = "#45C479")) +
    
    # 扩展Y轴 (防止标签被切)
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    
    theme_bw() +
    my_theme+
    labs(
         x = "", y = "Number of SNPs") +
    theme(
      # 分面标题样式
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = 11, color = "black"),
      
      # 坐标轴文字
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, color = "black"), # 竖排时X轴不需要倾斜了
      axis.text.y = element_text(size = 10, color = "black"),
      
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      
      # 调整面板间距 (让三行之间稍微分开一点，不那么挤)
      panel.spacing = unit(1, "lines") 
    )
  
  return(p)
}
