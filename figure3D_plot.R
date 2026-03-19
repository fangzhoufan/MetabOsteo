library(dplyr)
library(ggplot2)
library(scatterpie)
library(tidyr)
library(stringr)
# 注意：不再需要 cowplot 包了

plot_simple_scatterpie <- function(df, radius_scale = 0.1) { # 默认用您刚才设定的小半径
  
  # --- 1. 定义内置的严格顺序 (保持不变) ---
  lipid_group <- c("LDLC", "TG", "TC", "HDLC", "Hyperlipidemia")
  glycemic_group <- c("HbA1c", "Glucose", "T2DM", "T1DM")
  bp_group <- c("SBP", "DBP", "Hypertension")
  exposure_levels <- c(lipid_group, glycemic_group, bp_group)
  
  # Y轴顺序：M00在最上面，Positive Control在最下面
  category_levels <- c(
    "Positive Control", 
    "Other musculoskeletal disorders (M95-M99)",
    "Osteopathies and chondropathies (M80-M94)",
    "Soft tissue disorders (M60-M79)",
    "Dorsopathies (M40-M54)",
    "Systemic connective tissue disorders (M30-M36)",
    "Arthropathies (M00-M25)"
  )
  
  # --- 2. 数据清洗与整理 (保持不变) ---
  plot_data <- df %>%
    filter(!Tier %in% c("Tier 4", "Tier 4 "), effect_class != "Not-Sig") %>%
    mutate(effect_class = str_to_title(effect_class)) %>%
    group_by(exposure, Major_Category, effect_class) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = effect_class, values_from = Count, values_fill = 0) %>%
    mutate(Total_Signals = Positive + Negative) %>%
    mutate(Group = case_when(
      exposure %in% lipid_group ~ "Lipid Traits",
      exposure %in% glycemic_group ~ "Glycemic Traits",
      exposure %in% bp_group ~ "Blood Pressure",
      TRUE ~ "Other"
    )) %>%
    # 控制分面顺序
    mutate(Group = factor(Group, levels = c("Lipid Traits", "Glycemic Traits", "Blood Pressure")))
  
  # 应用排序
  plot_data$exposure <- factor(plot_data$exposure, levels = exposure_levels)
  # 补全可能缺失的类别以防报错
  current_cats <- unique(as.character(plot_data$Major_Category))
  final_cat_levels <- category_levels[category_levels %in% current_cats]
  # 如果有预定义列表之外的新类别，加到后面
  extra_cats <- current_cats[!current_cats %in% category_levels]
  plot_data$Major_Category <- factor(plot_data$Major_Category, levels = c(final_cat_levels, extra_cats))
  
  # 生成数字坐标
  plot_data <- plot_data %>%
    mutate(x_num = as.numeric(exposure),
           y_num = as.numeric(Major_Category))
  
  # --- 3. 绘制主图 (已优化标题和布局) ---
  p_main <- ggplot() +
    geom_scatterpie(data = plot_data, 
                    aes(x = x_num, y = y_num, group = interaction(x_num, y_num), 
                        r = sqrt(Total_Signals) * radius_scale), 
                    cols = c("Positive", "Negative"), 
                    color = NA, alpha = 0.9) +
    facet_grid(~Group, scales = "free_x", space = "free_x") +
    scale_x_continuous(breaks = 1:length(exposure_levels), labels = exposure_levels) +
    scale_y_continuous(
      breaks = 1:length(levels(plot_data$Major_Category)), 
      # 使用 str_replace 把 " (" 替换为 "\n("，实现换行
      labels = stringr::str_replace(levels(plot_data$Major_Category), " \\(", "\n\\(")
    )+
    scale_fill_manual(values = c("Positive" = "#ED0000FF", "Negative" = "#00468BFF"), 
                      name = "Effect Direction") +
    theme_bw() + my_theme+
    # --- 关键修改：更新标题，不再提大小代表数量 ---
    labs(x = "", y = "", 
         title = "Distribution of Significant Causal Associations (Cardiometabolic Traits → Orthopedic Phenotypes)") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      strip.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey90"), 
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white")
      # 不再需要设置右侧边距了
    )
  
  # 直接返回主图
  return(p_main)
}
