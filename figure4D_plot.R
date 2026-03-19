library(dplyr)
library(ggplot2)
library(stringr)

#' @title 绘制药物靶点与骨科亚类关联气泡图
#' @param df 原始结果数据框 (final_Tier_drug_EUR)
#' @return ggplot 对象
plot_drug_orthopedic_bubble <- function(df) {
  
  # ============================================================================
  # 1. 基础设置与字典定义
  # ============================================================================
  
  # 药物靶点顺序
  drug_traits <- c(
    "HMGCR", "NPC1L1", "PCSK9", "APOB", "PPARA", "ANGPTL3", "APOC3", "CTEP",
    "SLC5A2", "GLP1R", "KCNJ11", "ABCC8", "PPARG", 
    "CACNA1C ", "CACNA1D ", "CACNB2 ", "SLC12A1"
  )
  
  # Sub_Category 的标准顺序 (ICD10 Order)
  # 请根据您的实际数据调整这个列表，确保包含所有出现的 Sub_Category
  icd10_order <- c(
    "Infectious arthropathies (M00-M03)", "Inflammatory polyarthropathies (M05-M14)", 
    "Osteoarthritis (M15-M19)", "Other joint disorders (M20-M25)",
    "Systemic connective tissue disorders (M30-M36)",
    "Deforming dorsopathies (M40-M43)", "Spondylopathies (M45-M49)", 
    "Other dorsopathies (M50-M54)", "Disorders of muscles (M60-M63)",
    "Disorders of synovium and tendon (M65-M68)", "Other soft tissue disorders (M70-M79)",
    "Disorders of bone density and structure (M80-M85)", "Other osteopathies (M86-M90)", 
    "Chondropathies (M91-M94)",
    "Other musculoskeletal disorders (M95-M99)",
    "CHD",'T2DM','Hypertension'
  )
  
  # Major_Category 的映射 (用于分面标题)
  major_cat_levels <- c(
    "Arthropathies (M00-M25)",
    "Systemic connective tissue disorders (M30-M36)", 
    "Dorsopathies (M40-M54)",
    "Soft tissue disorders (M60-M79)",
    "Osteopathies and chondropathies (M80-M94)",
    "Other musculoskeletal disorders (M95-M99)",
    "Positive Control"
  )
  
  # ============================================================================
  # 2. 数据汇总
  # ============================================================================
  
  bubble_data <- df %>%
    
    # 筛选显著结果 (Tier 1+, 1, 2, 3) -> 排除 Not-Sig
    filter(effect_class != "Not-Sig") %>%
    
    # 按 Exposure 和 Sub_Category 分组统计
    group_by(exposure, Sub_Category) %>%
    summarise(
      Positive = sum(effect_class == "Positive"),
      Negative = sum(effect_class == "Negative"),
      total = n(),
      .groups = 'drop'
    ) %>%
    
    # 过滤掉没有任何显著结果的组合
    filter(total > 0) %>%
    
    # 计算阳性比例
    mutate(
      proportion_positive = Positive / total
    )
  
  # ============================================================================
  # 3. 添加分面与排序信息
  # ============================================================================
  
  plot_ready <- bubble_data %>%
    # (1) 映射 ICD10 Main Category (列分面)
    # 这里我们简单地利用 Sub_Category 的前缀或您的映射逻辑
    # 为了简化，我使用 grep 模糊匹配，您可以换回您原来的 case_when 精确匹配
    mutate(icd10_main = case_when(
      grepl("M00-M03|M05-M14|M15-M19|M20-M25", Sub_Category) ~ "Arthropathies\n(M00-M25)",
      grepl("M30-M36", Sub_Category) ~ "Systemic\nconnective\ntissue\ndisorders",
      grepl("M40-M43|M45-M49|M50-M54", Sub_Category) ~ "Dorsopathies\n(M40-M54)",
      grepl("M60-M63|M65-M68|M70-M79", Sub_Category) ~ "Soft tissue\ndisorders (M60-M79)",
      grepl("M80-M85|M86-M90|M91-M94", Sub_Category) ~ "Osteopathies and\nchondropathies",
      grepl("M95-M99", Sub_Category) ~ "Other\nmusculo-\nskeletal\ndisorders",
      TRUE ~ "Positive Control"
    )) %>%
    # 强制设定 icd10_main 的因子顺序 (控制列的左右顺序)
    mutate(icd10_main = factor(icd10_main, levels = c(
      "Arthropathies\n(M00-M25)", "Systemic\nconnective\ntissue\ndisorders", "Dorsopathies\n(M40-M54)", 
      "Soft tissue\ndisorders (M60-M79)", "Osteopathies and\nchondropathies", "Other\nmusculo-\nskeletal\ndisorders", "Positive Control"
    ))) %>%
    
    # (2) 映射 Metabolic Type (行分面)
    mutate(metabolic_type = case_when(
      exposure %in% c("HMGCR", "NPC1L1", "PCSK9", "APOB", "PPARA", "ANGPTL3", "APOC3", "CTEP") ~ "Lipid-lowering",
      exposure %in% c("SLC5A2", "GLP1R", "KCNJ11", "ABCC8", "PPARG") ~ "Glucose-lowering",
      exposure %in% c("CACNA1C ", "CACNA1D ", "CACNB2 ", "SLC12A1") ~ "Antihypertensive",
      TRUE ~ "Other"
    )) %>%
    # 强制设定行顺序
    mutate(metabolic_type = factor(metabolic_type, levels = c("Lipid-lowering", "Glucose-lowering", "Antihypertensive"))) %>%
    
    # (3) 设定 Exposure 顺序 (Y轴)
    # rev() 是因为 ggplot Y轴是从下往上画的，我们希望 HMGCR 在最上面
    mutate(exposure = factor(exposure, levels = rev(drug_traits))) %>%
    
    # (4) 设定 Sub_Category 顺序 (X轴) 并处理换行
    mutate(
      Sub_Category_Label = str_replace(Sub_Category, " \\(", "\n\\("), # 在括号前换行
      # 确保因子顺序正确，否则图会乱
      Sub_Category_Label = factor(Sub_Category_Label, levels = str_replace(icd10_order, " \\(", "\n\\("))
    )
  
  # ============================================================================
  # 4. 绘图
  # ============================================================================
  
  p <- ggplot(plot_ready, aes(x = Sub_Category_Label, y = exposure)) +
    
    # 气泡图层
    geom_point(aes(
      size = total,               # 大小代表显著关联的数量
      fill = proportion_positive, # 填充色代表阳性比例
      color = proportion_positive # 边框色同填充色 (避免黑色边框太重)
    ), shape = 21, stroke = 0.5) + # shape 21 支持 fill 和 color
    
    # 大小映射
    scale_size_continuous(
      range = c(2, 11), 
      name = "Total Significant Pairs",
      breaks = c(1, 5, 10, 20)
    ) +
    
    # 颜色映射 (红蓝渐变)
    scale_fill_gradient2(
      low = "#00468BFF", mid = "grey90", high = "#ED0000FF", 
      midpoint = 0.5, name = "Proportion Positive", limits = c(0, 1)
    ) +
    scale_color_gradient2(
      low = "#00468BFF", mid = "grey90", high = "#ED0000FF", 
      midpoint = 0.5, guide = "none", limits = c(0, 1)
    ) +
    
    # 分面 (核心布局)
    # 行：药物类型，列：疾病大类
    facet_grid(metabolic_type ~ icd10_main, scales = "free", space = "free") +
    
    # 标签与主题
    labs(x = "", y = "", title = "Significant Associations by Drug Targets and Orthopedic Sub-Category") +
    theme_bw() +ggplot_themes(text_size = 10)+
    theme(
      # 分面标题
      strip.background = element_rect(fill = "grey95", color = 'black'),
      strip.text = element_text(size = 9.5,color = "black"),
      
      # 坐标轴文字
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5),
      axis.text.y = element_text(size = 10, color = "black"),
      
      # 网格线
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_blank(),
      
      # 图例
      legend.position = "right"
    ) + 
    theme(
      strip.background = element_rect(fill = "white")
      # 不再需要设置右侧边距了
    )
  
  return(p)
}