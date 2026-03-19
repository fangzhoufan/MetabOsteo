library(dplyr)
library(ggplot2)
library(stringr)

draw_category_plot <- function(cat_name, full_data) {
  
  # --- 1. 数据清洗与预处理 ---
  plot_subdata <- full_data %>%
    dplyr::filter(Major_Category == cat_name) %>%
    
    # 清洗 Outcome 名称
    mutate(outcome_clean = str_replace(outcome, "^[A-Z0-9]+_", "")) %>%
    mutate(outcome_clean = str_to_sentence(outcome_clean)) %>%
    mutate(outcome_clean=outcome.detail) |>
    
    # 生成代谢分组
    mutate(metabolic_type = case_when(
      exposure %in% c("HMGCR", "NPC1L1","PCSK9", "APOB",
                      "PPARA"  ,   "ANGPTL3"  , "APOC3",'CTEP') ~ "Lipid-lowering drugs",
      exposure %in% c('SLC5A2','GLP1R','KCNJ11','ABCC8', 'PPARG') ~ "Glucose-lowering drugs",
      exposure %in% c('CACNA1C ' , 'CACNA1D ' , 'CACNB2 ' , 'SLC12A1') ~ "Antihypertensive drugs",
      TRUE ~ "Other"
    )) %>%
    mutate(metabolic_type = factor(metabolic_type, levels = c("Lipid-lowering drugs", "Glucose-lowering drugs", "Antihypertensive drugs"))) %>%
    
    # 确保 Tier 顺序
    mutate(Tier = factor(Tier, levels = rev(c("Tier 4", "Tier 3", "Tier 2", "Tier 1", "Tier 1+"))),
           exposure= factor(exposure,levels=c("HMGCR", "NPC1L1", "PCSK9", "APOB", "PPARA", "ANGPTL3", "APOC3", "CTEP",
                                     "SLC5A2", "GLP1R", "KCNJ11", "ABCC8", "PPARG", 
                                     "CACNA1C ", "CACNA1D ", "CACNB2 ", "SLC12A1"))) %>%
    
    # 构建填充组
    mutate(fill_group = case_when(
      effect_class == "Not-Sig" ~ "Not-Sig",
      TRUE ~ paste(effect_class, Tier, sep = ".")
    ))
  
  # 如果没有数据，返回NULL
  if(nrow(plot_subdata) == 0) return(NULL)
  
  # 特殊字符修正
  plot_subdata$outcome_clean[plot_subdata$outcome_clean=='Chd'] <- 'CHD' 
  plot_subdata$outcome_clean[plot_subdata$outcome_clean=='T2dm'] <- 'T2DM' 
  plot_subdata$outcome_clean[plot_subdata$outcome_clean=='Hypertension (FinnGen)'] <- 'Hypertension' 
  
  # --- [关键修复 1]：强制将 effect_class 转换为包含所有3个水平的因子 ---
  # 顺序必须固定：Negative -> Not-Sig -> Positive (或者您喜欢的其他固定顺序)
  # 这里的顺序决定了 override.aes 中颜色的对应顺序
  plot_subdata$Association <- factor(plot_subdata$effect_class, 
                                     levels = c("Negative", "Not-Sig", "Positive"))
  
  # --- 2. 定义颜色映射 ---
  cols_fill <- c(
    "Positive.Tier 1+" = "#ED0000FF", "Positive.Tier 1"  = "#ED0000CC",
    "Positive.Tier 2"  = "#ED000080", "Positive.Tier 3"  = "#ED000040",
    "Positive.Tier 4"  = "#ED000020",
    
    "Negative.Tier 1+" = "#00468BFF", "Negative.Tier 1"  = "#00468BCC", 
    "Negative.Tier 2"  = "#00468B80", "Negative.Tier 3"  = "#00468B40",
    "Negative.Tier 4"  = "#00468B20",
    
    "Not-Sig"          = "white"
  )
  
  # --- 3. 绘图 ---
  p <- ggplot(plot_subdata, aes(x = exposure, y = outcome_clean)) +
    
    geom_point(aes(
      fill = fill_group,   
      color = Association, # 映射到我们修复后的因子
      shape = Tier,        
      size = Tier          
    ), stroke = 0.6) +     
    
    scale_fill_manual(values = cols_fill, guide = "none") +
    
    # --- [关键修复 2]：在 scale_color_manual 中设置 drop = FALSE ---
    scale_color_manual(
      # 确保这里包含所有 levels 的值
      values = c(
        "Negative" = "black", 
        "Not-Sig" = "grey50", 
        "Positive" = "black"
      ),
      # drop = FALSE 强制图例显示所有3个分类，即使数据中缺少某一种
      drop = FALSE, 
      guide = guide_legend(
        override.aes = list(
          # 这里的顺序必须严格对应上面的 factor levels (Negative, Not-Sig, Positive)
          color = c("#00468BFF", "grey50", "#ED0000FF"),  
          shape = c(16, 16, 16) 
        )
      )
    ) +
    
    scale_shape_manual(values = c(
      "Tier 1+" = 23, "Tier 1" = 24, "Tier 2" = 21, 
      "Tier 3" = 22, "Tier 4" = 21
    )) +
    
    scale_size_manual(values = c(
      "Tier 1+" =6, "Tier 1" = 5, "Tier 2" = 4.5, 
      "Tier 3" = 3, "Tier 4" = 1
    )) +
    
    geom_text(data = dplyr::filter(plot_subdata, Tier == "Tier 1+"),
              aes(label = "*"), 
              color = "black", size = 3, vjust = 0.7) +
    
    facet_grid(Sub_Category ~ metabolic_type,
               scales = 'free',
               space = 'free') + 
    
    labs(title = cat_name, x = NULL, y = NULL,color = 'Association') +
    
    theme_bw() +
    ggplot_themes(text_size = 13) + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12,color = "black"),
      axis.text.y = element_text(size = 12,color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y = element_blank()
    ) 
  
  return(p)
}