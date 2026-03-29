library(tidyverse)                                                                                                                  
library(scales)                                                                                                                     

# ── Palette ───────────────────────────────────────────────────────────────────                                                    
BAND_COL    <- "darkblue"                                                                                                            
CURRENT_COL <- "darkred"                                                                                                            

# ── Label size parameters ─────────────────────────────────────────────────────                                                    
SZ_SAA      <- 3   # SAA target label                                                                                             
SZ_CURRENT  <- 3.0   # current allocation label                                                                                     
SZ_BAND     <- 2.8   # min/max band end labels                                                                                      

# ── Data ──────────────────────────────────────────────────────────────────────                                                    
alloc <- tribble(                                                                                                                   
  ~label, ~group,         ~saa, ~lo, ~hi, ~current,                                                                                 
  "MM",   "Asset Class",    5,    0,  30,    6,
  "FI",   "Asset Class",   50,    0,  70,   41,
  "EQ",   "Asset Class",   45,    0,  60,   48,                                                                                     
  "AI",   "Asset Class",    0,    0,  25,    5,
  "EUR",  "Currency",      75,   50, 100,   76,                                                                                     
  "USD",  "Currency",       0,    0,  35,   18,                                                                                     
  "CHF",  "Currency",       0,    0,  25,   NA,
  "GBP",  "Currency",       0,    0,  20,   NA                                                                                      
) %>%                                                                                                                               
  mutate(label = factor(label, levels = c("MM", "FI", "EQ", "AI",                                                                   
                                          "EUR", "USD", "CHF", "GBP")))                                                             

# ── Plot ──────────────────────────────────────────────────────────────────────                                                    
ggplot(alloc, aes(x = label)) +                                                                                                     
  
  # ── Alternating column wash ───────────────────────────────────────────────                                                      
  geom_tile(aes(y = 50, height = 116, width = 0.85),                                                                                
            fill = "grey97", colour = NA, show.legend = FALSE) +                                                                    
  
  # ── Range band ────────────────────────────────────────────────────────────                                                      
  geom_segment(aes(y = lo, yend = hi, xend = label),                                                                                
               linewidth = 6, alpha = 0.20, lineend = "round",                                                                      
               colour = BAND_COL) +
  
  # ── Band end tick marks ───────────────────────────────────────────────────                                                      
  geom_point(aes(y = lo), shape = 95, size = 7, alpha = 0.55, colour = BAND_COL) +                                                  
  geom_point(aes(y = hi), shape = 95, size = 7, alpha = 0.55, colour = BAND_COL) +                                                  
  
  # ── Band end labels ───────────────────────────────────────────────────────                                                      
  geom_text(aes(y = lo, label = paste0(lo, "%")),                                                                                   
            colour = "black", vjust = 1.9, size = SZ_BAND) +                                                                       
  geom_text(aes(y = hi, label = paste0(hi, "%")),
            colour = "grey55", vjust = -0.9, size = SZ_BAND) +                                                                      
  
  # ── SAA target — horizontal line + label inline left ─────────────────────                                                       
  geom_errorbar(aes(ymin = saa, ymax = saa),                                                                                        
                width = 0.45, linewidth = 2.0, colour = BAND_COL) +                                                                 
  geom_text(aes(y = saa, label = paste0(saa, "%")),                                                                                 
            colour = BAND_COL, vjust = 0.5, hjust = 2.8,                                                                            
            size = SZ_SAA, fontface = "bold") +                                                                                     
  
  # ── Current allocation — filled red circle + label inline right ───────────                                                      
  geom_point(data = ~filter(.x, !is.na(current)),                                                                                   
             aes(y = current), shape = 21, size = 6,                                                                                
             fill = CURRENT_COL, colour = "white", stroke = 1.6) +
  geom_text(data = ~filter(.x, !is.na(current)),                                                                                    
            aes(y = current, label = paste0(current, "%")),                                                                         
            colour = CURRENT_COL, vjust = 0.5, hjust = -0.5,                                                                        
            size = SZ_CURRENT, fontface = "bold") +                                                                                 
  
  scale_y_continuous(limits = c(-10, 112), breaks = seq(0, 100, 10),                                                                
                     labels = function(x) paste0(x, "%"),                                                                           
                     expand = c(0, 0)) +                                                                                            
  facet_wrap(~group, scales = "free_x", nrow = 1) +
  
  theme_minimal(base_size = 12) +                                                                                                   
  theme(                                                                                                                            
    panel.grid.major.y = element_line(colour = "darkgrey", linewidth = 0.4),                                                          
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),                                                                                           
    axis.title        = element_blank(),                                                                                            
    axis.text.y       = element_text(size = 8, colour = "grey60"),                                                                  
    axis.text.x       = element_text(face = "bold", size = 11, colour = BAND_COL),                                                  
    strip.text        = element_text(face = "bold", size = 10.5,                                                                    
                                     colour = "white", hjust = 0.02),
    strip.background  = element_rect(fill = BAND_COL, colour = NA),                                                                 
    panel.spacing     = unit(1.8, "lines"),                                                                                         
    plot.title        = element_text(face = "bold", size = 14, colour = BAND_COL),                                                  
    plot.subtitle     = element_text(colour = "grey50", size = 9.5,                                                                 
                                     margin = margin(b = 12)),                                                                      
    plot.background   = element_rect(fill = "white", colour = NA),                                                                  
    plot.margin       = margin(16, 24, 16, 16)                                                                                      
  ) +                                                                                                                               
  labs(
    title    = "Strategic Allocation & Tactical Leeway",                                                                            
    subtitle = "— SAA target  |  ── Min–max band  |  ● Current allocation"                                                          
  )                                                       