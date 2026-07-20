library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)


df<-data[,1:5]
# upload data
colnames(df) <- c(
  "Datetime",
  "T1","VPD1",
  "T2","VPD2"
)


# calculate VPD
df <- df %>%
  mutate(
    date = as.Date(Datetime)
  )

# unique days
unique_days <- unique(df$date)

### Figure 2----
for (d in unique_days) {
  
  df_day <- df %>% filter(date == d)
  
  p <- ggplot(df_day, aes(x = Datetime)) +
    
    # TEMPERATURA (principal)
    geom_line(aes(y = T1), color = "black", linewidth = 0.9) +
    geom_line(aes(y = T2), color = "grey60", linewidth = 0.9) +
    
    # VPD (secundari, dashed)
    geom_line(aes(y = VPD1 * 5), color = "darkred", linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = VPD2 * 5), color = "orange", linetype = "dashed", linewidth = 0.8) +
    
    # escala dual
    scale_y_continuous(
      limits = c(0, 42), 
      name = "Temperature (°C)",
      sec.axis = sec_axis(~./5, name = "VPD (kPa)")
    ) +
    
    # tema científic net
    theme_classic(base_size = 13) +
    
    labs(
      x = NULL,
      y = NULL
    ) +
    
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      legend.position = "none",
      
      axis.line = element_line(size = 0.6),
      axis.ticks = element_line(size = 0.6)
    )
  
  # save file
  ggsave(
    filename = paste0("HW_", d, ".png"),
    plot = p,
    width = 8,
    height = 4,
    dpi = 300
  )
}



###Figure 3----
library(ggplot2)
library(tidyr)
library(dplyr)

# transformar a format llarg
df_long <- data %>%
  pivot_longer(cols = c(Aemet, Aemet_dif, P90, P95),
               names_to = "variable",
               values_to = "temp")

library(ggplot2)
library(tidyr)
library(dplyr)

df_long <- df %>%
  pivot_longer(cols = c(t_aemet, t_aemet_diff, p90, p95),
               names_to = "variable",
               values_to = "temp")

m<-ggplot(df_long, aes(x = Dia, y = temp)) +
  
  # dades
  geom_line(aes(color = variable, linetype = variable), size = 0.9) +
  
  # colors més sobris
  scale_color_manual(values = c(
    "Aemet" = "black",
    "Aemet_dif" = "firebrick",
    "P90" = "grey50",
    "P95" = "grey30"
  ),
  labels = c(
    "Aemet" = "AEMET",
    "Aemet_dif" = "AEMET + ΔT",
    "P90" = "P90",
    "P95" = "P95"
  )) +
  
  # línies
  scale_linetype_manual(values = c(
    "Aemet" = "solid",
    "Aemet_dif" = "solid",
    "P90" = "dashed",
    "P95" = "dotted"
  )) +
  scale_y_continuous(
    limits = c(0, 42))+
  scale_x_continuous(
    limits = c(12, 25),        # start i end
    breaks = 12:25             # un tick per cada dia
  )+
  labs(
    x = "Day of May",
    y = "Absolute temperature maxima (°C)",
    color = NULL,
    linetype = NULL
  ) +
  
  # tema científic net
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "none",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.5, "cm"),
    
    axis.line = element_line(size = 0.6),
    axis.ticks = element_line(size = 0.6),
    
    panel.grid = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

ggsave(
  filename = "max.png",
  plot = m,
  width = 8,
  height = 4,
  dpi = 300
)

###Means and t-test----
library(tidyr)
library(dplyr)
data1<-data%>%
  mutate(Port1_diff=Port1_temp-Port2_temp)%>%
  mutate(Port2_diff=Port1_temp-Port2_temp)

data2<-data1 %>%
  filter(between(hora,0,6))%>%
  group_by(dia)%>%
  summarise(
    across(
      c(Port1_temp, Port1_VPD, Port2_temp, Port2_VPD, Port1_diff, Port2_diff),
      list(
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        mean = ~mean(.x, na.rm = TRUE),
        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    )
  )%>%
  pivot_longer(
    -dia,
    names_to = c("Port", "variable", "stat"),
    names_pattern = "(Port1|Port2)?_(temp|VPD|diff)_?(min|max|mean|se)"
  ) %>%
  mutate(Day= paste0("dia_", dia))


data2 %>%
  filter(variable=="VPD",stat == "min") %>%
  tidyr::pivot_wider(names_from = Port, values_from = value) %>%
  with(t.test(Port1, Port2, paired = TRUE))


data3<-data2%>%
  pivot_wider(names_from = stat, values_from = value)
write.csv2(data3,"data.csv")
