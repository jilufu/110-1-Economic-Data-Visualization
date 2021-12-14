
library(readxl)
data_graph <- read_excel("work/goods.xlsx")
View(data_graph)



data_graph$group <- factor(data_graph$group,levels = c("第一季","第二季", "第三季", "第四季"))

data_graph <- left_join(data_graph,
  data_graph %>% 
    group_by(case) %>%
    summarize(nb_total = sum(nb))) %>%
  group_by(case) %>%
  mutate(nb_frac = 2*pi*cumsum(nb)/nb_total,
         start = lag(nb_frac, default = 0))



data_labels <- data_graph %>% 
  group_by(case) %>%
  summarize(x = x[1], y = y[1], nb_total = nb_total[1])

scale = .5/sqrt(max(data_graph$nb_total))

data_percent <- data_graph %>% mutate(
  percent=paste0(round(nb/nb_total*100),"%")
)
  



ggplot(data_graph) + 
  geom_arc_bar(aes(x0 = x, y0 = y, r0 = 0, r = sqrt(nb_total)*scale,
                   start = start, end = nb_frac, fill = group)) +
  geom_text(data = data_labels,
            aes(label = case, x = x, y = y + scale*sqrt(nb_total) + .1),
            size =11/.pt, 
            ) +
  geom_text(data = data_percent,
            aes(label = percent, x = x, y = y+c(0.3,0.1,-0.1,-0.3)),
            size =11/.pt, )+
  coord_fixed() +
  scale_fill_brewer(
    name="",
    labels = levels(data_graph$group),
    breaks = levels(data_graph$group),
    palette = "RdBu"
    )+
  labs( title="2020年綜合商品零售業及電商銷售資訊",
        subtitle="全年營業額大小(圓餅圖大小)及各季占比%",
        caption="Source:https://www.moea.gov.tw/MNS/dos/home/Home.aspx",
        x="",
        y=""
  )+
  theme(  plot.title =element_text( size=17 ),
          plot.subtitle =element_text( size=12 ),
          text = element_text( size=8 )  ,
          axis.text=element_blank(),
          axis.line=element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top"
          
  ) ->plot0







