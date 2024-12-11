pacman::p_load(pacman, ggplot2, plotly, tidyr, dplyr, htmlwidgets, scales, ggpubr)

setwd("~/Desktop/File/Giardino Punk/Articolo ecologia politica pt.2")

data <- read.csv("Dati land grabbing.csv")
names(data) <- c("Anno","Totale", "Biocarburante", "Biocarburante, fallito")
data <- gather(data, key = `Tipo acquisizione`, value = Ettari_num, -Anno, -`Biocarburante, fallito`)

for(i in 1:length(data$Ettari)){
  data$Ettari[i] <- label_number(scale_cut=cut_short_scale())(data$Ettari_num[i])
}
data$Ettari <- paste(data$Ettari, "ha")

plot <- ggplot(data, aes(x=Anno, y=Ettari_num, col=`Tipo acquisizione`, label = Ettari))+
  geom_line(linejoin = "round", linewidth=1.5)+
  scale_y_log10(breaks=10^(5:7), minor_breaks = rep(1:9, 5)*(10^rep(4:8, each=9)),
                labels=c("100,000", "1 mln", "10 mln"))+
  scale_x_continuous(breaks = seq(from=2015, to=2024, by=2))+
  scale_color_manual(values=c("white","#ef11f8"),
                     breaks = c("Totale", "Biocarburante"))+
  theme(
    panel.background = element_rect(fill="#121212"),
    plot.background = element_rect(fill="#121212", color=NA), #transparent plot bg
    panel.grid.major = element_line(color="grey"),
    panel.grid.minor = element_line(color="grey"),
    legend.background = element_rect(fill="#121212"),
    legend.box.background = element_rect(fill="#121212"),
    legend.text=element_text(color="white", family="Cairo", size = 12),
    legend.title = element_text(color="white", family="Cairo", size=14),
    legend.position = "bottom",
    axis.text = element_text(color="white", family="Cairo", size=10),
    plot.title = element_text(color="white", family="Cairo", size=16),
    axis.title = element_text(color="white", family="Cairo", size=12)
  )+
  xlab("")+
  ylab("Ettari")+
  labs(title = "Acquisizione di terreni su grande scala nel mondo")
  # labs(title="Acquisizione di terreni\nsu grande scala nel mondo")
  
p <- ggplotly(plot, tooltip = c("x","label")) %>%
  layout(plot, autosize=T, legend = list(orientation = "h", title = ""), hovermode="x")

saveWidget(p,"Grafico_1.html")

plotly_json(p)

#Bar plot of percentages - World
Inv_tot <- gather(read.csv("Global - Investment in agriculture.csv"),
                  key="Finalità Investimento", value="Ettari")

Inv_tot$Percentuale <- Inv_tot$Ettari*100/(sum(Inv_tot$Ettari))

Inv_tot$`Finalità Investimento` <- c("Coltura alimentare",
                                     "Biomassa per biocarburanti",
                                     "Allevamento",
                                     "Agricoltura non specificata",
                                     "Coltura non alimentare",
                                     "Mangime",
                                     "Biomassa per energia")
Inv_tot <- Inv_tot[1:6,]

Inv_tot$`Finalità Investimento` <- factor(Inv_tot$`Finalità Investimento`,
                                          levels=c("Coltura alimentare",
                                                   "Biomassa per biocarburanti",
                                                   "Allevamento",
                                                   "Agricoltura non specificata",
                                                   "Coltura non alimentare",
                                                   "Mangime"))
Inv_tot$Ettari <- label_number(accuracy=0.1, scale_cut=scales::cut_short_scale())(Inv_tot$Ettari)

Inv_tot$Ettari <- Inv_tot$Ettari

plot <- ggplot(Inv_tot, aes(x=`Finalità Investimento`, y=Percentuale,
                            fill = `Finalità Investimento`, label=Ettari))+
  geom_col()+
  # geom_text(aes(label=Ettari), vjust=0, color="white", size=6)+
  theme(
    panel.background = element_rect(fill="#121212"),
    plot.background = element_rect(fill="#121212", color=NA), #transparent plot bg
    panel.grid.major = element_line(color="grey"),
    panel.grid.minor = element_line(color="grey"),
    legend.background = element_rect(fill="#121212"),
    legend.box.background = element_rect(fill="#121212"),
    legend.text=element_text(color="white", family="Cairo", size = 14),
    legend.title = element_blank(),
    legend.position="bottom",
    axis.text.y = element_text(color="white", family="Cairo", size=14),
    axis.text.x =element_blank(),
    plot.title = element_text(color="white", family="Cairo", size=20),
    axis.title = element_text(color="white", family="Cairo", size=18))+
  xlab("")+
  ylab("%")+
  scale_x_discrete(breaks = c("Coltura alimentare",
                              "Biomassa per biocarburanti",
                              "Allevamento",
                              "Agricoltura non specificata",
                              "Coltura non alimentare",
                              "Mangime",
                              "Biomassa per energia"))+
  labs(title = "Finalità dell'acquisizione di terreni nel mondo")
  # labs(title="Finalità acquisizioni di terreno - Mondo")
plot <- ggplotly(plot, tooltip = c("x","y","Ettari"))
plot <- layout(plot, autosize=T, yaxis = list(hoverformat = ".2f"),
               legend = list(orientation = "h", title = ""))

saveWidget(plot,"Grafico_2.html")

plotly_json(ggplotly(plot, tooltip = c("x","y","Ettari"))%>%
              layout(plot, autosize=T, legend = list(orientation = "h", title = "")))

#Bar plot of percentages - Italy
Inv_it <- gather(read.csv("Italy - Investment in agriculture.csv"),
                  key="Finalità Investimento", value="Ettari")

Inv_it$Percentuale <- Inv_it$Ettari*100/(sum(Inv_it$Ettari))

Inv_it <- Inv_it[c(2,1,5,3,4,7,6),]

Inv_it$`Finalità Investimento` <- c("Coltura alimentare",
                                    "Biomassa per biocarburanti",
                                    "Allevamento",
                                    "Agricoltura non specificata",
                                    "Coltura non alimentare",
                                    "Mangime",
                                    "Biomassa per energia")
Inv_it <- Inv_it[1:6,]

Inv_it$`Finalità Investimento` <- factor(Inv_tot$`Finalità Investimento`,
                                          levels=c("Coltura alimentare",
                                                   "Biomassa per biocarburanti",
                                                   "Allevamento",
                                                   "Agricoltura non specificata",
                                                   "Coltura non alimentare",
                                                   "Mangime"))


for(i in 1:6){
  Inv_it$Ettari[i] <- label_number(accuracy=0.1, scale_cut=scales::cut_short_scale())(as.numeric(Inv_it$Ettari[i]))
}
Inv_it$Ettari <- paste(Inv_it$Ettari, "ha")

plot2 <- ggplot(Inv_it, aes(x=`Finalità Investimento`, y=Percentuale,
                            fill = `Finalità Investimento`, label=Ettari))+
  geom_col()+
  # geom_text(aes(label=Ettari), vjust=0, color="white", size=6)+
  theme(
    panel.background = element_rect(fill="#121212"),
    plot.background = element_rect(fill="#121212", color=NA), #transparent plot bg
    panel.grid.major = element_line(color="grey"),
    panel.grid.minor = element_line(color="grey"),
    legend.background = element_rect(fill="#121212"),
    legend.box.background = element_rect(fill="#121212"),
    legend.text=element_text(color="white", family="Cairo", size = 14),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(color="white", family="Cairo", size=14),
    axis.text.x =element_blank(),
    plot.title = element_text(color="white", family="Cairo", size=20),
    axis.title = element_text(color="white", family="Cairo", size=18))+
  xlab("")+
  ylab("%")+
  scale_fill_discrete()+
  labs(title = "Finalità dell'acquisizione di terreni nel mondo - Aziende italiane")
  # labs(title="Finalità acquisizioni di terreno - Aziende italiane")

plot2 <- ggplotly(plot2, tooltip = c("x","y","Ettari"))
plot2 <- layout(plot2, autosize=T, yaxis = list(hoverformat = ".2f"),
               legend = list(orientation = "h", title = ""))

saveWidget(plot2,"Grafico_3.html")

plotly_json(ggplotly(plot2, tooltip = c("x","y","Ettari"))%>%
              layout(widget, autosize=T, legend = list(orientation = "h", title = "")))
