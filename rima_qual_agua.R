library(ggplot2)
library(reshape2)

setwd("C:/Users/ferna/Desktop/TEMP/")
dados <- read.csv("aguaSuperficial.csv", header = TRUE, sep = ";", dec = ",")

normalizar <- function(x){
  round((x-min(x))/(max(x)-min(x)), 4)
}

dadosNORM <- apply(dados[,2:11], 2, normalizar)
dadosNORM <- as.data.frame(dadosNORM)
pontos <- as.character(dados[,1])
dadosNORM <- data.frame(pontos, dadosNORM)

names(dadosNORM) <- c("Pontos Amostrados", "Ferro", "Manganês", 
                      "Acidez", "Condutividade", "SDT", "ST", 
                      "Sulfato", "Turbidez", "pH", "OD")

mapaCalor <- melt(dadosNORM, id.vars = c("Pontos Amostrados"))
#mapaCalor$variable <- factor(as.character(mapaCalor$variable),
#                             levels=rev(sort(levels(mapaCalor$variable))))

options(OutDec = ",")

# grafico <- ggplot(mapaCalor, aes(x = `Pontos Amostrados`, y = variable, fill = value))+
#   geom_tile(color = "white", size = 0.25)+
#   scale_fill_viridis_c(
#     option = "C", begin = 0.0000, end = 1.000,
#     limits = c(0, 1),
#     name = "Escala Relativa\nde Valores Máximos\ne Mínimos",
#     guide = guide_colorbar(
#       direction = "horizontal",
#       label.position = "bottom",
#       title.position = "top",
#       ticks = FALSE))+
#   xlab("Pontos Amostrados")+ylab("Parâmetros")+
#   geom_text(aes(label = round(value, 2)), color="white", size=3, fontface = "bold")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scale Green, Yellow, Red
grafico <- ggplot(mapaCalor, aes(x = `Pontos Amostrados`, y = variable, fill = value))+
  geom_tile(color = "white", size = 0.25)+
  scale_color_gradient2(
    low = "green", mid = 'yellow', high = 'red', midpoint = 0.5,
    limits = c(0, 1), aesthetics = "fill",
    name = "Escala Relativa\nde Valores Máximos\ne Mínimos",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      ticks = FALSE))+
  xlab("Pontos Amostrados")+ylab("Parâmetros")+
  geom_text(aes(label = round(value, 2)), color="white", size=3, fontface = "bold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(grafico, file = "RIMA_AG_SUPERF.png", width=8, height=6, dpi=180, type="cairo")
