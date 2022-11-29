mah <- read.csv("MAH_assignment_data.csv")

ggplot(mah, aes(x = Sample, y = Gene, fill = Expression)) +
  geom_tile(colour="black", linewidth=0.5)+ 
  scale_fill_gradient(low = "black", high = "yellow")+
  theme_grey(base_size=12)+
  facet_grid(~ Tissue, switch = "x", scales = "free_x", space = "free_x") +
  ggtitle(label = "Pheromone Gene Expression") +
  scale_x_discrete(labels=c('A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez', 
                            'A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez',
                            'A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez',
                            'A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez'))+
  theme(plot.title = element_text(face="bold"),
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.y =element_text(color = "black"),
        axis.text.x =element_text(angle = 315, hjust = 0, vjust = 0.5, color = "black"),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())
