library(Metrics)
library(grid)
library(pBrackets)
library(ggplot2)
library(ggpubr)
library(ggpubr)
library(grid)
library(pBrackets)

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

r_a <- function(x) 50 - x
r_f <- function(x) 30 - .5*x
r_f_pes <- function(x) 40 - .5*x

breaks <- c(20, 40)
labels <- c(expression(x[0]), expression(x[1]))
brack <- bracketsGrob(.15, 0.46, .15, .65, h=0.02, lwd=1, col="black")

ggplot() + 
  stat_function(fun=r_a, colour="black")+
  stat_function(fun=r_f, colour="black")+
  stat_function(fun=r_f_pes, colour="black")+
  theme_classic()+
  theme (axis.ticks.x = element_blank () , axis.text.y = element_blank () , axis.ticks.y = element_blank (), axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 52), name = "Rent ($/ha)")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 82), labels = labels, breaks = breaks, name = expression(Z[i]))+
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = 10), linetype = "dashed")+
  geom_segment(aes(x = 20, y = 0, xend = 20, yend = 30), linetype = "dashed")+
  annotation_custom(brack)+
  annotation_custom(grob = textGrob(label = "PES\npayment", x = 0.06, y = .57, hjust = 0))+
  annotate('text', x = 60, y = 2, label = expression(r[f]), color = "black", size = 4)+
  annotate('text', x = 72, y = 9, label = "r[f] + PES", color = "black", parse = TRUE, size = 4)+
  annotate('text', x = 3, y = 45, label = expression(r[a]), color = "black", size = 4)


library(ggplot2)
x= seq(from = -5, to = 4, by = 1)
x_no <- 5
target_perfect <- x +5
x_neg <- 5 - .35*x
x_pos <- .1*x +5
df <- data.frame( x = x, x_no, x_pos, x_neg, target_perfect)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# perfect characteristic
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ggplot(df, aes(x = x, y = target_perfect)) + 
  #geom_line(color = "darkgreen")+
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(filename = "figs/framework_perfect.png", width = 6, height = 4)

ggplot(subset(df, target_perfect > 2), aes(x = x, y = target_perfect)) + 
  #geom_line(color = "darkgreen")+
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(filename = "figs/framework_perfect_enrolled.png", width = 6, height = 4)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# economist critique
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ggplot(df, aes(x = x, y = x_neg)) + 
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(filename = "figs/framework_neg.png", width = 6, height = 4)

ggplot(subset(df, x_neg>4), aes(x = x, y = x_neg)) + 
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(filename = "figs/framework_neg_enrolled.png", width = 6, height = 4)

ggplot(subset(df, x_neg>4), aes(x = x, y = x_neg)) + 
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  annotate('text', x = -2.5, y = 8.5, label = "poverty\nsmallholders\nindigenous communities", color = "black", size = 3.5)

ggsave(filename = "figs/framework_defor.png", width = 6, height = 4)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# no correlation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ggplot(df, aes(x = x, y = x_no)) + 
  #geom_line(color = "darkgreen")+
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(filename = "figs/framework_no.png", width = 6, height = 4)
# 
# ggplot(subset(df, target_perfect > 2), aes(x = x, y = target_perfect)) + 
#   #geom_line(color = "darkgreen")+
#   geom_point(size = 4, color = "brown")+
#   theme_classic()+
#   theme (axis.title.x = element_text(margin = margin(t = 10)))+
#   scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 10), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
#   scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
#   geom_vline(xintercept = 0, linetype = "dashed")
# 
# ggsave(filename = "figs/framework_no_enrolled.png", width = 6, height = 4)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# first plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(ggplot2)
x= seq(from = -5, to = 9, by = 1)
y <- x+6
df <- data.frame( x = x, y)

ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 16), , breaks = seq(0, 15, 2),name = "number of landowners")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 10),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 5)+
  geom_bracket(xmin = -5, xmax = -0.1, y.position = 12
               ,label = "apply\nadditional\ncarbon benefits\n= 0"
  )+
  geom_bracket(xmin = 0.1, xmax = 4.9, y.position = 12
               ,label = "apply\nadditional\ncarbon benefits\n= 10"
  )+
  geom_bracket(xmin = 5.1, xmax = 9.9, y.position = 7
               ,label = "", tip.length = c(-0.02,- 0.02)
  )+
  annotate('text', x = 7.5, y = 6.5, label = "do not apply", color = "black", size = 4)

ggsave(filename = "figs/framework.png", width = 6, height = 4)
                       