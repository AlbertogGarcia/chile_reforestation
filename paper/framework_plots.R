library(Metrics)
library(ggplot2)
library(ggpubr)
set.seed(0930)
x= seq(from = -5, to = 4, by = 1)
x_no <- 5
rnorm_1 <- rnorm(10)
target_perfect <- x +5 +rnorm_1
perfect_fun <- function(x) x + 5

rnorm_2 <- rnorm(10)
x_neg <- 5 - .65*x + rnorm_2
neg_fun <- function(x) 5 - .35*x


x_pos <- .1*x +5
df <- data.frame( x = x, x_no, x_pos, x_neg, target_perfect)%>%
  mutate(perfect_enroll = ifelse(x < -2, "applied", "applied and enrolled"),
         neg_enroll = ifelse(x_neg < 4.25, "applied", "applied and enrolled"))
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# perfect characteristic
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ggplot(df, aes(x = x, y = target_perfect, color = perfect_enroll)) + 
  stat_function(fun=perfect_fun, colour="black")+
  geom_point(size = 5)+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)), legend.title = element_blank())+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 11), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("#CC79A7", "#009E73"))

ggsave(filename = "paper/figs/framework_perfect.png", width = 6, height = 4)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# economist critique
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ggplot(df, aes(x = x, y = x_neg, color = neg_enroll)) + 
  stat_function(fun=neg_fun, colour="black")+
  geom_point(size = 5)+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)), legend.title = element_blank())+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 11), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("#CC79A7", "#009E73"))

ggsave(filename = "paper/figs/framework_neg.png", width = 6, height = 4)

ggplot(df, aes(x = x, y = x_neg, color = neg_enroll)) + 
  stat_function(fun=neg_fun, colour="black")+
  geom_point(size = 5)+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)), legend.title = element_blank())+
  scale_y_continuous(expand = c(0.02, 0.01), limits = c(0, 11), , breaks = seq(0, 10, 1),name = "targeting characterstic")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 5),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("#CC79A7", "#009E73"))+
  annotate('text', x = -2.5, y = 8.5, label = "poverty\nsmallholders\nindigenous communities", color = "black", size = 3.5)

ggsave(filename = "paper/figs/framework_neg_defor.png", width = 6, height = 4)

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
df <- data.frame( x = x, y)%>%
  mutate(group = ifelse(x < 0, "inframarginal", ifelse(x >= 5, "do not apply", "additional")
                        )
         )

ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 4, color = "brown")+
  theme_classic()+
  theme (axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 16), , breaks = seq(0, 15, 2),name = "number of landowners")+
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(-5, 10),  breaks = x, name = "cost of providing forest carbon")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 5)+
  geom_bracket(xmin = -5, xmax = -0.1, y.position = 12
               ,label = "(i) apply,\nprovide no additional\ncarbon benefits"
  )+
  geom_bracket(xmin = 0.1, xmax = 4.9, y.position = 12
               ,label = "(ii) apply,\ncontribute additional\ncarbon benefits= $10"
  )+
  geom_bracket(xmin = 5.1, xmax = 9.9, y.position = 7
               ,label = "", tip.length = c(-0.02,- 0.02)
  )+
  annotate('text', x = 7.5, y = 6.5, label = "(iii) do not apply", color = "black", size = 4)

ggsave(filename = "paper/figs/framework.png", width = 6, height = 4)
                       