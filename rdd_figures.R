library(ggplot2)

results_rdd <- readRDS("rdresults_main.rds")

received_bonus <- results_rdd %>%
  filter(outcome == "received_bonus")

bonus_plot <- ggplot(received_bonus, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_discrete(name = "Bandwidth")+
  theme_minimal()+ylab("CLATE estimate") + xlab("Donut Size")+ ggtitle("impact of smallholder status on probability of receiving payment")
bonus_plot
ggsave(path = "figs", filename = "received_bonus_plot.png", width = 8, height = 5)

rptpro_monto_total <- results_rdd %>%
  filter(outcome == "rptpro_monto_total")

rptpro_monto_total_plot <- ggplot(rptpro_monto_total, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_discrete(name = "Bandwidth")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")+ ggtitle("impact of smallholder status on total project payment amount")
rptpro_monto_total_plot
ggsave(path = "figs", filename = "payment_plot.png", width = 8, height = 5)

rptpre_superficie_bonificada <- results_rdd %>%
  filter(outcome == "rptpre_superficie_bonificada")

rptpre_superficie_bonificada_plot <- ggplot(rptpre_superficie_bonificada, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_discrete(name = "Bandwidth")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size") + ggtitle("impact of smallholder status on area enrolled (ha)")
rptpre_superficie_bonificada_plot
ggsave(path = "figs", filename = "area_enrolled_plot.png", width = 8, height = 5)

reforestation <- results_rdd %>%
  filter(outcome == "reforestation")

reforestation_plot <- ggplot(reforestation, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_discrete(name = "Bandwidth")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")+ ggtitle("impact of smallholder status on probability of pursuing reforestation")
reforestation_plot

timber <- results_rdd %>%
  filter(outcome == "timber")

timber_plot <- ggplot(timber, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_discrete(name = "Bandwidth")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")#+ ggtitle("impact of smallholder status on probability of pursuing timber production")
timber_plot
ggsave(path = "figs", filename = "timber_plot.png", width = 8, height = 5)

nontimber <- results_rdd %>%
  filter(outcome == "nontimber")

nontimber_plot <- ggplot(nontimber, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  scale_color_discrete(name = "Bandwidth")+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")#+ ggtitle("impact of smallholder status on probability of receiving payment")
nontimber_plot

