library(ggplot2)

results_rdd <- readRDS("rdresults_main.rds")

received_bonus <- results_rdd %>%
  filter(outcome == "received_bonus")

bonus_plot <- ggplot(received_bonus, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")+ ggtitle("impact of smallholder status on probability of receiving payment")
bonus_plot

rptpro_monto_total <- results_rdd %>%
  filter(outcome == "rptpro_monto_total")

rptpro_monto_total_plot <- ggplot(rptpro_monto_total, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")#+ ggtitle("impact of smallholder status on probability of receiving payment")
rptpro_monto_total_plot


rptpre_superficie_bonificada <- results_rdd %>%
  filter(outcome == "rptpre_superficie_bonificada")

rptpre_superficie_bonificada_plot <- ggplot(rptpre_superficie_bonificada, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")#+ ggtitle("impact of smallholder status on probability of receiving payment")
rptpre_superficie_bonificada_plot


reforestation <- results_rdd %>%
  filter(outcome == "reforestation")

reforestation_plot <- ggplot(reforestation, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")#+ ggtitle("impact of smallholder status on probability of receiving payment")
reforestation_plot


timber <- results_rdd %>%
  filter(outcome == "timber")

timber_plot <- ggplot(timber, aes(x = as.factor(donut), y = coeff, color = as.factor(bw)))+
  geom_point(position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), position = position_dodge(width = 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+ylab("CLATE estimate") + xlab("donut size")#+ ggtitle("impact of smallholder status on probability of receiving payment")
timber_plot

