
# reg.treat <- triQc %>%
#   att_gt(
#     yname = 'l.wage.perhr',
#     tname = 'year',
#     idname = 'fips.code',
#     gname = 'ch.year',
#     # xformla = controls,
#     data = .,
#     panel = F,
#     control_group = 'nevertreated',
#     anticipation = 0,
#     # weightsname = 'pwgt',
#     bstrap = T,
#     cband = T,
#     biters = 1000,
#     clustervars = 'state.id',
#     est_method = "dr",
#     base_period = "universal",
#     print_details = F,
#   )
# summary(reg.treat)
#
# # Aggregating the ATT(g,t). type = 'simple'.
# reg.treat.simple <- aggte(reg.treat, type = 'simple', na.rm = T)
# summary(reg.treat.simple)
#
# # Aggregating the ATT(g,t). type = 'dynamic'.
# reg.treat.es <- aggte(reg.treat, type = 'dynamic', min_e = -9, max_e = 9, na.rm = T)
# summary(reg.treat.es)
# ggdid(reg.treat.es, ylab = 'Event Study Estimate')
#
# # Now we are ready to go! Let me put all this into a table
# event.study.wageperhr <- data.frame(
#   type = "dynamic",
#   term = paste0('ATT(', reg.treat.es$egt, ")"),
#   event.time = reg.treat.es$egt,
#   estimate = reg.treat.es$att.egt,
#   std.error = reg.treat.es$se.egt,
#   conf.low = reg.treat.es$att.egt - reg.treat.es$crit.val.egt * reg.treat.es$se.egt,
#   conf.high = reg.treat.es$att.egt + reg.treat.es$crit.val.egt * reg.treat.es$se.egt,
#   point.conf.low = reg.treat.es$att.egt - stats::qnorm(1 - reg.treat.es$DIDparams$alp /
#     2) * reg.treat.es$se.egt,
#   point.conf.high = reg.treat.es$att.egt + stats::qnorm(1 - reg.treat.es$DIDparams$alp
#     / 2) * reg.treat.es$se.egt
# )
#
# # This is another option
# p_es_never2 <- ggplot(data = event.study.wageperhr, mapping = aes(x = event.time, y = estimate)) +
#   geom_line(size = 0.5, alpha = 2, colour = "black") +
#   geom_vline(xintercept = 0, color = 'black', size = 1, linetype = "dotted") +
#   geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1,
#                   color = "red") +
#   geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
#   xlab("Event time") +
#   ylab("Event Study Estimate") +
#   ylim(range(event.study.wageperhr$conflow,
#              event.study.wageperhr$confhigh,
#              event.study.wageperhr$conflow,
#              event.study.wageperhr$confhigh,
#              event.study.wageperhr$conf.low,
#              event.study.wageperhr$conf.high
#   )) +
#   scale_x_continuous(breaks = c(-9:9)) +
#   #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
#   theme(axis.text.y = element_text(size = 12)) +
#   theme(axis.text.x = element_text(size = 12)) +
#   theme(axis.title = element_text(color = "black", size = 12)) +
#   theme(plot.title = ggtext::element_markdown(size = 12,
#                                               #face = "bold",
#                                               color = "black",
#                                               hjust = 0,
#                                               lineheight = 1.2)
#   ) +
#   annotate(geom = "text", x = -6, y = 300000, label = "ATT(g,t) = 22865 (58722) & CI(-92227, 137957);
#   ATT_{es} = 31206 (56368) & CI(-79274, 141686)", color = "black")
#
# p_es_never2
#
# # Aggregating the ATT(g,t). type = 'group'.
# reg.treat.grp <- aggte(reg.treat, type = 'group', na.rm = T)
# summary(reg.treat.grp)
# ggdid(reg.treat.grp)
#
# # Aggregating the ATT(g,t). type = 'calendar'.
# reg.treat.calender <- aggte(reg.treat, type = 'calendar', na.rm = T)
# summary(reg.treat.calender)
# ggdid(reg.treat.calender, ylab = 'Calendar Time ATTs')