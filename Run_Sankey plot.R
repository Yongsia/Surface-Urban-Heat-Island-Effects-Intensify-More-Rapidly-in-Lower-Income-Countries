library(dplyr)
library(ggplot2)
library(ggsankey)

# YD Sankey plot ----------------------------------------------------------


# yd = read.csv("/mnt/work/YY/dataset/YN_total.csv",row.names = 1)
# 
# yd1 = subset(yd, pvalue < 0.05 & slope > 0)
# yd3 = yd1[,c(6,9,10)]
# ydf = yd3 %>%
#   make_long(NAME_EN, Cli_Zone, INCOME_GRP.y)
# yd_n <- ydf%>%
#   dplyr::group_by(node)%>%
#   tally()
# yd_p <- merge(ydf, yd_n, by.x = 'node', by.y = 'node', all.x = TRUE)
# 
# p1 <- ggplot(yd_p, aes(x = x,
#                       next_x = next_x,
#                       node = node,
#                       next_node = next_node,
#                       fill = factor(node),
#                       label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
#   #scale_fill_manual(values = mycol2) + #更改配色
#   geom_sankey(#flow.alpha = 0.6,#条带不透明度
#     #flow.fill = 'grey', #条带填充色
#     #flow.color = 'grey80', #条带描边色
#     smooth = 10, #条带弯曲度
#     width = 0.12) + #节点宽度
#   geom_sankey_text(size = 1,
#                    color = "black",hjust = 1.2) +
#   theme_void() +
#   theme(legend.position = 'none')
# #scale_fill_lancet()##library("ggsci")
# #scale_fill_viridis_d(option ="inferno")
# ggsave(p1,dpi=300, width=10, height=8,filename = "/mnt/work/YY/yn.eps")


# YN Sankey plot ----------------------------------------------------------
# yd = read.csv("/mnt/work/YY/dataset/YN_total.csv",row.names = 1)
# 
# yd1 = subset(yd, pvalue < 0.05 & slope > 0)
# yd3 = yd1[,c(6,9,10)]
# ydf = yd3 %>%
#   make_long(NAME_EN, Cli_Zone.x, INCOME_GRP.x)
# yd_n <- ydf%>%
#   dplyr::group_by(node)%>%
#   tally()
# yd_p <- merge(ydf, yd_n, by.x = 'node', by.y = 'node', all.x = TRUE)
# 
# p1 <- ggplot(yd_p, aes(x = x,
#                        next_x = next_x,
#                        node = node,
#                        next_node = next_node,
#                        fill = factor(node),
#                        label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
#   #scale_fill_manual(values = mycol2) + #更改配色
#   geom_sankey(#flow.alpha = 0.6,#条带不透明度
#     #flow.fill = 'grey', #条带填充色
#     #flow.color = 'grey80', #条带描边色
#     smooth = 10, #条带弯曲度
#     width = 0.12) + #节点宽度
#   geom_sankey_text(size = 1,
#                    color = "black",hjust = 1.2) +
#   theme_void() +
#   theme(legend.position = 'none')
# 
# ggsave(p1,dpi=300, width=10, height=8,filename = "/mnt/work/YY/yn.eps")

# SD Sankey plot ----------------------------------------------------------
yd = read.csv("/mnt/work/YY/dataset/SD_total.csv",row.names = 1)

yd1 = subset(yd, pvalue < 0.05 & slope > 0)
yd3 = yd1[,c(6,9,10)]
ydf = yd3 %>%
  make_long(NAME_EN, Cli_Zone.x, INCOME_GRP.x)
yd_n <- ydf%>%
  dplyr::group_by(node)%>%
  tally()
yd_p <- merge(ydf, yd_n, by.x = 'node', by.y = 'node', all.x = TRUE)

p1 <- ggplot(yd_p, aes(x = x,
                       next_x = next_x,
                       node = node,
                       next_node = next_node,
                       fill = factor(node),
                       label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
  #scale_fill_manual(values = mycol2) + #更改配色
  geom_sankey(#flow.alpha = 0.6,#条带不透明度
    #flow.fill = 'grey', #条带填充色
    #flow.color = 'grey80', #条带描边色
    smooth = 10, #条带弯曲度
    width = 0.12) + #节点宽度
  geom_sankey_text(size = 1,
                   color = "black",hjust = 1.2) +
  theme_void() +
  theme(legend.position = 'none')
#scale_fill_lancet()##library("ggsci")
#scale_fill_viridis_d(option ="inferno")
ggsave(p1,dpi=300, width=10, height=8,filename = "/mnt/work/YY/sd.eps")

# SN Sankey plot ----------------------------------------------------------
yd = read.csv("/mnt/work/YY/dataset/SN_total.csv",row.names = 1)

yd1 = subset(yd, pvalue < 0.05 & slope > 0)
yd3 = yd1[,c(6,9,10)]
ydf = yd3 %>%
  make_long(NAME_EN, Cli_Zone.x, INCOME_GRP.x)
yd_n <- ydf%>%
  dplyr::group_by(node)%>%
  tally()
yd_p <- merge(ydf, yd_n, by.x = 'node', by.y = 'node', all.x = TRUE)

p1 <- ggplot(yd_p, aes(x = x,
                       next_x = next_x,
                       node = node,
                       next_node = next_node,
                       fill = factor(node),
                       label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
  #scale_fill_manual(values = mycol2) + #更改配色
  geom_sankey(#flow.alpha = 0.6,#条带不透明度
    #flow.fill = 'grey', #条带填充色
    #flow.color = 'grey80', #条带描边色
    smooth = 10, #条带弯曲度
    width = 0.12) + #节点宽度
  geom_sankey_text(size = 1,
                   color = "black",hjust = 1.2) +
  theme_void() +
  theme(legend.position = 'none')
#scale_fill_lancet()##library("ggsci")
#scale_fill_viridis_d(option ="inferno")
ggsave(p1,dpi=300, width=10, height=8,filename = "/mnt/work/YY/sn.eps")

# WD Sankey plot ----------------------------------------------------------
yd = read.csv("/mnt/work/YY/dataset/WD_total.csv",row.names = 1)

yd1 = subset(yd, pvalue < 0.05 & slope > 0)
yd3 = yd1[,c(6,9,10)]
ydf = yd3 %>%
  make_long(NAME_EN, Cli_Zone.x, INCOME_GRP.x)
yd_n <- ydf%>%
  dplyr::group_by(node)%>%
  tally()
yd_p <- merge(ydf, yd_n, by.x = 'node', by.y = 'node', all.x = TRUE)

p1 <- ggplot(yd_p, aes(x = x,
                       next_x = next_x,
                       node = node,
                       next_node = next_node,
                       fill = factor(node),
                       label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
  #scale_fill_manual(values = mycol2) + #更改配色
  geom_sankey(#flow.alpha = 0.6,#条带不透明度
    #flow.fill = 'grey', #条带填充色
    #flow.color = 'grey80', #条带描边色
    smooth = 10, #条带弯曲度
    width = 0.12) + #节点宽度
  geom_sankey_text(size = 1,
                   color = "black",hjust = 1.2) +
  theme_void() +
  theme(legend.position = 'none')
#scale_fill_lancet()##library("ggsci")
#scale_fill_viridis_d(option ="inferno")
ggsave(p1,dpi=300, width=10, height=8,filename = "/mnt/work/YY/wd.eps")

# WN Sankey plot ----------------------------------------------------------
yd = read.csv("/mnt/work/YY/dataset/WN_total.csv",row.names = 1)

yd1 = subset(yd, pvalue < 0.05 & slope > 0)
yd3 = yd1[,c(6,9,10)]
ydf = yd3 %>%
  make_long(NAME_EN, Cli_Zone.x, INCOME_GRP.x)
yd_n <- ydf%>%
  dplyr::group_by(node)%>%
  tally()
yd_p <- merge(ydf, yd_n, by.x = 'node', by.y = 'node', all.x = TRUE)

p1 <- ggplot(yd_p, aes(x = x,
                       next_x = next_x,
                       node = node,
                       next_node = next_node,
                       fill = factor(node),
                       label = paste0(node," (n=", n, ")"))) +#  round(pct,1), '%)' 
  #scale_fill_manual(values = mycol2) + #更改配色
  geom_sankey(#flow.alpha = 0.6,#条带不透明度
    #flow.fill = 'grey', #条带填充色
    #flow.color = 'grey80', #条带描边色
    smooth = 10, #条带弯曲度
    width = 0.12) + #节点宽度
  geom_sankey_text(size = 1,
                   color = "black",hjust = 1.2) +
  theme_void() +
  theme(legend.position = 'none')
#scale_fill_lancet()##library("ggsci")
#scale_fill_viridis_d(option ="inferno")
ggsave(p1,dpi=300, width=10, height=8,filename = "/mnt/work/YY/wn.eps")

