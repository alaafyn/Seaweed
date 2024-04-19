Skip to content
alaafyn
/
Seaweed

Type / to search

Code
Issues
Pull requests
Actions
Projects
Wiki
Security
Insights
Settings
Editing README.md in Seaweed
BreadcrumbsSeaweed
/
README.md
in
main

Edit

Preview
Indent mode

Spaces
Indent size

2
Line wrap mode

Soft wrap
Editing README.md file contents
1390
1391
1392
1393
1394
1395
1396
1397
1398
1399
1400
1401
1402
1403
1404
1405
1406
1407
1408
1409
1410
1411
1412
1413
1414
1415
1416
1417
1418
1419
1420
1421
1422
1423
1424
1425
1426
1427
1428
1429
1430
1431
1432
1433
1434
1435
1436
1437
1438
1439
1440
1441
1442
1443
1444
1445
1446
1447
1448
1449
1450
1451
1452
1453
1454
1455
1456
1457
1458
1459
1460
1461
1462
1463
1464
1465
1466
1467
1468
1469
1470
1471
1472
1473
1474
1475
1476
1477
1478
1479
1480
1481
1482
1483
1484
1485
1486
1487
1488
1489
1490
  wilcoxon_cutoff = 0.05,
  multigrp_strat = TRUE,
  strict = c("0", "1", "2"),
  sample_min = 10,
  only_same_subgrp = FALSE,
  curv = FALSE
)
plot_ef_bar(a15) + ggtitle("Basal vs Basal plus Seaweed Day 15")



a22<- run_lefse(cid_day22,group ="treatment",
  norm = "CPM", #`CPM`: pre-sample normalization of the sum of the values to 1e+06.
  norm_para = list(),
  kw_cutoff = 0.01,
  lda_cutoff = 2,
  bootstrap_n = 30,
  bootstrap_fraction = 2/3,
  wilcoxon_cutoff = 0.05,
  multigrp_strat = TRUE,
  strict = c("0", "1", "2"),
  sample_min = 10,
  only_same_subgrp = FALSE,
  curv = FALSE
)
plot_ef_bar(a22) + ggtitle("Basal vs Basal plus Seaweed Day 22")

plot6 <- plot_ef_bar(a22) + ggtitle("Basal vs Basal plus Seaweed Day 22")
plot7 <- plot_ef_bar(a15) + ggtitle("Basal vs Basal plus Seaweed Day 15")

# Use grid.arrange to arrange the plots with titles
alllefse <- grid.arrange(
  plot6, plot7,
  ncol = 2
)

# Save the combined plot
ggsave("Lefse DA.png", alllefse, width = 12, height = 8)



#Abundance box plot:  group para for plot_abunance() must be keep same with the group para in the differential analysis function. By default, plot_abundance() will plot all the markers, users can plot the specificity markers using para markers.

library(ggplot2)
library(gridExtra)

# Creating the abundance plots for Day 15 and Day 22
p_abd15 <- plot_abundance(a15, group = "treatment") +
  ggtitle("Lefse DA abundance on Day 15") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#F0E442")) # Custom fill colors

p_abd22 <- plot_abundance(a22, group = "treatment") +
  ggtitle("Lefse DA abundance on Day 22") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#F0E442")) # Use the same colors for consistency

# Arrange the plots in a single image
combined_lefseabd <- grid.arrange(p_abd15, p_abd22, ncol = 1) # Arranged vertically; change `ncol` to 2 for horizontal

# Save the combined image to file
ggsave("combined_lefseabundance_plots.png", combined_lefseabd, width = 10, height = 14)


###################################

#RNA-seq based DA methods (https://yiluheihei.github.io/microbiomeMarker/articles/microbiomeMarker-vignette.html)
#edgeR (Robinson, McCarthy, and Smyth 2010)
# multiple groups
#Subset treatment based on day15
mm_edger_mg <- run_edger(
    cid_day15,
    group = "treatment",
    method  = "QLFT",
    pvalue_cutoff = 0.05,
    p_adjust = "fdr"
)
mm_edger_mg
Edger15 <-plot_ef_bar(mm_edger_mg)
Edger15

#Day 22
mm_edger <- run_edger(
    cid_day22,
    group = "treatment",
    method  = "QLFT",
    pvalue_cutoff = 0.05,
    p_adjust = "fdr"
)
mm_edger
Edger22 <-plot_ef_bar(mm_edger)
Edger22

library(ggplot2)
library(ggpubr)
EdgeRcom<- ggarrange(Edger15 , Edger22 , ncol = 2, nrow = 1)
EdgeRcom
ggsave("EdgeRcom DA_plot.png", EdgeRcom, width = 10, height = 5)


EdgeRcom <- ggarrange(Edger15 + labs(title = "Day 15"), Edger22 + labs(title = "Day 22"), ncol = 2, nrow = 1)
EdgeRcom <- EdgeRcom + labs(title = "EdgeR Comparison")
EdgeRcom

Use Control + Shift + m to toggle the tab key moving focus. Alternatively, use esc then tab to move to the next interactive element on the page.
No file chosen
Attach files by dragging & dropping, selecting or pasting them.
Editing Seaweed/README.md at main · alaafyn/Seaweed
