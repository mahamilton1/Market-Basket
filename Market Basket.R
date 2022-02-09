library(dplyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(writexl)

summary(df)

even_row <- seq(2, nrow(df), 2)
odd_row <- seq(1, nrow(df), 2)

df1 <- df[even_row, ]
df2 <- df[odd_row, ] #do not use

sum(is.na(df1$Item01))
sum(is.na(df2$Item01))

n_distinct(df1)

items2 <- (sum(is.na(df1$Item02)))/(nrow(df1)) * 100
items3 <- (sum(is.na(df1$Item03)))/(nrow(df1)) * 100
items4 <- (sum(is.na(df1$Item04)))/(nrow(df1)) * 100
items5 <- (sum(is.na(df1$Item05)))/(nrow(df1)) * 100
items6 <- (sum(is.na(df1$Item06)))/(nrow(df1)) * 100 #do not use

df1_one <- df1[is.na(df1$Item02), ] #do not use
df1_two <- df1[!is.na(df1$Item02), ]

n_distinct(df1_two)

package <- df1_two
package$Item01 <- as.factor(package$Item01)
package$Item02 <- as.factor(package$Item02)
package$Item03 <- as.factor(package$Item03)
package$Item04 <- as.factor(package$Item04)
package$Item05 <- as.factor(package$Item05)
package$Item06 <- as.factor(package$Item06)
summary(package)

##common between items 
  ##"Apple Lightning to Digital AV Adapter" L, 
  ##"Dust-Off Compressed Gas 2 pack" L/R,
  ##"HP 61 ink" L/R
  ##"VIVO Dual LCD Monitor Desk mount" R
  ##"Apple Pencil" R, "USB 2.0 Printer cable" R
  ##"Nylon Braided Lightning to USB cable" L/R
  ##"Screen Mom Screen Cleaner kit" R
  ##"Apple USB-C Charger cable" R

package_split <- split(package$Item01, package$Item02,
                       package$Item03, package$Item04,
                       package$Item05, package$Item06)
trans <- as(package_split, "transactions")
inspect(head(trans,5))

#APPLE LIGHTNING TO ADAPTER 
#L - 0
av_ad <- apriori(trans, 
                 parameter = list(
                   conf = 0.5,
                   minlen = 3),
                 appearance = list(
                   lhs = "Apple Lightning to Digital AV Adapter",
                   default = "rhs"))
inspect(head(sort(av_ad, by = "confidence"), 5))

#DUST OFF COMPRESSED GAS
#L - 0
do_cg2 <- apriori(trans, 
                  parameter = list(
                    conf = 0.5,
                    minlen = 3),
                  appearance = list(
                    lhs = "Dust-Off Compressed Gas 2 pack",
                    default = "rhs"))
inspect(head(sort(do_cg2, by = "confidence"), 5))
#R - 1 to 1
do_cg2r <- apriori(trans, 
                   parameter = list(
                     conf = 0.5,
                     minlen = 3),
                   appearance = list(
                     rhs = "Dust-Off Compressed Gas 2 pack",
                     default = "lhs")) 
inspect(head(sort(do_cg2r, by = "confidence"), 5))

#HP 61 ink
#L - 0
hp61 <- apriori(trans,
                parameter = list(
                  minlen = 3,
                  conf = 0.5),
                appearance = list(
                  lhs = "HP 61 ink",
                  default = "rhs"))
inspect(head(sort(hp61, by = "confidence"), 5))
#R - 1 to 0.9444
hp61r <- apriori(trans,
                 parameter = list(
                   minlen = 3,
                   conf = 0.5),
                 appearance = list(
                   rhs = "HP 61 ink",
                   default = "lhs"))
inspect(head(sort(hp61r, by = "confidence"), 5))

#VIVO Dual LCD Monitor Desk mount
#R - 1 to 1
vlcd <- apriori(trans,
                parameter = list(
                  minlen = 3,
                  conf = 0.5),
                appearance = list(
                  rhs = "VIVO Dual LCD Monitor Desk mount",
                  default = "lhs"))
inspect(head(sort(vlcd, by = "confidence"), 5))

#Apple Pencil" R
#R - 0.9286 to 0.8667
apen <- apriori(trans,
                parameter = list(
                  minlen = 3,
                  conf = 0.5),
                appearance = list(
                  rhs = "Apple Pencil",
                  default = "lhs"))
inspect(head(sort(apen, by = "confidence"), 5)) 

#USB 2.0 Printer cable
#R- 0.5217 to 0.5217 (only 2 instead of 5)
usb2 <- apriori(trans,
                parameter = list(
                  minlen = 3,
                  conf = 0.5),
                appearance = list(
                  rhs = "USB 2.0 Printer cable",
                  default = "lhs"))
inspect(head(sort(usb2, by = "confidence"), 5))

#Nylon Braided Lightning to USB cable
#L - 0
nbusb <- apriori(trans,
                 parameter = list(
                   minlen = 3,
                   conf = 0.5),
                 appearance = list(
                   lhs = "Nylon Braided Lightning to USB cable",
                   default = "rhs"))
inspect(head(sort(nbusb, by = "confidence"), 5))
#R - 1 to 1
nbusbr <- apriori(trans,
                  parameter = list(
                    minlen = 3,
                    conf = 0.5),
                  appearance = list(
                    rhs = "Nylon Braided Lightning to USB cable",
                    default = "lhs"))
inspect(head(sort(nbusbr, by = "confidence"), 5))

#Screen Mom Screen Cleaner kit
#R - 1 to 1
screen <- apriori(trans,
                  parameter = list(
                    minlen = 3,
                    conf = 0.5),
                  appearance = list(
                    rhs = "Screen Mom Screen Cleaner kit",
                    default = "lhs"))
inspect(head(sort(screen, by = "confidence"), 5))

#Apple USB-C Charger cable
#R - 0
ausbc <- apriori(trans,
                 parameter = list(
                   minlen = 3,
                   conf = 0.1),
                 appearance = list(
                   rhs = "Apple USB-C Charger cable",
                   default = "lhs"))
inspect(head(sort(ausbc, by = "confidence"), 5))

##"Dust-Off Compressed Gas 2 pack" R**, do_cg2r
##"HP 61 ink" R**(1-0.94), hp61r
##"VIVO Dual LCD Monitor Desk mount" R**, vlcd
##"Apple Pencil" R**(0.92-0.86), apen
##"Nylon Braided Lightning to USB cable" R**, nbusbr
##"Screen Mom Screen Cleaner kit" R**, screen

#1-1 for 0.2supp
do_cg2r <- apriori(trans, 
                   parameter = list(
                     minlen = 3,
                     supp = 0.2,
                     conf = 0.5,
                     target = "rules"),
                   appearance = list(
                     rhs = "Dust-Off Compressed Gas 2 pack",
                     default = "lhs")) 
inspect(head(sort(do_cg2r, by = "confidence"), 5))
non_docg2r <- do_cg2r[!is.redundant(do_cg2r)]
inspect(head(sort(non_docg2r, by = "confidence"), 5))
summary(non_docg2r)
plot(non_docg2r, measure = "support", shading = "confidence",
     method = "graph", engine = "html")

#0.8788 - 0.8571 for 0.2supp
hp61r <- apriori(trans,
                 parameter = list(
                   minlen = 3,
                   supp = 0.2,
                   conf = 0.5,
                   target = "rules"),
                 appearance = list(
                   rhs = "HP 61 ink",
                   default = "lhs"))
inspect(head(sort(hp61r, by = "confidence"), 5))
summary(hp61r)
plot(hp61r, measure = "support", shading = "confidence",
     method = "graph", engine = "html")

#0.9643 - 0.9310 for 0.2upp
vlcd <- apriori(trans,
                parameter = list(
                  minlen = 3,
                  supp = 0.2,
                  conf = 0.5,
                  target = "rules"),
                appearance = list(
                  rhs = "VIVO Dual LCD Monitor Desk mount",
                  default = "lhs"))
inspect(head(sort(vlcd, by = "confidence"), 5))
non_vlcd <- vlcd[!is.redundant(vlcd)]
inspect(head(sort(non_vlcd, by = "confidence"), 5))
summary(non_vlcd)
plot(non_vlcd, measure = "support", shading = "confidence",
     method = "graph", engine = "html")

#0 for 0.2supp
apen <- apriori(trans,
                parameter = list(
                  minlen = 3,
                  supp = 0.2,
                  conf = 0.5,
                  target = "rules"),
                appearance = list(
                  rhs = "Apple Pencil",
                  default = "lhs"))
inspect(head(sort(apen, by = "confidence"), 5))

#0.9600 - 0.9259 for 0.2supp
nbusbr <- apriori(trans,
                  parameter = list(
                    minlen = 3,
                    supp = 0.2,
                    conf = 0.5,
                    target = "rules"),
                  appearance = list(
                    rhs = "Nylon Braided Lightning to USB cable",
                    default = "lhs"))
inspect(head(sort(nbusbr, by = "confidence"), 5))
non_nbusbr <- nbusbr[!is.redundant(nbusbr)]
inspect(head(sort(non_nbusbr, by = "confidence"), 5))
summary(non_nbusbr)
plot(non_nbusbr, measure = "support", shading = "confidence",
     method = "graph", engine = "html")

#0.8709 - 0.8286 for 0.2supp
screen <- apriori(trans,
                  parameter = list(
                    minlen = 3,
                    supp = 0.2,
                    conf = 0.5,
                    target = "rules"),
                  appearance = list(
                    rhs = "Screen Mom Screen Cleaner kit",
                    default = "lhs"))
inspect(head(sort(screen, by = "confidence"), 5))
summary(screen)
plot(screen, measure = "support", shading = "confidence",
     method = "graph", engine = "html")

##top 3 rules
inspect(head(sort(non_docg2r, by = "support", decreasing = TRUE), 1))
inspect(head(sort(non_vlcd, by = "confidence", decreasing = TRUE), 1))
inspect(head(sort(non_nbusbr, by = "confidence", decreasing = TRUE), 1))

write_xlsx(package, "Cleaned_Dataset.xlsx")