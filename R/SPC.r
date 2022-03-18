# # # # meanColor <- function(matrixList) {
# # # #   tictoc::tic("mean")
# # # #   row <- nrow(matrixList[[1]])
# # # #   col <- ncol(matrixList[[1]])
# # # #   corp<- colorRamp(c("blue", "white", "red"), alpha = TRUE)
# # # #   l <- length(matrixList)
# # # #   avr <- corp(matrixList[[1]])^2
# # # #   for (i in 2:l) {
# # # #     avr <- avr + corp(matrixList[[i]])^2
# # # #   }
# # # #   res <- array(dim = c(row, col, 4))
# # # #   res[,,1] <- sqrt(matrix(avr[,1], row, col)/l)
# # # #   res[,,2] <- sqrt(matrix(avr[,2], row, col)/l)
# # # #   res[,,3] <- sqrt(matrix(avr[,3], row, col)/l)
# # # #   res[,,4] <- sqrt(matrix(avr[,1], row, col)/l)
# # # #   tictoc::toc()
# # # #   return(res)
# # # # }
# library("RcppFaddeeva")
# library(optimx)
# library(scales)
# library(ggplot2)
# library(reshape2)
#
# a_theme <- theme(legend.position = "none",
#                  # panel.background = element_blank(),
#                  panel.background = element_rect(fill = "transparent",colour = NA),
#                  plot.background = element_rect(fill = "transparent",colour = NA),
#                  # panel.grid.major = element_line(size = 0.2, linetype = 'dashed',
#                  #                                 colour = "lightgray"),
#                  # panel.grid.minor = element_line(size = 0.2, linetype = 'dashed',
#                  #                                 colour = "lightgray"),
#                  panel.grid.major = element_blank(),
#                  panel.grid.minor = element_blank(),
#                  axis.line.x = element_line(size = 0.3, linetype = 'solid',
#                                             colour = "black"),
#                  axis.text.y = element_blank(),
#                  axis.ticks.y = element_blank(),
#                  axis.title.y = element_blank())
# # axis.title = element_blank(),
# # axis.ticks = element_blank())
#
# # devtools::load_all()
# f <- function(x, param){
#   v1 <- Voigt(x, param[3], param[5], param[6]) #3.233 #0.005
#   v2 <- Voigt(x, param[4], param[7], param[8])
#   # v3 <- Voigt(x, param[7], param[9], param[10])
#   # v4 <- Voigt(x, param[8], param[9], param[10])
#   # f <- v1/param[1] + v2/param[2] + v3/param[3] + v4/param[4] + rnorm(length(x), 0, 1)
#   f <- v1/param[1] + v2/param[2] + rnorm(length(x), 0, 1)
#   return(f)
# }
#
#
# ta <- function(exp, param) {
#   # ta <- chisq.test(exp$y, f(exp$x, param))$statistic
#   ta <- sum((exp$y - f(exp$x, param))^2)
#   return(ta)
# }
#
# set.seed(200)
# nIA <- 0.2
# nIB <- 0.3
# nnIA <- rnorm(200, 0, nIA)
# nnIB <- rnorm(200, 0, nIB)
# # nnIB <- nnIA
#
# set.seed(800)
# nCSA <- 0.001
# nCSB <- 0.001
# nnCSA <- rnorm(200, 0, nCSA)
# nnCSB <- rnorm(200, 0, nCSB)
#
# for (k in 7) {
#   CSA <- 3.24
#   CSB <- 3.24 + 0.002*k
#   IA <- 1
#   IB <- 2.5
#   LA <- 0.003
#   LB <- 0.005
#
#   x_axis <- seq(3.2, 3.3, length.out = 200)
#   da <- data.frame(x = x_axis,
#                    exp = f(x_axis, c(IA, IB, CSA, CSB, LA, LA, LB, LB)),
#                    red =  Voigt(x_axis, CSA, LA, LA)/IA, col = "red",
#                    blue = Voigt(x_axis, CSB, LB, LB)/IB, col = "blue")
#   p1<- ggplot(da, aes(x = x, y = exp)) +
#     geom_line(aes(color = "black")) +
#     geom_line(aes(x, y = red, color = "gray"), size = 1) +
#     geom_line(aes(x, y = blue, color = "gray"), size = 1) +
#     xlab("") + ylab("[a.u.]") +
#     a_theme
#
#   N <- 200
#   X <- list()
#   for (i in 1:N) {
#     X[[i]] <- f(x_axis,
#                 c(IA + nnIA[i],
#                   IB + nnIB[i],
#                   CSA + nnCSA[i],
#                   CSB + nnCSB[i],
#                   LA,
#                   LA,
#                   LB,
#                   LB))
#   }
#   X <- do.call("rbind", X)
#
#   # X2 <- list()
#   # for (i in 1:200) {
#   #   fi <- runif(200) # ound(runif(200)) * 2 - 1
#   #   X2[[i]] <- apply(X * fi, 2, sum)
#   #   # plot(apply(XX, 2, sum), type = "l")
#   # }
#   # X2 <- do.call("rbind", X2)
#   # X <- X2
#
#   da <- data.frame(x_axis = x_axis,
#                    x = t(X))
#   long<- melt(da, id.vars = "x_axis")
#
#   p2 <- ggplot(long, aes(x = x_axis, y = value, color = variable)) +
#     geom_line() +
#     xlab("ppm") + ylab("[a.u.]") + labs(caption = paste("spc", nIA, nIB, nCSA, nCSB, sep = " / ")) +
#     a_theme
#
#   correlation = list()
#   x<-list()
#
#   sample_size <- 30
#   for (i in 1:50) {
#     col = colorRampPalette(c("blue", "white", "red"))(10)
#     randIdx = sample(nrow(X),sample_size)
#     x[[i]] <- X[randIdx,]
#     cor = cor(X[randIdx,], method = "spearman", use = "complete.obs")
#     correlation[[i]] <- cor
#   }
#
#   labs <- rownames(correlation[[1]])
#   x_trace <- apply(x[[1]],2,median)
#
#   # array <- meanColor(lapply(correlation, function(x) rescale(x, to=c(0 , 1), from=c(-1,1))))
#   avr <- correlation[[1]] / 50
#   for (i in 2:50) {
#     avr <- avr + correlation[[i]] / 50
#   }
#   array <- fusion::corToColor(rescale(avr, to=c(min(avr), 1)))
#
#   fix <- "test2NoCorr"
#   png(filename = paste("spc",nIA,nIB,nCSA,nCSB,fix,".png", sep = "_"), width = 800, height = 800, bg = "transparent")
#   p3 <- plotCorrelation(array, labs, trace = x_trace, x_axis)
#   dev.off()
#
#   png(filename = paste("spc_k",nIA,nIB,nCSA,nCSB,fix,".png", sep = "_"), width = (800*0.75), height = 500, bg = "transparent")
#   gridExtra::grid.arrange(p1, p2)
#   dev.off()
#
#   svglite::svglite(filename = paste("spc",nIA,nIB,nCSA,nCSB,fix,".svg", sep = "_"), width = 4, height = 4, bg = "transparent")
#   p3 <- plotCorrelation(array, labs, trace = x_trace, x_axis)
#   dev.off()
#
#   svglite::svglite(filename = paste("spc_k",nIA,nIB,nCSA,nCSB,fix,".svg", sep = "_"), width = (4*0.75), height = 500/800*4, bg = "transparent")
#   gridExtra::grid.arrange(p2, p1)
#   dev.off()
# }
#
#
#
# #
# #
# #
# # a_theme <- theme(legend.position = "none",
# #                  # panel.background = element_blank(),
# #                  panel.background = element_rect(fill = "transparent",colour = NA),
# #                  plot.background = element_rect(fill = "transparent",colour = NA),
# #                  # panel.grid.major = element_line(size = 0.2, linetype = 'dashed',
# #                  #                                 colour = "lightgray"),
# #                  # panel.grid.minor = element_line(size = 0.2, linetype = 'dashed',
# #                  #                                 colour = "lightgray"),
# #                  panel.grid.major = element_blank(),
# #                  panel.grid.minor = element_blank(),
# #                  axis.line = element_line(size = 0.3, linetype = 'solid',
# #                                             colour = "black"))
# # # axis.title = element_blank(),
# # # axis.ticks = element_blank())
# #
# # load("~/Downloads/SPC_ABA1.rda")
# #
# # plot(aba1_h, h)
# #
# # hty <- data.frame(class = "control", x = aba1_h, y = h)
# # pos <- data.frame(class = "positive", x = aba1_p, y = p)
# # wide <- rbind(hty, pos)
# # wide$x <- log(wide$x)
# # wide$y <- log(wide$y)
# # mod <- lm(y ~ x + x*class, data = wide)
# # shapiro.test(wide$y)
# # shapiro.test(wide$x)
# #
# # ggplot(data = wide, aes(x = x, y = y, group = class, color = class)) +
# #   geom_point() +
# #   stat_smooth(method = "lm",formula = y ~ x, fullrange = TRUE) +
# #   scale_color_manual(values = c("#2B9532", "#E11E1E")) +
# #   labs(title = paste("Adj R2 = ",signif(summary(mod)$adj.r.squared, 5),
# #                      "Intercept =",signif(mod$coef[[1]],5 ),
# #                      " Slope =",signif(mod$coef[[2]], 5),
# #                      " P =",signif(summary(mod)$coef[2,4], 5))) +
# #   a_theme
