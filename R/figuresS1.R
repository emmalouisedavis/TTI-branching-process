#' Makes Figure S1 from manuscript
#' @author Joel Hellewell
#'
#' @return A ggplot2 plot object
#' @export
#' @importFrom data.table data.table
#' @importFrom tidyr gather
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom graphics hist
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_ribbon theme_bw theme xlab ylab geom_line geom_vline scale_colour_brewer scale_fill_brewer element_text aes
#'
make_figure_S1a <- function() {

  num <- Source <- xlim <- NULL
  # A colour-blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  hist(pmax(1,rlnorm((0:20000000)/100000,meanlog=1.434,sdlog=0.6612)+rgamma((0:20000000)/1000000,shape=2.116,rate=0.6899)-3),plot=F,breaks=c(0,0.5,(10:20000)/10)) -> h1
  hist(pmax(1,rlnorm((0:20000000)/100000,meanlog=1.434,sdlog=0.6612)+rgamma((0:20000000)/1000000,shape=14.9,rate=1.2716)-11.09),plot=F,breaks=c(0,0.5,(10:20000)/10)) -> h2
  GI <- data.frame(gamma_sing = dgamma((0:2000)/100,shape=1.819,scale=2.821),
                   gamma_china = dgamma((0:2000)/100,shape=2.1217,scale=1.028))

  GI2 <- data.frame(num=c(h1$density,h2$density,GI$gamma_sing,GI$gamma_china),x = c(h1$breaks[1:length(h1$density)],h2$breaks[1:length(h2$density)],(0:2000)/100,(0:2000)/100),Source=c(rep('He et al.',length(h1$density)),rep('Ashcroft et al.',length(h2$density)),rep('Ganyani et al. (1)',2001),rep('Ganyani et al. (2)',2001)))

  GI2 %>%
    ggplot2::ggplot(ggplot2::aes(x = x,
                                 y = num,
                                 ymin = 0,
                                 ymax = num,
                                 fill = Source)) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    cowplot::theme_cowplot() +
    ggplot2::geom_line(aes(x, num,colour=Source),size=0.5) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 10)) +
    ggplot2::scale_fill_manual(values = cbPalette[c(2,3,4,5)],name="") +
    ggplot2::scale_colour_manual(guide="none",values = cbPalette[c(2,3,4,5)],name="") +
    # ggplot2::geom_vline(data = medians,
    #                     ggplot2::aes(xintercept = x,
    #                                  col = as.factor(dist)),
    #                     lty = 2,
    #                     size = 0.8) +
    ggplot2::labs(tag = "c",
                  x = "generation interval (days)",
                  y = "probability density") +
    xlim(c(0,20))

}


#' Constructs figure 2 from the manuscript
#' @author Joel Hellewell
#' @return A patchwork object containing a number of ggplot2 plots
#' @export
#' @importFrom sn dsn
#' @importFrom ggplot2 ggplot aes geom_line geom_vline coord_cartesian labs geom_ribbon scale_fill_manual theme element_text
#' @importFrom cowplot theme_cowplot
#' @examples
#'\dontrun{
#'make_figure_2()
#'}
make_figure_S1 <- function() {

  # A colour-blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  p2 <- data.frame(x = seq(0, 15, 0.1),
                   y = dlnorm(x = seq(0, 15, 0.1),
                                meanlog = 1.434065,
                                sdlog = 0.6612),
                   theta = rep(c("Li et al."),rep(151,1))) %>%
    ggplot2::ggplot(aes(x = x, y = y, fill=theta,colour=theta)) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::geom_vline(xintercept = exp(1.43), lty = 2) +
    ggplot2::coord_cartesian(xlim = c(0, 13)) +
    ggplot2::labs(tag = "a", x = "time since infection (days)", y = "probability density") +
    ggplot2::geom_ribbon(aes(ymax = y, ymin = 0),
                         alpha = 0.4) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(values = cbPalette[1],
                               name = c("Incubation\nperiod"))  +
    ggplot2::scale_colour_manual(guide="none",values = cbPalette[1])


  p3 <- data.frame(y = c(dgamma(seq(0, 43, 0.1), 2.115779, 0.6898583),dgamma(seq(0, 43, 0.1), 97.18750,3.71875)),
                   x = c(seq(0, 43, 0.1)-3,seq(0, 43, 0.1)-25.625),
                   theta = c(rep(c("He et al."), rep(431, 1)),rep(c("Ashcroft et al."), rep(431, 1)))) %>%
    ggplot2::ggplot(aes(x, y, fill = theta, colour=theta)) +
    ggplot2::geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.4) +
    ggplot2::geom_line(aes(x, y)) +
    cowplot::theme_cowplot() +
    ggplot2::coord_cartesian(xlim = c(-10, 10)) +
    ggplot2::geom_vline(xintercept = -1.4,lty=2) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(values = cbPalette[c(2,5)],
                               name = c("Transmission\nprofile")) +
    ggplot2::scale_colour_manual(guide="none",values = cbPalette[c(2,5)]) +
    ggplot2::labs(tag = "b",
                  x = "time since symptom onset (days)",
                  y = "probability density")


  ((p2 / p3) | make_figure_S1a() ) & theme(axis.text = element_text(size = 10),
                                         legend.title = element_text(size = 12),
                                         legend.text = element_text(size = 10),
                                         axis.title = element_text(size = 10))

}
