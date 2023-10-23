## ----eval = FALSE-------------------------------------------------------------
#  devtools :: install_github(repo = "ajiangsfu/NMRphasing",force = TRUE)
#  ## if you do not have old versions of NMRphasing, please remove force = TRUE

## -----------------------------------------------------------------------------
library(NMRphasing)
data("fdat")
str(fdat)

## ----dpi = 300----------------------------------------------------------------

## in order to make comparison, absorption part can be extracted
fdat$Observed_Absorption = Re(fdat$frequency_domain)

library(ggpubr)
p1 = ggplot(fdat, aes(x = ppm, y = Observed_Absorption)) +
      geom_line() + theme_bw() + labs(y = "Observed Absorption") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1

## ----dpi = 300----------------------------------------------------------------

fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "SPC_DANM") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"),font.label =10,nrow = 2, ncol=1)


## ----dpi = 300----------------------------------------------------------------
fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "NLS") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"),font.label =10,nrow = 2, ncol=1)


## ----dpi = 300----------------------------------------------------------------
fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "MPC_DANM") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"),font.label =10,nrow = 2, ncol=1)


## ----dpi = 300----------------------------------------------------------------
fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "MPC_EMP") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"),font.label =10,nrow = 2, ncol=1)


## ----dpi = 300----------------------------------------------------------------
fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "SPC_EMP") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"),font.label =10,nrow = 2, ncol=1)


## ----dpi = 300----------------------------------------------------------------
fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "SPC_AAM") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"),font.label =10,nrow = 2, ncol=1)


## ----dpi = 300----------------------------------------------------------------
fdat$Phased_Absoprtion = NMRphasing(specDatIn = fdat$frequency_domain, method = "SPC_DSM") 

p2 = ggplot(fdat, aes(x = ppm, y = Phased_Absoprtion)) +
      geom_line() + theme_bw() + labs(y = "Phased Absoprtion") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggarrange(plotlist = list(p1,p2),labels = c("Before","After"), font.label =10,nrow = 2, ncol=1)


