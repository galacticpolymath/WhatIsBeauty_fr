require(pacman)
p_load(galacticPubs,galacticEdTools,dplyr,readr,readxl)

# Define function to convert wavelength to RGB color
# From https://gist.github.com/friendly/67a7df339aa999e2bcfcfec88311abfc
# Wavelength to RGB
#
# This function converts a given wavelength of light to an
# approximate RGB color value.
#
wavelength_to_rgb <- function(wavelength, gamma=0.8){

#
#    Based on code by Dan Bruton
#    http://www.physics.sfasu.edu/astro/color/spectra.html
#    '''

    if (wavelength >= 380 & wavelength <= 440) {
        attenuation = 0.3 + 0.7 * (wavelength - 380) / (440 - 380)
        R = ((-(wavelength - 440) / (440 - 380)) * attenuation) ^ gamma
        G = 0.0
        B = (1.0 * attenuation) ^ gamma
        }
    else if (wavelength >= 440 & wavelength <= 490) {
        R = 0.0
        G = ((wavelength - 440) / (490 - 440)) ^ gamma
        B = 1.0
        }
    else if (wavelength >= 490 & wavelength <= 510) {
        R = 0.0
        G = 1.0
        B = (-(wavelength - 510) / (510 - 490)) ^ gamma
        }
    else if (wavelength >= 510 & wavelength <= 580) {
        R = ((wavelength - 510) / (580 - 510)) ^ gamma
        G = 1.0
        B = 0.0
        }
    else if (wavelength >= 580 & wavelength <= 645) {
        R = 1.0
        G = (-(wavelength - 645) / (645 - 580)) ^ gamma
        B = 0.0
        }
    else if (wavelength >= 645 & wavelength <= 750) {
        attenuation = 0.3 + 0.7 * (750 - wavelength) / (750 - 645)
        R = (1.0 * attenuation) ^ gamma
        G = 0.0
        B = 0.0
        }
    else {
        R = 1
        G = 1
        B = 1
        }
    R = R * 255
    G = G * 255
    B = B * 255
    return (rgb(floor(R), floor(G), floor(B), max=255))
}

spec_data <- read_excel("data/barrenense_MSP_forNatalie.xlsx",sheet="Graph")
names(spec_data)[1]<-"wavelength"
manual_colors<-sapply(spec_data$wavelength,function(x) wavelength_to_rgb(x))
names(manual_colors)<-as.character(spec_data$wavelength)

# Darter sensitivity curve
ggplot(spec_data) + geom_smooth(formula="y~x",aes(x = wavelength, y = LWS), method="loess",se = FALSE,color=gpPal[[1]]$hex[1],size=2) +
  theme_galactic(text.cex = 1.5,pad.outer = rep(10,4),pad.xlab=30) +
  ylab("Darter Visual Sensitivity") + xlab("Wavelength of Light (nm)")+
  #ggplot2::geom_vline(xintercept=570,linetype="dashed",color="orange")+
  #ggplot2::geom_vline(xintercept=584,color="red")+
  coord_cartesian(ylim=c(0,1.1),clip="off",expand=F)+
  #Add spectrum below graph
  geom_segment(aes(x=wavelength,xend=wavelength,y=-0.25,yend=-0.15,col=as.character(wavelength)),show.legend = F)+
  scale_color_manual(values=manual_colors)+
  geom_rect(xmin=300,xmax=380,ymin=-0.25,ymax=-0.15,fill="gray35")+
  annotate("text",x=340,y=-0.2,label="UV",vjust=0.5,size=10,col="gray80")
gpsave("darter_vis_sensitivity_curve.png")
#This peak doesn't match expectations (the preferred orange model is at 570 :shrug:)
#



#Try another dataset from Jenny
(spec_data2<-read_excel("data/GovardovskiiA1A2.xls","Sheet1"))
ggplot(spec_data2) + geom_smooth(formula="y~x",aes(x = wavelength...4, y = `spectrum (600.65 nm)`), method="loess",se = FALSE,color=gpPal[[1]]$hex[1],size=2) +
  theme_galactic(text.cex = 1.5) +
  ylim(0,1)+
  ylab("Darter Visual Sensitivity") + xlab("Wavelength (nm)")+
  ggplot2::geom_vline(xintercept=570,linetype="dashed",color="orange")+
  ggplot2::geom_vline(xintercept=584,color="red")
