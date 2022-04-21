require(pacman)
p_load(galacticPubs,galacticEdTools,imagefluency,cowplot,grid)

#read in images
img_paths<-list.files("assets/P2_abstract-art-and-symmetry",pattern="*.jpg",full.names = T)
img_names<-basename(img_paths)
imgs<-lapply(img_paths,function(x)img_read(x))
names(imgs) <- img_names

#view images side-by-side
startwith<-1
plot_grid(ggdraw()+draw_image(imgs[[startwith]]),ggdraw()+draw_image(imgs[[startwith+1]]))

lapply(imgs,function(x)img_symmetry(x))
