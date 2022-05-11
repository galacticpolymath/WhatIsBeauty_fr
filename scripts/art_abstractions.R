require(pacman)
p_load(galacticPubs,galacticEdTools,imagefluency,cowplot,grid,dplyr)

#read in images
img_paths<-list.files("assets/P2_abstract-art-and-symmetry",pattern="_abs.*\\.jpg",full.names = T)
img_names<-basename(img_paths)
trmts<-gsub("^[^_]*_abs(.?)_.*$","\\1",img_names)
dataset<-tibble(
  artwork = gsub("(^[^_]*)_abs.*$", "\\1", img_names),
  file = img_names,
  type = sapply(trmts,function(x) switch(x,
                                         `-`="decreased symmetry",
                                         `+`="increased symmetry",
                                         "unmodified symmetry"))
  )
imgs<-lapply(img_paths,function(x)img_read(x))
names(imgs) <- img_names

dataset$vert_symmetry<-pbapply::pbsapply(imgs,function(i){
  img_symmetry(i)["vertical"]
})
#view images side-by-side
startwith<-1
plot_grid(ggdraw()+draw_image(imgs[[startwith]]),ggdraw()+draw_image(imgs[[startwith+1]]),ggdraw()+draw_image(imgs[[startwith+2]]),nrow=1)

#Assign a random order to each artwork
set.seed(25)
dataset$order<-rep(sample(1:3,3),length(unique(dataset$artwork)))
