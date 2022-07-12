# grid_plot_tricks
grid plot tricks

```
my_pushViewport<-function(
  idx,my_layout,
  x_width,y_height,
  xscale=c(0,1),
  yscale=c(0,1)){
  my_layout<-as.matrix(my_layout[nrow(my_layout):1,])
  if(missing(x_width)){
    x_width<-rep(1,ncol(my_layout))/ncol(my_layout)
  }else{
    x_width<-x_width/sum(x_width)
  }
  if(missing(y_height)){
    y_height<-rep(1,nrow(my_layout))/nrow(my_layout)
    y_height<-rev(y_height)
  }else{
    y_height<-rev(y_height)
    y_height<-y_height/sum(y_height)
  }
  
  min_row_idx<-min(which(my_layout==idx,arr.ind=T)[,"row"])
  max_row_idx<-max(which(my_layout==idx,arr.ind=T)[,"row"])
  min_col_idx<-min(which(my_layout==idx,arr.ind=T)[,"col"])
  max_col_idx<-max(which(my_layout==idx,arr.ind=T)[,"col"])
  y_bottom<-cumsum(y_height)#(1:nrow(my_layout))/nrow(my_layout)
  y_top<-c(0,y_bottom[-length(y_bottom)])#(1:nrow(my_layout)-1)/nrow(my_layout)
  x_right<-cumsum(x_width)#(1:ncol(my_layout))/ncol(my_layout)
  x_left<-c(0,x_right[-length(x_right)])#(1:ncol(my_layout)-1)/ncol(my_layout)
  
  pushViewport(viewport(
    x=(x_left[min_col_idx]+x_right[max_col_idx])/2,
    y=(y_top[min_row_idx]+y_bottom[max_row_idx])/2,
    width=(x_right[max_col_idx]-x_left[min_col_idx]),
    height=(y_bottom[max_row_idx]-y_top[min_row_idx]),
    xscale=xscale,yscale=yscale))
}

pushViewport(viewport(width=0.95, height=0.9))

my_pushViewport(my_layout[ii,1],my_layout,xscale=c(0,as.numeric(xlim_date[2]-xlim_date[1])))
```

## A very useful example

```
grid.newpage()

pushViewport(viewport(layout=grid.layout(
  nrow=5,ncol=4,
  widths=unit(c(5,5,5,5),c("lines","null","null","null")),
  heights=unit(c(1,1,1,2,1),c("lines","lines","null","null","lines")))))

my_layout<-rbind(
  c(1,1,1,2),
  c(1,1,1,3),
  c(4,4,5,6),
  c(7,7,5,8),
  c(7,7,9,0))

for(idx in 1:9){
  pushViewport(viewport(
    layout.pos.row=which(apply(my_layout==idx,1,any)),
    layout.pos.col=which(apply(my_layout==idx,2,any))))
  grid.rect()
  grid.text(idx)
  popViewport()
}
```

### with NA

```
grid.newpage()

pushViewport(viewport(layout=grid.layout(
  nrow=5,ncol=4,
  widths=unit(c(5,5,5,5),c("lines","null","null","null")),
  heights=unit(c(1,1,1,2,1),c("lines","lines","null","null","lines")))))

my_layout<-rbind(
  c(1,1,1,2),
  c(1,1,1,3),
  c(4,4,5,6),
  c(7,7,5,8),
  c(7,7,9,NA))

for(idx in 1:9){
  pushViewport(viewport(
    layout.pos.row=which(apply(my_layout==idx,1,any)),
    layout.pos.col=which(apply(my_layout==idx,2,any))))
  grid.rect()
  grid.text(idx)
  popViewport()
}
```

## plotViewport/dataViewport

```
grid.rect(gp = gpar(lty = "dashed"))
x <- y <- 1:10
pushViewport(plotViewport(c(5.1, 4.1, 4.1, 2.1)))
pushViewport(dataViewport(x, y))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.points(x, y)
grid.text("1:10", x = unit(-3, "lines"), rot = 90)
popViewport(2)
```
## Table
```
df<-data.frame(
  color=c("black","red","blue"),
  text=LETTERS[1:3],
  percent=c(0.3,0.4,0.6))

myborder<-unit(c(0.1,0.5,0.1,0.5),"lines")
textvp<-viewport()
percentvp<-viewport(width=unit(1,"lines"),xscale=c(-0.05,1.05),yscale=c(-0.05,1.05))
symbolvp<-viewport(width=unit(1,"lines"))

grid.newpage()
mylegend<-frameGrob(
  layout=grid.layout(
    nrow=3,ncol=3,
    widths=unit(rep(5,3),"lines"),
    height=unit(rep(2,3),"lines")))
for(ii in 1:3){
  mylegend<-packGrob(
    mylegend,
    linesGrob(
      x=c(0,1),y=c(0.5,0.5),
      vp=symbolvp,
      gp=gpar(col=df$symbol[ii],lwd=1.5)),
    row=ii,col=1,border=myborder)
  mylegend<-packGrob(
    mylegend,
    textGrob(df$text[ii]),
    row=ii,col=2,border=myborder)
  
  p_left<-rectGrob(x=0,y=0.5,width=df$percent[ii],just="left",gp=gpar(fill="lightsteelblue1"))
  p_right<-rectGrob(x=1,y=0.5,width=1-df$percent[ii],just="right",gp=gpar(fill="coral1"))
  p_text<-textGrob(df$percent[ii])
  mylegend<-packGrob(
    mylegend,
    gTree(children=gList(p_left,p_right,p_text)),
    row=ii,col=3,border=myborder)
}
grid.draw(mylegend)
```

## best table example
```
my_table_names<-list(
  "",
  "Assay",
  "% T1D Free\n0 AB",
  "% T1D Free\n1 AB",
  "% T1D Free\n2+ AB",
  expression(paste("Spearman's ",rho)),
  "AUC")

myborder<-unit(c(0.1,0.1,0.1,0.1),"lines")
symbolvp<-viewport(width=unit(1,"lines"))
mylegend<-frameGrob()

for(jj in 1:length(my_table_names)){
  mylegend<-packGrob(mylegend,textGrob(my_table_names[[jj]],gp=gpar(cex=0.8)),row=1,col=jj,border=myborder)
}
for(ii in 1:nrow(df_npositive_ROC)){
  a_table<-df_npositive_ROC$table[[ii]]
  # col 1
  col1<-linesGrob(
    x=c(0,1),y=c(0.5,0.5),
    vp=symbolvp,
    gp=gpar(col=all_colors[ii],lwd=1.5))
  mylegend<-packGrob(mylegend,col1,row=ii+1,col=1,border=myborder)
  
  # col 2
  col2<-textGrob(df_npositive_ROC$name[ii],gp=gpar(cex=0.8))
  mylegend<-packGrob(mylegend,col2,row=ii+1,col=2,border=myborder)
  
  # cols 3-5
  for(jj in 1:3){
    
    p_left<-rectGrob(x=0,y=0.5,width=a_table[jj,1]/sum(a_table[jj,]),just="left",gp=gpar(fill="lightsteelblue1"))
    p_right<-rectGrob(x=1,y=0.5,width=a_table[jj,2]/sum(a_table[jj,]),just="right",gp=gpar(fill="coral1"))
    p_text<-textGrob(paste0(num_to_str(a_table[jj,1]/sum(a_table[jj,])*100,1),"%"),gp=gpar(cex=0.8))
    
    mylegend<-packGrob(
      mylegend,
      gTree(children=gList(p_left,p_right,p_text)),
      row=ii+1,col=2+jj,border=myborder)
  }
  
  # col 6
  col6<-textGrob(num_to_str(df_npositive_ROC$cor[ii],digits=3),gp=gpar(cex=0.8))
  mylegend<-packGrob(mylegend,col6,row=ii+1,col=6,border=myborder)
  
  # col7
  col7<-textGrob(num_to_str(df_npositive_ROC$auc[ii],digits=3),gp=gpar(cex=0.8))
  mylegend<-packGrob(mylegend,col7,row=ii+1,col=7,border=myborder)
}

grid.newpage()
mylegend$framevp<-viewport(layout=grid.layout(
  nrow=dim(mylegend$framevp$layout$respect.mat)[1],
  ncol=dim(mylegend$framevp$layout$respect.mat)[2],
  widths=unit(c(2,7,4,4,4,3,3),"lines"),
  heights=unit(rep(1.5,6),"lines")))

```


## frameGrob example (sometimes does not work)
```
library(grid)
fg<-frameGrob()
fg<-packGrob(fg,rectGrob(gp=gpar(fill=adjustcolor("red",0.2))),row=1,col=1)
fg<-packGrob(fg,rectGrob(gp=gpar(fill=adjustcolor("blue",0.2))),row=1,col=2)
fg<-packGrob(fg,rectGrob(gp=gpar(fill=adjustcolor("yellow",0.2))),row=1,col=3)
fg<-packGrob(fg,rectGrob(gp=gpar(fill=adjustcolor("green",0.2))),row=1,col=4)

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=4,widths=unit(c(0.3,0.2,0.1,0.4),"native"),heights=unit(1,"cm"))))
grid.draw(fg)
```
not successfull either
```
library(grid)
fg<-frameGrob()
fg<-packGrob(fg,rectGrob(gp=gpar(fill=adjustcolor("red",0.2))),row=1,col=1)
fg<-packGrob(fg,rectGrob(gp=gpar(fill=adjustcolor("blue",0.2))),row=1,col=2)

grid.newpage()
grid.draw(grob(fg,vp=viewport(layout=grid.layout(
  nrow=1,ncol=2,widths=unit(c(0.3,0.2),"native"),heights=unit(1,"cm")))))

fg2<-fg
fg2$framevp<-viewport(layout=grid.layout(
  nrow=1,ncol=2,widths=unit(c(0.3,0.2),"in"),heights=unit(1,"cm")))


grid.newpage()

a_gtable<-frameGrob()
fg1<-fg;fg2<-fg;fg3<-fg;fg4<-fg
fg1$framevp<-viewport(layout=grid.layout(
  nrow=1,ncol=2,widths=unit(c(0.3,0.2),"in"),heights=unit(1,"cm")))
fg2$framevp<-viewport(layout=grid.layout(
  nrow=1,ncol=2,widths=unit(c(0.3,0.2),"in"),heights=unit(1,"cm")))
fg3$framevp<-viewport(layout=grid.layout(
  nrow=1,ncol=2,widths=unit(c(0.3,0.2),"in"),heights=unit(1,"cm")))
fg4$framevp<-viewport(layout=grid.layout(
  nrow=1,ncol=2,widths=unit(c(0.3,0.2),"in"),heights=unit(1,"cm")))
a_gtable<-packGrob(a_gtable,fg1,row=1,col=1)
a_gtable<-packGrob(a_gtable,fg2,row=2,col=1)
a_gtable<-packGrob(a_gtable,fg3,row=3,col=1)
a_gtable<-packGrob(a_gtable,fg4,row=4,col=1)

a_gtable$framevp<-viewport(layout=grid.layout(
  nrow=4,ncol=1,
  widths=unit(5,"lines"),
  heights=unit(5,"lines")))
grid.draw(a_gtable)

```

## (Good) a table of percent bars

```
a_frame<-frameGrob()
a_frame<-packGrob(a_frame,rectGrob(gp=gpar(col=NA,fill="red",alpha=0.5)),side="right")
a_frame<-packGrob(a_frame,rectGrob(gp=gpar(col=NA,fill="blue",alpha=0.5)),side="right")

p1<-gTree(children=gList(
  rectGrob(),
  gTree(children=gList(a_frame),vp=viewport(layout=grid.layout(ncol=2,nrow=1,widths=unit(c(1,3),"null"))))))
p2<-gTree(children=gList(
  rectGrob(),
  gTree(children=gList(a_frame),vp=viewport(layout=grid.layout(ncol=2,nrow=1,widths=unit(c(2,3),"null"))))))
p3<-gTree(children=gList(
  rectGrob(),
  gTree(children=gList(a_frame),vp=viewport(layout=grid.layout(ncol=2,nrow=1,widths=unit(c(5,3),"null"))))))
p4<-gTree(children=gList(
  rectGrob(),
  gTree(children=gList(a_frame),vp=viewport(layout=grid.layout(ncol=2,nrow=1,widths=unit(c(0.7,3),"null"))))))

frame_new<-frameGrob()
frame_new<-packGrob(frame_new,p1,side="bottom")
frame_new<-packGrob(frame_new,p2,side="bottom")
frame_new<-packGrob(frame_new,p3,side="bottom")
frame_new<-packGrob(frame_new,p4,side="bottom")

frame_new$framevp<-viewport(
  height=unit(4,"cm"),width=unit(4,"cm"),
  layout=grid.layout(4,1))

grid.newpage()
grid.draw(frame_new)
```
### gtable
gtable: https://cran.r-project.org/web/packages/gridExtra/vignettes/gtable.html
