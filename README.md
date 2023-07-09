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

## percent grob
```
percentGrob<-function(nn){
  a_rect_frame<-frameGrob()
  a_rect_frame<-packGrob(a_rect_frame,rectGrob(gp=gpar(col=NA,fill="coral1",alpha=0.5)),side="right")
  a_rect_frame<-packGrob(a_rect_frame,rectGrob(gp=gpar(col=NA,fill="lightsteelblue1",alpha=0.5)),side="right")
  a_pp<-nn[2]/sum(nn)
  gTree(children=gList(
    gTree(
      children=gList(a_rect_frame),
      vp=viewport(layout=grid.layout(ncol=2,nrow=1,widths=unit(c(a_pp,1-a_pp),"null")))),
    textGrob(paste0(num_to_str(a_pp*100,1),"%"))))
}

myborder<-unit(c(0.1,0.1,0.1,0.1),"lines")
symbolvp<-viewport(width=unit(1,"lines"))

mylegend1<-frameGrob()

for(ii in 1:nrow(df_npositive_ROC)){
  a_table<-df_npositive_ROC$table[[ii]]
  
  # col 1-2
  col1<-linesGrob(
    x=c(0,1),y=c(0.5,0.5),
    vp=symbolvp,gp=gpar(col=all_colors[ii],lwd=1.5))
  col2<-textGrob(df_npositive_ROC$name[ii])
  mylegend1<-packGrob(mylegend1,col1,row=ii,col=1,border=myborder)
  mylegend1<-packGrob(mylegend1,col2,row=ii,col=2,border=myborder)
  
  # col 3-6
  for(jj in 1:3){
    mylegend1<-packGrob(
      mylegend1,
      textGrob(sum(a_table[jj,])),
      row=ii,col=1+2*jj,border=myborder)
    mylegend1<-packGrob(
      mylegend1,
      percentGrob(a_table[jj,]),
      row=ii,col=2+2*jj,border=myborder)
  }
}

mylegend1_label<-frameGrob(layout=grid.layout(nrow=2,ncol=8))
mylegend1_label<-packGrob(mylegend1_label,textGrob(""),row=1,col=1:2)
mylegend1_label<-packGrob(mylegend1_label,textGrob("AB-"),row=1,col=3:4)
mylegend1_label<-packGrob(mylegend1_label,textGrob("1 AB+"),row=1,col=5:6)
mylegend1_label<-packGrob(mylegend1_label,textGrob("2+ AB+"),row=1,col=7:8)
mylegend1_label<-packGrob(mylegend1_label,textGrob(""),row=2,col=1)
mylegend1_label<-packGrob(mylegend1_label,textGrob("Assay"),row=2,col=2)
mylegend1_label<-packGrob(mylegend1_label,textGrob("n"),row=2,col=3)
mylegend1_label<-packGrob(mylegend1_label,textGrob("% T1D"),row=2,col=4)
mylegend1_label<-packGrob(mylegend1_label,textGrob("n"),row=2,col=5)
mylegend1_label<-packGrob(mylegend1_label,textGrob("% T1D"),row=2,col=6)
mylegend1_label<-packGrob(mylegend1_label,textGrob("n"),row=2,col=7)
mylegend1_label<-packGrob(mylegend1_label,textGrob("% T1D"),row=2,col=8)

mylegend1$framevp<-
  viewport(layout=grid.layout(
    nrow=5,ncol=8,
    widths=unit(c(1,6,2,3,2,3,2,3),"lines"),
    heights=unit(rep(1.5,5),"lines")))

mylegend1_label$framevp<-
  viewport(layout=grid.layout(
    nrow=2,ncol=8,
    widths=unit(c(1,6,2,3,2,3,2,3),"lines"),
    heights=unit(c(1.5,1.5),"lines")))

# stack
  mylegend<-frameGrob()
  mylegend<-packGrob(mylegend,mylegend1_label,side="bottom",border=unit(c(0,0,1,0),"lines"))
  mylegend<-packGrob(mylegend,mytable_ab_present,side="bottom")

```

## Simple Stack
```
grid.draw(gTree(
  children=gList(
    rectGrob(vp=viewport(layout.pos.row=1,layout.pos.col=1)),
    rectGrob(vp=viewport(layout.pos.row=1,layout.pos.col=2))),
  vp=viewport(layout=grid.layout(1,2))))
```
## best table example
```
df<-data.frame(a=c("AA","bb","Cc"),b=c(1,2.5,3),c=c(0.2,0.5,0.6))

mylegend<-frameGrob()
for(ii in 1:nrow(df)){
  
  col1<-gTree(children=gList(rectGrob(),textGrob(df$a[ii])))
  
  g1<-gTree(
    children=gList(rectGrob(),textGrob(df$b[ii])),
    vp=viewport(layout.pos.row=1,layout.pos.col=1))
  g2<-gTree(
    children=gList(rectGrob(),pointsGrob(x=unit(df$b[ii],"native"),y=unit(0.5,"native"),vp=viewport(xscale=c(0.8,3.2)))),
    vp=viewport(layout.pos.row=1,layout.pos.col=2))
  col2<-gTree(children=gList(g1,g2),vp=viewport(layout=grid.layout(1,2)))
  
  g1<-rectGrob(gp=gpar(fill="coral"),vp=viewport(layout.pos.row=1,layout.pos.col=1))
  g2<-rectGrob(gp=gpar(fill="deepskyblue"),vp=viewport(layout.pos.row=1,layout.pos.col=2))
  gb<-gTree(children=gList(g1,g2),vp=viewport(width=0.95,height=0.95,layout=grid.layout(1,2,widths=unit(c(df$c[ii],1-df$c[ii]),"null"))))
  col3<-gTree(children=gList(rectGrob(),gb,textGrob(df$c[ii])))
  
  mylegend<-packGrob(mylegend,col1,row=ii,col=1)
  mylegend<-packGrob(mylegend,col2,row=ii,col=2)
  mylegend<-packGrob(mylegend,col3,row=ii,col=3)
}

mylegend$framevp<-viewport(
  layout=grid.layout(3,3,widths=unit(c(6,4,4),"lines"),heights=unit(c(2,2,2),"lines")))
grid.draw(mylegend)
```
## Best legend example
```
  mylegend1<-frameGrob()
  for(ii in 1:nrow(df_ROC)){
    mylegend1<-packGrob(mylegend1,linesGrob(x=c(0,1),y=c(0.5,0.5),vp=symbolvp,gp=gpar(col=all_colors[ii],lwd=1.5)),row=ii,col=1,border=myborder)
    mylegend1<-packGrob(mylegend1,textGrob(df_ROC$name[ii],x=0,just="left"),row=ii,col=2,border=myborder)
  }

  mylegend2<-frameGrob()
  mylegend2<-packGrob(mylegend2,pointsGrob(x=0.5,y=0.5,vp=symbolvp,size=unit(7,"points"),pch=24,gp=gpar(col="black",fill="white",lwd=1.5)),row=1,col=1,border=myborder)
  mylegend2<-packGrob(mylegend2,pointsGrob(x=0.5,y=0.5,vp=symbolvp,size=unit(7,"points"),pch=21,gp=gpar(col="white",fill="black",lwd=1.5)),row=2,col=1,border=myborder)
  mylegend2<-packGrob(mylegend2,textGrob("Positive Cutoff",x=0,just="left"),row=1,col=2,border=myborder)
  mylegend2<-packGrob(mylegend2,textGrob("Youden's Optimal Cutoff",x=0,just="left"),row=2,col=2,border=myborder)
  
  pushViewport(viewport(
    x=1,y=0,
    width=unit(1,"grobwidth",mylegend1)+unit(2,"lines"),
    height=unit(1,"grobheight",mylegend1)+unit(2,"lines"),
    just=c(1,0)))
```
## for survival plot
```
dostep<-function(x,y){
  n<-length(x)
  xrep<-rep(x,c(1,rep(2,n-1)))
  yrep<-rep(y,c(rep(2,n-1),1))
  result<-list(x=xrep,y=yrep)
  return(result)
}
```
## center a text box
```
grid.newpage()
pushViewport(viewport())
a_textGrob<-textGrob(x=0,"AAA\nBBBBB\nCCCCCCCC\nDD",just="left")
pushViewport(viewport(
  width=unit(1,"grobwidth",a_textGrob),
  height=unit(1,"grobheight",a_textGrob)))
grid.rect()
grid.draw(a_textGrob)
```

## flexible text box
```
value_to_axis<-function(value,at){
  rangey<-range(value,na.rm=T)
  a_fit<-spline(1:length(at),at,method="hyman")
  xnew<-a_fit$x
  ynew<-a_fit$y
  if(rangey[1]<ynew[1]-1e-8){
    slope<-(ynew[2]-ynew[1])/(xnew[2]-xnew[1])
    a_y<-rangey[1]
    a_x<-xnew[1]+(a_y-ynew[1])/slope
    ynew<-c(a_y,ynew)
    xnew<-c(a_x,xnew)
  }
  if(rangey[2]>ynew[length(ynew)]+1e-8){
    slope<-
      (ynew[length(ynew)]-ynew[length(ynew)-1])/
      (xnew[length(ynew)]-xnew[length(ynew)-1])
    a_y<-rangey[2]
    a_x<-xnew[length(xnew)]+(a_y-ynew[length(xnew)])/slope
    ynew<-c(ynew,a_y)
    xnew<-c(xnew,a_x)
  }
  axis2value<-approxfun(xnew,ynew,rule=2)
  value2axis<-approxfun(ynew,xnew,rule=2)
  return(value2axis(value))
}
```

## rotate axis
```
        grid.xaxis(at=tick,label=rep("",length(tick)),gp=gpar(cex=0.6))
        grid.text(
          label=list_tick[[idx_xx]],rot=90,
          x=unit(tick,"native"),y=unit(-0.75,"char"),
          just="right",gp=gpar(cex=0.6))

        grid.yaxis(at=tick,label=rep("",length(tick)),gp=gpar(cex=0.6))
        grid.text(
          label=list_tick[[idx_yy]],
          x=unit(-0.75,"char"),y=unit(tick,"native"),
          just="right",gp=gpar(cex=0.6))

```

## matrix plot
```
continuous_correlation<-function(df_data,df_pos,list_at,list_tick,df_names){
  grid.newpage()
  # my_layout<-matrix(1:(ncol(df_data)^2),ncol(df_data),ncol(df_data))
  my_layout<-grid.layout(ncol(df_data),ncol(df_data))
  # pushViewport(viewport(x=0.52,y=0.52,width=0.85,height=0.85))
  pushViewport(plotViewport(margins=c(4,4,1,1)))
  pushViewport(viewport(layout=my_layout))
  # df_rank<-df_data%>%mutate_all(percent_rank)
  for(idx_xx in 1:ncol(df_data)){
    for(idx_yy in 1:ncol(df_data)){
      # my_pushViewport(my_layout[idx_yy,idx_xx],my_layout,xscale=c(-0.1,1.1),yscale=c(-0.1,1.1))
      if(idx_xx==idx_yy){
        ## name panel
        xx<-value_to_axis(df_data[[idx_xx]],list_at[[idx_xx]])
        yy<-value_to_axis(df_data[[idx_yy]],list_at[[idx_yy]])
        pushViewport(dataViewport(
          layout.pos.row=idx_yy,
          layout.pos.col=idx_xx,xData=xx,yData=yy))
        grid.rect()
        grid.text(df_names[idx_xx])
      }else if(idx_xx<idx_yy){
        ## scatter panel
        xx<-value_to_axis(df_data[[idx_xx]],list_at[[idx_xx]])
        yy<-value_to_axis(df_data[[idx_yy]],list_at[[idx_yy]])
        xxp<-df_pos[[idx_xx]]
        yyp<-df_pos[[idx_yy]]
        pushViewport(dataViewport(
          layout.pos.row=idx_yy,
          layout.pos.col=idx_xx,
          xData=c(xx,value_to_axis(list_tick[[idx_xx]],list_at[[idx_xx]])),
          yData=c(yy,value_to_axis(list_tick[[idx_yy]],list_at[[idx_yy]]))))
        grid.rect()
        grid.points(
          x=unit(xx,"native"),y=unit(yy,"native"),size=unit(0.1,"picas"),pch=16,
          gp=gpar(col=ifelse(xor(xxp,yyp),"darkslategray4","brown3")))
        grid.lines(x=unit(max(xx[!xxp],na.rm=TRUE),"native"),gp=gpar(lty=3))
        grid.lines(y=unit(max(yy[!yyp],na.rm=TRUE),"native"),gp=gpar(lty=3))
      }else{
        ## correlation panel
        pushViewport(viewport(
          layout.pos.row=idx_yy,
          layout.pos.col=idx_xx,xscale=c(-0.1,1.1),yscale=c(-0.1,1.1)))
        grid.rect()
        xx<-df_data[[idx_xx]]
        yy<-df_data[[idx_yy]]
        plot_cor_circle(cor(xx,yy,method="spearman",use="pairwise.complete.obs"))
      }
      if(idx_yy==nrow(my_layout)){
        tick<-value_to_axis(list_tick[[idx_xx]],at=list_at[[idx_xx]])
        grid.xaxis(at=tick,label=rep("",length(tick)),gp=gpar(cex=0.6))
        grid.text(
          label=list_tick[[idx_xx]],rot=90,
          x=unit(tick,"native"),y=unit(-0.75,"char"),
          just="right",gp=gpar(cex=0.6))
      }
      if(idx_xx==1){
        tick<-value_to_axis(list_tick[[idx_yy]],at=list_at[[idx_yy]])
        grid.yaxis(at=tick,label=rep("",length(tick)),gp=gpar(cex=0.6))
        grid.text(
          label=list_tick[[idx_yy]],
          x=unit(-0.75,"char"),y=unit(tick,"native"),
          just="right",gp=gpar(cex=0.6))
      }
      popViewport(1)
    }
  }
  grid.text("Scaled Value in Percentile",x=0.5,y=unit(-2.5,"lines"))
  grid.text("Scaled Value in Percentile",x=unit(-2.5,"lines"),y=0.5,rot=90)
}
```

## Right align text
```
textGrob(df_temp$name[ii],x=1,just="right")
```

## viewport stack
```
tree <- vpTree(viewport(w=0.8, h=0.8, name="A"),
               vpList(vpStack(viewport(x=0.1, y=0.1, w=0.5, h=0.5,
                                       just=c("left", "bottom"), name="B"),
                              viewport(x=0.1, y=0.1, w=0.5, h=0.5,
                                       just=c("left", "bottom"), name="C"),
                              viewport(x=0.1, y=0.1, w=0.5, h=0.5,
                                       just=c("left", "bottom"), name="D")),
                      viewport(x=0.5, w=0.4, h=0.9,
                               just="left", name="E")))

```
