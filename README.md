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
