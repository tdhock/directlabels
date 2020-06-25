dlcompare <- structure(function # Direct label comparison plot
### Compare several plots and/or label placement methods. This creates
### a custom grid graphics display based on lattice and/or ggplot2
### output. Plots will be on the columns and positioning methods will
### be on the rows.
(plots,
### List of ggplot2 or lattice plots. List names will be used to
### annotate the plot.
 pos.funs,
### List of label placement methods to apply to each plot. List names,
### or function names if specified as character strings, will be used
### to annotate the plot.
 rects=TRUE,
### Draw rectangles around each plot, creating a grid?
 row.items="plots",
### If "plots" then put plots on the rows and method on the
### columns. Otherwise, do the opposite.
 debug=FALSE
### Show debug output?
 ){
  ## Augment positioning method list names if possible
  names(pos.funs) <- sapply(seq_along(pos.funs),function(i){
    N <- names(pos.funs)[i]
    f <- pos.funs[[i]]
    if(!is.null(N)&&N!="")N
    else if(class(f)=="character")f
    else ""
  })
  if(sum(names(pos.funs)!="")==0)names(pos.funs) <- NULL
  grid.newpage()

  standard <- row.items=="plots"
  row.items <- if(standard)plots else pos.funs
  col.items <- if(standard)pos.funs else plots
  
  rowadd <- if(is.null(names(col.items)))0 else 1
  widths <- rep("null",l=length(col.items))
  if(!is.null(names(row.items)))widths <- c(widths,"cm")
  heights <- rep("null",l=length(row.items))
  if(rowadd)heights <- c("cm",heights)
  the.layout <-
    grid.layout(length(heights),length(widths),
                widths=unit(1,widths),
                heights=unit(1,heights))
  pushViewport(viewport(layout=the.layout))

  for(col in seq_along(col.items)){
    if(!is.null(names(col.items))){
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=1))
      grid.text(
        names(col.items)[col],
        name=paste0("dlcompare.text.col.", col))
      popViewport()
    }
    for(row in seq_along(row.items)){
      if(col==1&&!is.null(names(row.items))){
        pushViewport(viewport(layout.pos.col=length(col.items)+1,
                              layout.pos.row=row+rowadd))
        grid.text(
          names(row.items)[row],rot=-90,
          name=paste0("dlcompare.text.row", row))
        popViewport()
      }
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=row+rowadd))
      p <- if(standard)
        direct.label(row.items[[row]],col.items[[col]],debug=debug)
      else direct.label(col.items[[col]],row.items[[row]],debug=debug)
      print(p,newpage=FALSE)
      if(rects)grid.rect(name=paste0("dlcompare.rect.row.", row))
      popViewport()
    }
  }
  popViewport()
},ex=function(){
  library(lattice)
  oldopt <- lattice.options(panel.error=NULL)

  ## Compare two plots of the same data using lattice and ggplot2.
  deaths.by.sex <- list(male=mdeaths, female=fdeaths)
  deaths.list <- list()
  for(sex in names(deaths.by.sex)){
    deaths.ts <- deaths.by.sex[[sex]]
    deaths.list[[sex]] <-
      data.frame(year=as.numeric(time(deaths.ts)),
                 sex,
                 deaths=as.integer(deaths.ts))
  }
  deaths <- do.call(rbind, deaths.list)
  death.plot.list <-
    list(lattice=xyplot(deaths~year,deaths,groups=sex,type="l"))
  if(require(ggplot2)){
    death.plot.list$ggplot2 <- 
      qplot(year,deaths,data=deaths,colour=sex,geom="line")
  }

  if(names(dev.cur())!="postscript"){##to avoid error on pkg check.
    ## Use some exotic labeling options with different rotation, font
    ## face, family, and alpha transparency.
    exotic <- list("last.points",
                   rot=c(0,180),
                   fontsize=c(10,20),
                   fontface=c("bold","italic"),
                   fontfamily=c("mono","serif"),
                   alpha=c(0.25,1))
    dlcompare(death.plot.list, list(exotic))
  }
    
  lattice.options(oldopt)

  ## Compare a legend with direct labels on the same plot.
  library(nlme)
  if(require(ggplot2)){
    ggrat <- qplot(Time,weight,data=BodyWeight,
                   colour=Rat,geom="line",facets=.~Diet)
    pfuns <- list("legend","direct labels"="last.qp")
    dlcompare(list(ggrat),pfuns,rects=FALSE,row.items="posfuns")
  }
})
