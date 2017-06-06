#' @title Bayesian Analysis of DSD
#'
#' @author Victor Manuel Aguirre-Torres, Nery Sofia Huerta-Pacheco, Edgar A. Lopez
#'
#' @description Definitive Screening Designs are a class of experimental designs that under factor sparsity have the potential to estimate linear, quadratic and interaction effects with little experimental effort. BAYESDEF is a package that performs a five step strategy to analyze this kind of experiments that makes use of tools coming from the Bayesian approach. It also includes the least absolute shrinkage and selection operator (lasso) as a check (Aguirre VM. (2016) <DOI:10.1002/asmb.2160>).
#'
#' You can learn more about this package at:
#' http://www.uv.mx/personal/nehuerta/bayesdef/
#'
#' @details
#'
#' BAYESDEF is a package with a graphical interface dedicated to perform Bayesian analysis of
#' Definitive Screening Designs with thirteen runs. These very economic experimental plans are
#' gaining popularity because, under certain conditions, they allow the estimation of main,
#' interaction and quadratic effects. Tinhe package also allows the user to fit custom models to
#' the data. It also includes the additional feature to analyze the data using the least absolute
#' shrinkage and selection operator "lasso".
#' Note: BAYESDEF is free software and comes with ABSOLUTELY NO WARRANTY.

#' @return BAYESDEF is a graphic interface
#' @examples \dontrun{
#' ##Install package
#' library(BAYESDEF)
#' ##Call the package
#' BAYESDEF()
#' }
#'
#' @references
#' Aguirre VM. Bayesian analysis of definitive screening designs when the response
#' is nonnormal. Applied Stochastic Models in Business and Industry 2016; 32(4):440–452.
#' DOI: 10.1002/asmb.2160
#'
#' Aguirre VM, de la Vara R. A Bayesian analysis of very small unreplicated experiments.
#' Quality and Reliability Engineering International 2014a; 30(3):413–426.
#' DOI: 10.1002/qre.1578
#'
#' Friedman J, Hastie T, Tibshirani R. Regularization paths for generalized linear
#' models via coordinate descent. Journal of Statistical Software 2010; 33:1–22.
#' DOI: 10.18637/jss.v033.i01
#'
#' Jones B, Nachtsheim C. A class of three-level designs for definitive screening in
#' the presence of second order effects. Journal of Quality Technology 2011; 43:1–15.
#'
#' Tibshirani R. Regression shrinkage and selection via the lasso. Journal of the
#' Royal Statistical Society B 1996; 58:267–288. DOI:10.1111/j.1467-9868.2011.00771.x
#'
#'
#' @export BAYESDEF
#' @import graphics
#' @import grDevices
#' @import utils
#' @import tcltk
#' @import gWidgets
#' @import readxl
#' @import REdaS
#' @import glmnet
#' @importFrom stats coef dt pt qt var

BAYESDEF<-function(){

  mi<- new.env()
  options("guiToolkit"="tcltk")

  ##Screen
  w<- gwindow("BAYESDEF",visible=FALSE,width = 800,height= 510)
  g<- ggroup(horizontal=FALSE, spacing=0, container = w)

  nb <- gnotebook(container=g,width = 800,height= 500)
  g0<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "BAYESDEF")
  g1<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Design")
  g2<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Bayesian")
  g3<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Lasso")
  g4<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Custom")

  #GLOBAL VARIABLES
  assign("gdata",NULL, envir =mi)
  assign("gdata1",NULL, envir =mi)
  assign("YY1",5, envir =mi)
  assign("YY2",20, envir =mi)
  assign("alpha",0.05, envir =mi)
  assign("Vari",NULL, envir =mi)
  assign("n",NULL, envir =mi)
  assign("m",NULL, envir =mi)
  assign("a",NULL, envir =mi)
  assign("na",NULL, envir =mi)
  assign("namesmat",NULL, envir =mi)
  assign("names2",NULL,envir =mi)
  assign("anames",NULL,envir =mi)
  assign("V1",NULL, envir =mi)
  assign("dt1",NULL, envir = mi)
  assign("matdes",NULL, envir = mi)
  assign("desing",NULL,envir=mi)
  assign("desing1",NULL,envir=mi)
  assign("desing2",NULL,envir=mi)

  ##MENU - OPEN
  #Open
  abrirc<-function(h,...){
    data<-tk_choose.files()
    data1<-read.csv(data)
    assign("gdata",data1, envir =mi)
  }

  abrirt<-function(h,...){
    data<-tk_choose.files()
    data1<-read.table(data,header=TRUE)
    assign("gdata",data1, envir =mi)
  }

  #Open xlsx
  openex<-function(h,...){
    data<-tk_choose.files()
    xlsx<-read_excel(data,sheet = 1, col_names=TRUE)
    data2<-as.data.frame(xlsx)
    assign("gdata",data2, envir =mi)
  }

  ##View
  ver<-function(h,...){
    gdata<-get("gdata",envir =mi)
    fix(gdata)
  }

  ##Re-start
  inicio<-function(h,...){
    dispose(w)
    BAYESDEF()
  }

  ##Read
  read1 <- function(h, ...) {
    w1 <- tktoplevel()
    tkwm.title(w1, "Odd Value")
    frame <- ttkframe(w1, padding = c(5, 5, 30, 30))
    tkpack(frame, expand = TRUE, fill = "both")
    nested_frame <- ttkframe(frame)
    tkpack(nested_frame)
    label <- ttklabel(nested_frame, text = "Value:")
    tkpack(label, side = "left")
    text_var <- tclVar("")
    entry <- ttkentry(nested_frame, textvariable = text_var)
    tkpack(entry)
    button_frame <- ttkframe(frame)
    tkpack(button_frame, anchor = "ne")
    button <- ttkbutton(button_frame, text = "Save")
    button1 <- ttkbutton(button_frame, text = "Ok", command = function() tkdestroy(w1))
    sapply(list(button, button1), tkpack)
    yyy <- function() {
      msg <- tclvalue(text_var)
      assign("YY1", msg, envir = mi)
    }
    tkconfigure(button, command = yyy)
    tkconfigure(button1, underline = 0)
  }

  read2 <- function(h, ...) {
    w1 <- tktoplevel()
    tkwm.title(w1, "Odd Value")
    frame <- ttkframe(w1, padding = c(5, 5, 30, 30))
    tkpack(frame, expand = TRUE, fill = "both")
    nested_frame <- ttkframe(frame)
    tkpack(nested_frame)
    label <- ttklabel(nested_frame, text = "Value:")
    tkpack(label, side = "left")
    text_var <- tclVar("")
    entry <- ttkentry(nested_frame, textvariable = text_var)
    tkpack(entry)
    button_frame <- ttkframe(frame)
    tkpack(button_frame, anchor = "ne")
    button <- ttkbutton(button_frame, text = "Save")
    button1 <- ttkbutton(button_frame, text = "Ok", command = function() tkdestroy(w1))
    sapply(list(button, button1), tkpack)
    yyy <- function() {
      msg <- tclvalue(text_var)
      assign("YY2", msg, envir = mi)
    }
    tkconfigure(button, command = yyy)
    tkconfigure(button1, underline = 0)
  }

  #Save
  save1<-function(h,...){
    gdata1<-get("desing1",envir =mi)
    gdata2<-get("desing2",envir =mi)
    filename <- tclvalue(tkgetSaveFile())
    nam<-paste0(filename,"(DSD)",".csv")
    nam1<-paste0(filename,"(Random)",".csv")
    write.csv(gdata1, file =nam)
    write.csv(gdata2, file =nam1)
  }

  #Custom
  main<-function(h, ...) {

    X0 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
    A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
    B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
    C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
    D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
    E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
    F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
    AB <- A.*B.;BA <- A.*B.;AC <- A.*C.; CA <- A.*C.;AD <- A.*D.;AE <- A.*E.;AF <- A.*F.;
    BC <- B.*C.;BD <- B.*D.;BE <- B.*E.;BF <- B.*F.;CD <- C.*D.;CE <- C.*E.;CF<- C.*F.;
    DE<- D.*E.;DF<- D.*F.;EF<- E.*F.;DA <- A.*D.;EA <- A.*E.;FA <- A.*F.;CB <- B.*C.;
    DB <- B.*D.;EB <- B.*E.;FB <- B.*F.;DC <- C.*D.;EC <- C.*E.;FC<- C.*F.;ED<- D.*E.;FD<- D.*F.;
    FE<- E.*F.;AA<- A.*A.;BB <-B.*B.;CC <- C.*C.;DD <-D.*D.;EE <- E.*E.;FF<- F.*F.;
    base_datos <-cbind(A.,B.,C.,D.,E.,F.,AB,AC,AD,AE,AF,BC,BD,BE,BF,CD,CE,CF,DE,DF,EF,AA,BB,CC,DD,EE,FF)
    gdata<-as.data.frame(base_datos)

    tt1<-tktoplevel()
    tkwm.title(tt1, "Effects")
    scr <- tkscrollbar(tt1, repeatinterval=5,
                       command=function(...)tkyview(tl1,...))
    tl1<-tklistbox(tt1,width=35,height=6,selectmode="multiple",background="white",yscrollcommand=function(...)tkset(scr,...))
    tkgrid(tklabel(tt1,text="Effects"))
    tkgrid(tl1,scr)
    tkgrid.configure(scr,rowspan=4,sticky="nsw")
    a<-length(gdata)
    method<- c(names(gdata))

    for (i in (1:a)){
      tkinsert(tl1,"end",method[i])
    }

    tkselection.set(tl1,0)
    OnOK <- function(){
      choice<-method[as.numeric(tkcurselection(tl1))+1]
      x<-choice
      assign("dt1", x, envir = mi)
      tkdestroy(tt1)
    }

    OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
    tkgrid(OK.but)
  }

  ##Parameters
  #Bayes
  parm<-function(h,...){
    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =mi)
    var<- colnames(gdata)
    assign("Vari",var, envir =mi)
    var1<-c("Null",var)
    tbl[1,1] <- "Response"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Alpha=0.05"
    tbl[2,1] <- "Odds ratio"
    tbl[3,1] <- "First Step"
    tbl[3,2] <- (cb2 <- gbutton("Change", container=tbl,handler=read1))
    tbl[3,3] <- "Next Steps"
    tbl[3,4] <- (cb3 <- gbutton("Change", container=tbl,handler=read2))
    tbl[4,2] <- "Default: 5"
    tbl[4,4] <- "Default: 20"

    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(b, handler=function(h,...) {
      na1<-svalue(cb1)
      assign("na",na1, envir =mi)
      na<-get("na",envir =mi)
      n1<-ncol(gdata)

      vta<-na
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }
      assign("a",a, envir =mi)
      assign("V1",c1, envir =mi)
      # Count factor a
      ta<-table(a)
      naa<-nrow(as.matrix(ta))
      anames<-rownames(as.matrix(ta))
      assign("naa",naa, envir =mi)
      assign("anames",anames, envir =mi)

      dispose(w1)
      BAYES()
    })
    visible(w1) <- TRUE
  }

  #Custom
  parm1<-function(h,...){
    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =mi)
    var<- colnames(gdata)
    assign("Vari",var, envir =mi)
    var1<-c("Null",var)
    tbl[1,1] <- "Response"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Effects"
    tbl[1,4] <- (cb3 <- gbutton("Choose", container=tbl,handler=main))
    tbl[3,2] <- "Alpha=0.05"

    tbl[3,3] <- (a <- gbutton("Cancel", container=tbl))
    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    tbl[3,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(b, handler=function(h,...) {
      na1<-svalue(cb1)
      assign("na",na1, envir =mi)
      dt1<-get("dt1",envir =mi)
      na<-get("na",envir =mi)
      n1<-ncol(gdata)

      vta<-na
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }
      assign("a",a, envir =mi)
      assign("V1",c1, envir =mi)
      # Count factor a
      ta<-table(a)
      naa<-nrow(as.matrix(ta))
      anames<-rownames(as.matrix(ta))
      assign("naa",naa, envir =mi)
      assign("anames",anames, envir =mi)

      dispose(w1)
      CUSTOM()
    })
    visible(w1) <- TRUE
  }

  #Lasso
  parm2<-function(h,...){
    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =mi)
    var<- colnames(gdata)
    assign("Vari",var, envir =mi)
    var1<-c("Null",var)
    tbl[1,1] <- "Response"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))


    tbl[2,3] <- (a <- gbutton("Cancel", container=tbl))
    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    tbl[2,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(b, handler=function(h,...) {
      na1<-svalue(cb1)
      assign("na",na1, envir =mi)
      na<-get("na",envir =mi)
      n1<-ncol(gdata)

      vta<-na
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }
      assign("a",a, envir =mi)
      assign("V1",c1, envir =mi)
      # Count factor a
      ta<-table(a)
      naa<-nrow(as.matrix(ta))
      anames<-rownames(as.matrix(ta))
      assign("naa",naa, envir =mi)
      assign("anames",anames, envir =mi)

      dispose(w1)
      LASSO()
    })
    visible(w1) <- TRUE
  }

  #Desing
  parm3<-function(h,...){

    Constant <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
    A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
    B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
    C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
    D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
    E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
    F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
    AB <- A.*B.;BA <- A.*B.;AC <- A.*C.; CA <- A.*C.;AD <- A.*D.;AE <- A.*E.;AF <- A.*F.;
    BC <- B.*C.;BD <- B.*D.;BE <- B.*E.;BF <- B.*F.;CD <- C.*D.;CE <- C.*E.;CF<- C.*F.;
    DE<- D.*E.;DF<- D.*F.;EF<- E.*F.;DA <- A.*D.;EA <- A.*E.;FA <- A.*F.;CB <- B.*C.;
    DB <- B.*D.;EB <- B.*E.;FB <- B.*F.;DC <- C.*D.;EC <- C.*E.;FC<- C.*F.;ED<- D.*E.;FD<- D.*F.;
    FE<- E.*F.;AA<- A.*A.;BB <-B.*B.;CC <- C.*C.;DD <-D.*D.;EE <- E.*E.;FF<- F.*F.;
    Quadratic <-cbind(AA,BB,CC,DD,EE,FF)
    Interaction <-cbind(AB,AC,AD,AE,AF,BC,BD,BE,BF,CD,CE,CF,DE,DF,EF)
    Main1<-cbind(A.,B.,C.,D.,E.,F.)

    w1<- gwindow("Desing",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    var1<-c("Null",var)
    tbl[1,c(1:5)]<-(glx0<-glabel("Definitive screening design for m = 6 factors",container=tbl))
    font(glx0) <- list(weight="bold",size= 9,family="sans",align ="center",spacing = 5)

    #tbl[1,2] <- "Effects"
    #tbl[1,3] <- (cb3 <- gbutton("Choose", container=tbl,handler=main))
    tbl[2,2]<-(glx1<-glabel("Effects",container=tbl))
    font(glx1) <- list(weight="bold",size= 9,family="sans",align ="center",spacing = 5)

    tbl[3,2]<-glabel("A",container=tbl)
    tbl[4,2]<-glabel("B",container=tbl)
    tbl[5,2]<-glabel("C",container=tbl)
    tbl[6,2]<-glabel("D",container=tbl)
    tbl[7,2]<-glabel("E",container=tbl)
    tbl[8,2]<-glabel("F",container=tbl)
    tbl[2,c(3,4)]<-(glx2<-glabel("Factors",container=tbl))
    font(glx2) <- list(weight="bold",size= 9,family="sans",align ="center",spacing = 5)

    tbl[3,c(3,4)]<-(ge1<-gedit("",container=tbl))
    tbl[4,c(3,4)]<-(ge2<-gedit("",container=tbl))
    tbl[5,c(3,4)]<-(ge3<-gedit("",container=tbl))
    tbl[6,c(3,4)]<-(ge4<-gedit("",container=tbl))
    tbl[7,c(3,4)]<-(ge5<-gedit("",container=tbl))
    tbl[8,c(3,4)]<-(ge6<-gedit("",container=tbl))
    tbl[2,5]<-(gl1<-glabel("   Units",container=tbl))
    font(gl1) <- list(weight="bold",size= 9,family="sans",align ="center",spacing = 5)

    tbl[3,5]<-(tg01<-gedit("",container=tbl))
    size(tg01)<-c(50,50)
    tbl[4,5]<-(tg02<-gedit("",container=tbl))
    size(tg02)<-c(50,50)
    tbl[5,5]<-(tg03<-gedit("",container=tbl))
    size(tg03)<-c(50,50)
    tbl[6,5]<-(tg04<-gedit("",container=tbl))
    size(tg04)<-c(50,50)
    tbl[7,5]<-(tg05<-gedit("",container=tbl))
    size(tg05)<-c(50,50)
    tbl[8,5]<-(tg06<-gedit("",container=tbl))
    size(tg06)<-c(50,50)
    tbl[1,c(6,8)]<-(gla1<-glabel("                          Levels",container=tbl))
    font(gla1) <- list(weight="bold",size= 9,family="sans",align ="center",spacing = 5)
    tbl[2,6]<-(gla2<-glabel("        -1",container=tbl))
    font(gla2) <- list(weight="bold",size= 8,family="sans",align ="center",spacing = 5)
    tbl[2,7]<-(gla3<-glabel("         0",container=tbl))
    font(gla3) <- list(weight="bold",size= 8,family="sans",align ="center",spacing = 5)
    tbl[2,8]<-(gla4<-glabel("         1",container=tbl))
    font(gla4) <- list(weight="bold",size= 8,family="sans",align ="center",spacing = 5)

    tbl[3,6]<-(tg11<-gedit("",container=tbl))
    size(tg11)<-c(50,50)
    tbl[4,6]<-(tg12<-gedit("",container=tbl))
    size(tg12)<-c(50,50)
    tbl[5,6]<-(tg13<-gedit("",container=tbl))
    size(tg13)<-c(50,50)
    tbl[6,6]<-(tg14<-gedit("",container=tbl))
    size(tg14)<-c(50,50)
    tbl[7,6]<-(tg15<-gedit("",container=tbl))
    size(tg15)<-c(50,50)
    tbl[8,6]<-(tg16<-gedit("",container=tbl))
    size(tg16)<-c(50,50)

    tbl[3,7]<-(tg21<-gedit("",container=tbl))
    size(tg21)<-c(50,50)
    tbl[4,7]<-(tg22<-gedit("",container=tbl))
    size(tg22)<-c(50,50)
    tbl[5,7]<-(tg23<-gedit("",container=tbl))
    size(tg23)<-c(50,50)
    tbl[6,7]<-(tg24<-gedit("",container=tbl))
    size(tg24)<-c(50,50)
    tbl[7,7]<-(tg25<-gedit("",container=tbl))
    size(tg25)<-c(50,50)
    tbl[8,7]<-(tg26<-gedit("",container=tbl))
    size(tg26)<-c(50,50)

    tbl[3,8]<-(tg31<-gedit("",container=tbl))
    size(tg31)<-c(50,50)
    tbl[4,8]<-(tg32<-gedit("",container=tbl))
    size(tg32)<-c(50,50)
    tbl[5,8]<-(tg33<-gedit("",container=tbl))
    size(tg33)<-c(50,50)
    tbl[6,8]<-(tg34<-gedit("",container=tbl))
    size(tg34)<-c(50,50)
    tbl[7,8]<-(tg35<-gedit("",container=tbl))
    size(tg35)<-c(50,50)
    tbl[8,8]<-(tg36<-gedit("",container=tbl))
    size(tg36)<-c(50,50)

     tbl[10,6] <- (c <- gbutton("Save", container=tbl))
    addHandlerClicked(c, handler=function(h,...) {
      f1<-svalue(ge1)
      f2<-svalue(ge2)
      f3<-svalue(ge3)
      f4<-svalue(ge4)
      f5<-svalue(ge5)
      f6<-svalue(ge6)
      fac<-matrix(c(f1,f2,f3,f4,f5,f6),6,1)
      u1<-svalue(tg01)
      u2<-svalue(tg02)
      u3<-svalue(tg03)
      u4<-svalue(tg04)
      u5<-svalue(tg05)
      u6<-svalue(tg06)
      uni<-matrix(c(u1,u2,u3,u4,u5,u6),6,1)
      l1<-svalue(tg11)
      l2<-svalue(tg12)
      l3<-svalue(tg13)
      l4<-svalue(tg14)
      l5<-svalue(tg15)
      l6<-svalue(tg16)
      level1<-matrix(c(l1,l2,l3,l4,l5,l6),6,1)
      e1<-svalue(tg21)
      e2<-svalue(tg22)
      e3<-svalue(tg23)
      e4<-svalue(tg24)
      e5<-svalue(tg25)
      e6<-svalue(tg26)
      level2<-matrix(c(e1,e2,e3,e4,e5,e6),6,1)
      v1<-svalue(tg31)
      v2<-svalue(tg32)
      v3<-svalue(tg33)
      v4<-svalue(tg34)
      v5<-svalue(tg35)
      v6<-svalue(tg36)
      level3<-matrix(c(v1,v2,v3,v4,v5,v6),6,1)
      effect<-c("A","B","C","D","E","F")
      matdes<-cbind(effect,fac,uni,level1,level2,level3)
      colnames(matdes)<-c("Effects","Factors","Units","Level -1","Level 0", "Level 1")
      rownames(matdes)<-c(1:6)
      assign("matdes",matdes, envir = mi)
      matdes<-get("matdes",envir = mi)
      A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
      B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
      C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
      D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
      E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
      F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
      mtx <-cbind(A.,B.,C.,D.,E.,F.)

      mdes<-matrix(,13,6)
      for(i in 1:13){
        if(mtx[i,1]==-1){
          mdes[i,1]<-matdes[1,4]
        }
        if(mtx[i,1]==0){
          mdes[i,1]<-matdes[1,5]
        }
        if(mtx[i,1]==1){
          mdes[i,1]<-matdes[1,6]
        }
      }
      for(i in 1:13){
        if(mtx[i,2]==-1){
          mdes[i,2]<-matdes[2,4]
        }
        if(mtx[i,2]==0){
          mdes[i,2]<-matdes[2,5]
        }
        if(mtx[i,2]==1){
          mdes[i,2]<-matdes[2,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,3]==-1){
          mdes[i,3]<-matdes[3,4]
        }
        if(mtx[i,3]==0){
          mdes[i,3]<-matdes[3,5]
        }
        if(mtx[i,3]==1){
          mdes[i,3]<-matdes[3,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,4]==-1){
          mdes[i,4]<-matdes[4,4]
        }
        if(mtx[i,4]==0){
          mdes[i,4]<-matdes[4,5]
        }
        if(mtx[i,4]==1){
          mdes[i,4]<-matdes[4,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,5]==-1){
          mdes[i,5]<-matdes[5,4]
        }
        if(mtx[i,5]==0){
          mdes[i,5]<-matdes[5,5]
        }
        if(mtx[i,5]==1){
          mdes[i,5]<-matdes[5,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,6]==-1){
          mdes[i,6]<-matdes[6,4]
        }
        if(mtx[i,6]==0){
          mdes[i,6]<-matdes[6,5]
        }
        if(mtx[i,6]==1){
          mdes[i,6]<-matdes[6,6]
        }
      }
      colnames(mdes)<-c(paste0(matdes[,1],"-",matdes[,2],"(",matdes[,3],")"))
      rownames(mdes)<-c(1:13)
      run<-matrix(c(1:13),13,1)
      colnames(run)<-"Run"
      mrun<-cbind(run,mdes)

     mu<-sample(c(1:13),13,replace = FALSE)

      x0<-matrix(1,1,6)
        for(i in 1:13){
        v<-mu[i]
        x<-mdes[v,]
        x0<-rbind(x0,x)
      }
      mrunax<-x0[-1,]
      rownames(mrunax)<-c(1:13)
      colnames(mrunax)<-colnames(mdes)
      runs<-matrix(c(mu),13,1)
      colnames(runs)<-"Run"
      mruna<-cbind(runs,mrunax)

      m1<-as.data.frame(matdes)
      m2<-as.data.frame(mrun)
      m3<-as.data.frame(mruna)
      desing1<-m2
      desing2<-m3
      assign("desing1",desing1, envir = mi)
      assign("desing2",desing2, envir = mi)
      save1()
    })

    tbl[10,7] <- (a <- gbutton("Cancel", container=tbl))
    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    tbl[10,8] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(b, handler=function(h,...) {
      f1<-svalue(ge1)
      f2<-svalue(ge2)
      f3<-svalue(ge3)
      f4<-svalue(ge4)
      f5<-svalue(ge5)
      f6<-svalue(ge6)
      fac<-matrix(c(f1,f2,f3,f4,f5,f6),6,1)
      u1<-svalue(tg01)
      u2<-svalue(tg02)
      u3<-svalue(tg03)
      u4<-svalue(tg04)
      u5<-svalue(tg05)
      u6<-svalue(tg06)
      uni<-matrix(c(u1,u2,u3,u4,u5,u6),6,1)
      l1<-svalue(tg11)
      l2<-svalue(tg12)
      l3<-svalue(tg13)
      l4<-svalue(tg14)
      l5<-svalue(tg15)
      l6<-svalue(tg16)
      level1<-matrix(c(l1,l2,l3,l4,l5,l6),6,1)
      e1<-svalue(tg21)
      e2<-svalue(tg22)
      e3<-svalue(tg23)
      e4<-svalue(tg24)
      e5<-svalue(tg25)
      e6<-svalue(tg26)
      level2<-matrix(c(e1,e2,e3,e4,e5,e6),6,1)
      v1<-svalue(tg31)
      v2<-svalue(tg32)
      v3<-svalue(tg33)
      v4<-svalue(tg34)
      v5<-svalue(tg35)
      v6<-svalue(tg36)
      level3<-matrix(c(v1,v2,v3,v4,v5,v6),6,1)
      effect<-c("A","B","C","D","E","F")
      matdes<-cbind(effect,fac,uni,level1,level2,level3)
      colnames(matdes)<-c("Effects","Factors","Units","Level -1","Level 0", "Level 1")
      rownames(matdes)<-c(1:6)
      assign("matdes",matdes, envir = mi)
      dispose(w1)
      DESING()
    })
    visible(w1) <- TRUE
  }

  ##Cerrar
  cerrar<-function(h,...){
    dispose(w)
  }

  #Desing function
  DESI<-function(){
      matdes<-get("matdes",envir = mi)
      A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
      B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
      C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
      D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
      E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
      F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
      mtx <-cbind(A.,B.,C.,D.,E.,F.)

      mdes<-matrix(,13,6)
      for(i in 1:13){
        if(mtx[i,1]==-1){
          mdes[i,1]<-matdes[1,4]
        }
        if(mtx[i,1]==0){
          mdes[i,1]<-matdes[1,5]
        }
        if(mtx[i,1]==1){
          mdes[i,1]<-matdes[1,6]
        }
      }
      for(i in 1:13){
        if(mtx[i,2]==-1){
          mdes[i,2]<-matdes[2,4]
        }
        if(mtx[i,2]==0){
          mdes[i,2]<-matdes[2,5]
        }
        if(mtx[i,2]==1){
          mdes[i,2]<-matdes[2,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,3]==-1){
          mdes[i,3]<-matdes[3,4]
        }
        if(mtx[i,3]==0){
          mdes[i,3]<-matdes[3,5]
        }
        if(mtx[i,3]==1){
          mdes[i,3]<-matdes[3,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,4]==-1){
          mdes[i,4]<-matdes[4,4]
        }
        if(mtx[i,4]==0){
          mdes[i,4]<-matdes[4,5]
        }
        if(mtx[i,4]==1){
          mdes[i,4]<-matdes[4,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,5]==-1){
          mdes[i,5]<-matdes[5,4]
        }
        if(mtx[i,5]==0){
          mdes[i,5]<-matdes[5,5]
        }
        if(mtx[i,5]==1){
          mdes[i,5]<-matdes[5,6]
        }
      }

      for(i in 1:13){
        if(mtx[i,6]==-1){
          mdes[i,6]<-matdes[6,4]
        }
        if(mtx[i,6]==0){
          mdes[i,6]<-matdes[6,5]
        }
        if(mtx[i,6]==1){
          mdes[i,6]<-matdes[6,6]
        }
      }
      colnames(mdes)<-c(paste0(matdes[,1],"-",matdes[,2],"(",matdes[,3],")"))
      rownames(mdes)<-c(1:13)
      run<-matrix(c(1:13),13,1)
      colnames(run)<-"Run"
      mrun<-cbind(run,mdes)

     mu<-sample(c(1:13),13,replace = FALSE)

      x0<-matrix(1,1,6)
        for(i in 1:13){
        v<-mu[i]
        x<-mdes[v,]
        x0<-rbind(x0,x)
      }
      mrunax<-x0[-1,]
      rownames(mrunax)<-c(1:13)
      colnames(mrunax)<-colnames(mdes)
      runs<-matrix(c(mu),13,1)
      colnames(runs)<-"Run"
      mruna<-cbind(runs,mrunax)

      m1<-as.data.frame(matdes)
      m2<-as.data.frame(mrun)
      m3<-as.data.frame(mruna)
      desing<-list(m1,m2,m3)
      names(desing) <- c("...Elements...","...Definitive Screening Design...","...DSD - Random...")
      assign("desing",desing,envir=mi)
      return(desing)
  }

  # 0 -------------------------------------------------------------------------------------------

  #Desing
  DESING<-function(h,...){
    dt1<-get("dt1",envir =mi)
    matdes<-get("matdes",envir = mi)

      DES<-function(){
        print(paste("..................","Matrix desing",".................."),quote=FALSE)
        print("",quote=FALSE)
        DESI()
      }

    tbl<-glayout(container=g1)
    gseparator(horizontal=TRUE, container=g1)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g1)
    tbl[2,c(1:7)]<-(outputArea <- gtext(container=tbl, expand=TRUE,width = 580,height= 450))
    out <- capture.output(DES())
    tbl[1,7]<-(b1<- gbutton("Save",container=tbl,handler=save1))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  # 1 -------------------------------------------------------------------------------------------
  #Bayes
  BAYES<-function(h,...){

    gdata<-get("gdata",envir =mi)
    a<-get("a",envir =mi)
    YY1<-get("YY1",envir =mi)
    YY2<-get("YY2",envir =mi)
    alpha<-get("alpha",envir =mi)
    Vari<-get("Vari",envir =mi)
    V1<-get("V1",envir =mi)


    SBAYES<-function(a,YY1,YY2,alpha,Vari,V1){
      YY1<-as.numeric(YY1)
      YY2<-as.numeric(YY2)
      varnames<-Vari[V1]
      A<-a
      aa<-matrix(a,1,)
      nn<-ncol(aa)
      rownames(aa)<-""
      colnames(aa)<-c(paste0("V.",1:nn))
      print(paste(".................","Response:",varnames,"................."),quote=FALSE)
      print("",quote=FALSE)
      print(aa)
      print("",quote=FALSE)
      print(paste("Odd ratio first step:",YY1),quote=FALSE)
      print("",quote=FALSE)
      print(paste("Odd ratio next steps:",YY2),quote=FALSE)
      print("",quote=FALSE)
      print(paste("Alpha:",alpha),quote=FALSE)
      print("................................................",quote=FALSE)
      print("",quote=FALSE)


      denstn<-function(mu,sigma,nu,alfa,name){
        a<-mu-5*sigma
        b<-mu+5*sigma
        delta<-(b-a)/100
        x<-seq(from=a,to=b,by=delta)
        fn<-(1/sigma)*dt((x-mu)/sigma,nu)

        # Line above zero and horizontal axis
        f0<-(1/sigma)*dt(-mu/sigma,nu)

        # Posterior probability Interval
        alfa2<-alfa/2
        Q1<- sigma*qt(alfa2,nu)+mu
        Q2<-sigma*qt(1-alfa2,nu)+mu

        # Posterior Odds
        probpos<-1-pt(-mu/sigma,nu)
        probneg<-1-probpos
        momioP <-probpos/(1-probpos)
        momioN <-probneg/(1-probneg)

        # Output of results
        salida <- list(name,round(mu,3),round(Q1,3),round(Q2,3),round(probpos,3),round(probneg,3),round(momioP,1),round(momioN,1))
        return(salida)
      }

      BayesA<-function(a,X,alpha){
        name <- unlist(dimnames(X))
        Y<-a
        alfa<-alpha
        name1 <- list("Effects","Estim","Q1","Q2","PPos","PNeg","OddPos","OddNeg")

        # Number of parameters in beta vector and number of effects
        kbeta<-length(name)
        nefectos<-kbeta-1
        nefectos

        Results<- matrix (0, nefectos,8)
        colnames(Results)<-name1

        # Estimation of Effects (BHAT); and Standard Error of Regression (S)
        IXX <- solve(t(X)%*%X)
        BHAT <- solve(t(X)%*%X)%*%t(X)%*%Y
        S <- sqrt((t(Y)%*%Y-t(BHAT)%*%t(X)%*%Y)/(13-kbeta))
        Cii <- sqrt(diag(IXX))

        for (i in 2:kbeta) {
          Bayes<-denstn(BHAT[i],S*Cii[i],13-kbeta,alfa,name=name[i])
          Results[i-1,]<-unlist(Bayes)
        }

        return(Results)
      }

      sequential_bayesian_strategy <- function(a,max_step1,max_step2,max_step3,max_step4,alpha){

        X0 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
        A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
        B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
        C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
        D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
        E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
        F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
        A.B. <- A.*B.;B.A. <- A.*B.;A.C. <- A.*C.; C.A. <- A.*C.;A.D. <- A.*D.;A.E. <- A.*E.;A.F. <- A.*F.;
        B.C. <- B.*C.;B.D. <- B.*D.;B.E. <- B.*E.;B.F. <- B.*F.;C.D. <- C.*D.;C.E. <- C.*E.;C.F.<- C.*F.;
        D.E.<- D.*E.;D.F.<- D.*F.;E.F.<- E.*F.;D.A. <- A.*D.;E.A. <- A.*E.;F.A. <- A.*F.;C.B. <- B.*C.;
        D.B. <- B.*D.;E.B. <- B.*E.;F.B. <- B.*F.;D.C. <- C.*D.;E.C. <- C.*E.;F.C.<- C.*F.;E.D.<- D.*E.;F.D.<- D.*F.;
        F.E.<- E.*F.;A.A.<- A.*A.;B.B. <-B.*B.;C.C. <- C.*C.;D.D. <-D.*D.;E.E. <- E.*E.;F.F.<- F.*F.;
        base_datos <-cbind(A.,B.,C.,D.,E.,F.,A.B.,A.C.,A.D.,A.E.,A.F.,B.C.,B.D.,B.E.,B.F.,C.D.,C.E.,C.F.,D.E.,D.F.,E.F.,A.A.,B.B.,C.C.,D.D.,E.E.,F.F.)
        # Probability Level
        resultado <- list()
        #Step 1 ----------------------------------------------------------
        X <- cbind(X0,A.,B.,C.,D.,E.,F.)
        AnBayes<-BayesA(a,X,alpha) #Ajuste del modelo
        resultado$step1 <- AnBayes
        r1<-as.data.frame(resultado$step1)
        mat<-as.matrix(r1[,c(7,8)])
        datanum<-as.numeric(mat)
        matx<-matrix(datanum,nrow(mat),2)
        rownames(matx)<-r1[,1]
        colnames(matx)<-c("OddPos","OddNeg")
        Significant<-matx[which(matx[,1]>=max_step1|matx[,2]>=max_step1),]
        nSig<-matrix(Significant,,2)
        #Step 2 ----------------------------------------------------------

        if(nrow(nSig)==0){
          gmessage("There are no significant effects",title="Warning")
          res<-list(r1)
          return(res)

        }

        if(nrow(nSig)==1){

            for (i in 1:nrow(matx)){
              if(matx[i,1]==nSig[1]){
                cad<-as.character(r1[i,1])
              }
            }
            cruzados <- outer(cad,cad,paste,sep="")
            effectnames<-c(cad,cruzados)
            X<-cbind(X0,base_datos[,c(effectnames)])
            AnBayes_2<-BayesA(a,X,alpha)
            resultado$step2 <- AnBayes_2
            r2<-as.data.frame(resultado$step2)
            mat<-as.matrix(r2[,c(7,8)])
            datanum<-as.numeric(mat)
            matx<-matrix(datanum,nrow(mat),2)
            rownames(matx)<-r2[,1]
            colnames(matx)<-c("OddPos","OddNeg")
            Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
            nSig<-matrix(Significant,,2)
            #Step 3 ----------------------------------------------------------

              if(nrow(nSig)==0){
                gmessage("There are no significant effects",title="Warning")
                res<-list(r1,r2)
                return(res)

              }

              if(nrow(nSig)==1){

                for (i in 1:nrow(matx)){
                  if(matx[i,1]==nSig[1]){
                  cad<-as.character(r2[i,1])
                  }
                }
                NEnames<-cad
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                res<-list(r1,r2,r3)
                return(res)

              }

              if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                NEnames<-rownames(Significant)
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                mat<-as.matrix(r3[,c(7,8)])
                datanum<-as.numeric(mat)
                matx<-matrix(datanum,nrow(mat),2)
                rownames(matx)<-r3[,1]
                colnames(matx)<-c("OddPos","OddNeg")
                Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                nSig<-matrix(Significant,,2)
                #Step 4 ----------------------------------------------------------

                    if(nrow(nSig)==0){
                      gmessage("There are no significant effects",title="Warning")
                      res<-list(r1,r2,r3)
                      return(res)
                    }

                    if(nrow(nSig)==1){
                      for (i in 1:nrow(matx)){
                        if(matx[i,1]==nSig[1]){
                        cad<-as.character(r3[i,1])
                        }
                      }
                        NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_4<-BayesA(a,X,alpha)
                          resultado$step4 <- AnBayes_4
                          r4<-as.data.frame(resultado$step4)

                          res<-list(r1,r2,r3,r4)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_4<-BayesA(a,X,alpha)
                           resultado$step4 <- AnBayes_4
                           r4<-as.data.frame(resultado$step4)

                           res<-list(r1,r2,r3,r4)
                           return(res)

                          }
                        }
                    }

                    if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                      namesSig<-rownames(Significant)
                      nuevos<- namesSig[ !(namesSig %in% mainP)]
                      nuevos1<- namesSig[ (namesSig %in% mainP)]
                      cruzados <- outer(nuevos1,nuevos1,paste,sep="")
                      EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
                      Effect<-sort(c(nuevos1,nuevos,EffectCruz))
                      X<-cbind(X0,base_datos[,c(Effect)])
                      AnBayes_4<-BayesA(a,X,alpha)
                      resultado$step4 <- AnBayes_4
                      r4<-as.data.frame(resultado$step4)
                      mat<-as.matrix(r4[,c(7,8)])
                      datanum<-as.numeric(mat)
                      matx<-matrix(datanum,nrow(mat),2)
                      rownames(matx)<-r4[,1]
                      colnames(matx)<-c("OddPos","OddNeg")
                      Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                      nSig<-matrix(Significant,,2)
                      #Step 5 ----------------------------------------------------------

                      if(nrow(nSig)==0){
                        gmessage("There are no significant effects",title="Warning")
                        res<-list(r1,r2,r3,r4)
                        return(res)
                      }

                      if(nrow(nSig)==1){
                        for (i in 1:nrow(matx)){
                          if(matx[i,1]==nSig[1]){
                          cad<-as.character(r4[i,1])
                          }
                        }
                          NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_5<-BayesA(a,X,alpha)
                          resultado$step5 <- AnBayes_5
                          r5<-as.data.frame(resultado$step5)

                          res<-list(r1,r2,r3,r4,r5)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_5<-BayesA(a,X,alpha)
                           resultado$step5 <- AnBayes_5
                           r5<-as.data.frame(resultado$step5)

                           res<-list(r1,r2,r3,r4,r5)
                           return(res)

                          }
                        }
                      }

                      if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                        namesSig<-rownames(Significant)
                        Effect<-sort(namesSig)
                        X<-cbind(X0,base_datos[,c(Effect)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                      if(nrow(nSig)>3){
                        SigMax<- pmax(Significant[,1],Significant[,2])
                        SortSigMax<-sort(SigMax,decreasing = TRUE)
                        Significant1<-SortSigMax[c(1:3)]
                        NEnames<-names(Significant1)
                        effectnames<-sort(NEnames)
                        X<-cbind(X0,base_datos[,c(effectnames)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                    }

              }
              #Este no entra no hay mas de 2 efectos del segundo paso
              if(nrow(nSig)>3){
                SigMax<- pmax(Significant[,1],Significant[,2])
                SortSigMax<-sort(SigMax,decreasing = TRUE)
                Significant1<-SortSigMax[c(1:3)]
                NEnames<-names(Significant1)
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                res<-list(r1,r2,r3)
                return(res)

              }

        }

        if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
            namesSig<-rownames(Significant)
            cruzados <- outer(namesSig,namesSig,paste,sep="")
            EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
            effectnames<-sort(c(namesSig,EffectCruz))
            X<-cbind(X0,base_datos[,c(effectnames)])
            AnBayes_2<-BayesA(a,X,alpha)
            resultado$step2 <- AnBayes_2
            r2<-as.data.frame(resultado$step2)
            mat<-as.matrix(r2[,c(7,8)])
            datanum<-as.numeric(mat)
            matx<-matrix(datanum,nrow(mat),2)
            rownames(matx)<-r2[,1]
            colnames(matx)<-c("OddPos","OddNeg")
            Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
            nSig<-matrix(Significant,,2)
            #Step 3 ----------------------------------------------------------
              if(nrow(nSig)==0){
                gmessage("There are no significant effects",title="Warning")
                res<-list(r1,r2)
                return(res)

              }

              if(nrow(nSig)==1){

                for (i in 1:nrow(matx)){
                  if(matx[i,1]==nSig[1]){
                  cad<-as.character(r2[i,1])
                  }
                }
                NEnames<-cad
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                res<-list(r1,r2,r3)
                return(res)

              }

              if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                NEnames<-rownames(Significant)
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                mat<-as.matrix(r3[,c(7,8)])
                datanum<-as.numeric(mat)
                matx<-matrix(datanum,nrow(mat),2)
                rownames(matx)<-r3[,1]
                colnames(matx)<-c("OddPos","OddNeg")
                Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                nSig<-matrix(Significant,,2)
                #Step 4 ----------------------------------------------------------

                    if(nrow(nSig)==0){
                      gmessage("There are no significant effects",title="Warning")
                      res<-list(r1,r2,r3)
                      return(res)
                    }

                    if(nrow(nSig)==1){
                      for (i in 1:nrow(matx)){
                        if(matx[i,1]==nSig[1]){
                        cad<-as.character(r3[i,1])
                        }
                      }
                        NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_4<-BayesA(a,X,alpha)
                          resultado$step4 <- AnBayes_4
                          r4<-as.data.frame(resultado$step4)

                          res<-list(r1,r2,r3,r4)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_4<-BayesA(a,X,alpha)
                           resultado$step4 <- AnBayes_4
                           r4<-as.data.frame(resultado$step4)

                           res<-list(r1,r2,r3,r4)
                           return(res)

                          }
                        }
                    }

                    if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                      namesSig<-rownames(Significant)
                      nuevos<- namesSig[ !(namesSig %in% mainP)]
                      nuevos1<- namesSig[ (namesSig %in% mainP)]
                      cruzados <- outer(nuevos1,nuevos1,paste,sep="")
                      EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
                      Effect<-sort(c(nuevos1,nuevos,EffectCruz))
                      X<-cbind(X0,base_datos[,c(Effect)])
                      AnBayes_4<-BayesA(a,X,alpha)
                      resultado$step4 <- AnBayes_4
                      r4<-as.data.frame(resultado$step4)
                      mat<-as.matrix(r4[,c(7,8)])
                      datanum<-as.numeric(mat)
                      matx<-matrix(datanum,nrow(mat),2)
                      rownames(matx)<-r4[,1]
                      colnames(matx)<-c("OddPos","OddNeg")
                      Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                      nSig<-matrix(Significant,,2)
                      #Step 5 ----------------------------------------------------------

                      if(nrow(nSig)==0){
                        gmessage("There are no significant effects",title="Warning")
                        res<-list(r1,r2,r3,r4)
                        return(res)
                      }

                      if(nrow(nSig)==1){
                        for (i in 1:nrow(matx)){
                          if(matx[i,1]==nSig[1]){
                          cad<-as.character(r4[i,1])
                          }
                        }
                          NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_5<-BayesA(a,X,alpha)
                          resultado$step5 <- AnBayes_5
                          r5<-as.data.frame(resultado$step5)

                          res<-list(r1,r2,r3,r4,r5)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_5<-BayesA(a,X,alpha)
                           resultado$step5 <- AnBayes_5
                           r5<-as.data.frame(resultado$step5)

                           res<-list(r1,r2,r3,r4,r5)
                           return(res)

                          }
                        }
                      }

                      if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                        namesSig<-rownames(Significant)
                        Effect<-sort(namesSig)
                        X<-cbind(X0,base_datos[,c(Effect)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                      if(nrow(nSig)>3){
                        SigMax<- pmax(Significant[,1],Significant[,2])
                        SortSigMax<-sort(SigMax,decreasing = TRUE)
                        Significant1<-SortSigMax[c(1:3)]
                        NEnames<-names(Significant1)
                        effectnames<-sort(NEnames)
                        X<-cbind(X0,base_datos[,c(effectnames)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                    }

              }

              if(nrow(nSig)>3){
                SigMax<- pmax(Significant[,1],Significant[,2])
                SortSigMax<-sort(SigMax,decreasing = TRUE)
                Significant1<-SortSigMax[c(1:3)]
                NEnames<-names(Significant1)
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                mat<-as.matrix(r3[,c(7,8)])
                datanum<-as.numeric(mat)
                matx<-matrix(datanum,nrow(mat),2)
                rownames(matx)<-r3[,1]
                colnames(matx)<-c("OddPos","OddNeg")
                Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                nSig<-matrix(Significant,,2)
                #Step 4 ----------------------------------------------------------
                    if(nrow(nSig)==0){
                      gmessage("There are no significant effects",title="Warning")
                      res<-list(r1,r2,r3)
                      return(res)
                    }

                    if(nrow(nSig)==1){
                      for (i in 1:nrow(matx)){
                        if(matx[i,1]==nSig[1]){
                        cad<-as.character(r3[i,1])
                        }
                      }
                        NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_4<-BayesA(a,X,alpha)
                          resultado$step4 <- AnBayes_4
                          r4<-as.data.frame(resultado$step4)

                          res<-list(r1,r2,r3,r4)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_4<-BayesA(a,X,alpha)
                           resultado$step4 <- AnBayes_4
                           r4<-as.data.frame(resultado$step4)

                           res<-list(r1,r2,r3,r4)
                           return(res)

                          }
                        }
                    }

                    if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                      namesSig<-rownames(Significant)
                      nuevos<- namesSig[ !(namesSig %in% mainP)]
                      nuevos1<- namesSig[ (namesSig %in% mainP)]
                      cruzados <- outer(nuevos1,nuevos1,paste,sep="")
                      EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
                      Effect<-sort(c(nuevos1,nuevos,EffectCruz))
                      X<-cbind(X0,base_datos[,c(Effect)])
                      AnBayes_4<-BayesA(a,X,alpha)
                      resultado$step4 <- AnBayes_4
                      r4<-as.data.frame(resultado$step4)
                      mat<-as.matrix(r4[,c(7,8)])
                      datanum<-as.numeric(mat)
                      matx<-matrix(datanum,nrow(mat),2)
                      rownames(matx)<-r4[,1]
                      colnames(matx)<-c("OddPos","OddNeg")
                      Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                      nSig<-matrix(Significant,,2)
                      #Step 5 ----------------------------------------------------------

                      if(nrow(nSig)==0){
                        gmessage("There are no significant effects",title="Warning")
                        res<-list(r1,r2,r3,r4)
                        return(res)
                      }

                      if(nrow(nSig)==1){
                        for (i in 1:nrow(matx)){
                          if(matx[i,1]==nSig[1]){
                          cad<-as.character(r4[i,1])
                          }
                        }
                          NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_5<-BayesA(a,X,alpha)
                          resultado$step5 <- AnBayes_5
                          r5<-as.data.frame(resultado$step5)

                          res<-list(r1,r2,r3,r4,r5)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_5<-BayesA(a,X,alpha)
                           resultado$step5 <- AnBayes_5
                           r5<-as.data.frame(resultado$step5)

                           res<-list(r1,r2,r3,r4,r5)
                           return(res)

                          }
                        }
                      }

                      if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                        namesSig<-rownames(Significant)
                        Effect<-sort(namesSig)
                        X<-cbind(X0,base_datos[,c(Effect)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                      if(nrow(nSig)>3){
                        SigMax<- pmax(Significant[,1],Significant[,2])
                        SortSigMax<-sort(SigMax,decreasing = TRUE)
                        Significant1<-SortSigMax[c(1:3)]
                        NEnames<-names(Significant1)
                        effectnames<-sort(NEnames)
                        X<-cbind(X0,base_datos[,c(effectnames)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                    }


              }

        }

        if(nrow(nSig)>3){
            SigMax<- pmax(Significant[,1],Significant[,2])
            SortSigMax<-sort(SigMax,decreasing = TRUE)
            Significant1<-SortSigMax[c(1:3)]
            namesSig<-names(Significant1)
            SnamesSig<-sort(namesSig)
            cruzados <- outer(SnamesSig,SnamesSig,paste,sep="")
            EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
            effectnames<-sort(c(namesSig,EffectCruz))
            X<-cbind(X0,base_datos[,c(effectnames)])
            AnBayes_2<-BayesA(a,X,alpha)
            resultado$step2 <- AnBayes_2
            r2<-as.data.frame(resultado$step2)
            mat<-as.matrix(r2[,c(7,8)])
            datanum<-as.numeric(mat)
            matx<-matrix(datanum,nrow(mat),2)
            rownames(matx)<-r2[,1]
            colnames(matx)<-c("OddPos","OddNeg")
            Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
            nSig<-matrix(Significant,,2)
            #Step 3 ----------------------------------------------------------
              if(nrow(nSig)==0){
                gmessage("There are no significant effects",title="Warning")
                res<-list(r1,r2)
                return(res)

              }

              if(nrow(nSig)==1){

                for (i in 1:nrow(matx)){
                  if(matx[i,1]==nSig[1]){
                  cad<-as.character(r2[i,1])
                  }
                }
                NEnames<-cad
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                res<-list(r1,r2,r3)
                return(res)

              }

              if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                NEnames<-rownames(Significant)
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                mat<-as.matrix(r3[,c(7,8)])
                datanum<-as.numeric(mat)
                matx<-matrix(datanum,nrow(mat),2)
                rownames(matx)<-r3[,1]
                colnames(matx)<-c("OddPos","OddNeg")
                Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                nSig<-matrix(Significant,,2)
                #Step 4 ----------------------------------------------------------

                    if(nrow(nSig)==0){
                      gmessage("There are no significant effects",title="Warning")
                      res<-list(r1,r2,r3)
                      return(res)
                    }

                    if(nrow(nSig)==1){
                      for (i in 1:nrow(matx)){
                        if(matx[i,1]==nSig[1]){
                        cad<-as.character(r3[i,1])
                        }
                      }
                        NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_4<-BayesA(a,X,alpha)
                          resultado$step4 <- AnBayes_4
                          r4<-as.data.frame(resultado$step4)

                          res<-list(r1,r2,r3,r4)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_4<-BayesA(a,X,alpha)
                           resultado$step4 <- AnBayes_4
                           r4<-as.data.frame(resultado$step4)

                           res<-list(r1,r2,r3,r4)
                           return(res)

                          }
                        }
                    }

                    if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                      namesSig<-rownames(Significant)
                      nuevos<- namesSig[ !(namesSig %in% mainP)]
                      nuevos1<- namesSig[ (namesSig %in% mainP)]
                      cruzados <- outer(nuevos1,nuevos1,paste,sep="")
                      EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
                      Effect<-sort(c(nuevos1,nuevos,EffectCruz))
                      X<-cbind(X0,base_datos[,c(Effect)])
                      AnBayes_4<-BayesA(a,X,alpha)
                      resultado$step4 <- AnBayes_4
                      r4<-as.data.frame(resultado$step4)
                      mat<-as.matrix(r4[,c(7,8)])
                      datanum<-as.numeric(mat)
                      matx<-matrix(datanum,nrow(mat),2)
                      rownames(matx)<-r4[,1]
                      colnames(matx)<-c("OddPos","OddNeg")
                      Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                      nSig<-matrix(Significant,,2)
                      #Step 5 ----------------------------------------------------------

                      if(nrow(nSig)==0){
                        gmessage("There are no significant effects",title="Warning")
                        res<-list(r1,r2,r3,r4)
                        return(res)
                      }

                      if(nrow(nSig)==1){
                        for (i in 1:nrow(matx)){
                          if(matx[i,1]==nSig[1]){
                          cad<-as.character(r4[i,1])
                          }
                        }
                          NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_5<-BayesA(a,X,alpha)
                          resultado$step5 <- AnBayes_5
                          r5<-as.data.frame(resultado$step5)

                          res<-list(r1,r2,r3,r4,r5)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_5<-BayesA(a,X,alpha)
                           resultado$step5 <- AnBayes_5
                           r5<-as.data.frame(resultado$step5)

                           res<-list(r1,r2,r3,r4,r5)
                           return(res)

                          }
                        }
                      }

                      if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                        namesSig<-rownames(Significant)
                        Effect<-sort(namesSig)
                        X<-cbind(X0,base_datos[,c(Effect)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                      if(nrow(nSig)>3){
                        SigMax<- pmax(Significant[,1],Significant[,2])
                        SortSigMax<-sort(SigMax,decreasing = TRUE)
                        Significant1<-SortSigMax[c(1:3)]
                        NEnames<-names(Significant1)
                        effectnames<-sort(NEnames)
                        X<-cbind(X0,base_datos[,c(effectnames)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                    }

                    if(nrow(nSig)>3){
                      SigMax<- pmax(Significant[,1],Significant[,2])
                      SortSigMax<-sort(SigMax,decreasing = TRUE)
                      Significant1<-SortSigMax[c(1:3)]
                      NEnames<-names(Significant1)
                      #CORTE
                      namesSig<-rownames(NEnames)
                      nuevos<- namesSig[ !(namesSig %in% mainP)]
                      nuevos1<- namesSig[ (namesSig %in% mainP)]
                      cruzados <- outer(nuevos1,nuevos1,paste,sep="")
                      EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
                      Effect<-sort(c(nuevos1,nuevos,EffectCruz))
                      X<-cbind(X0,base_datos[,c(Effect)])
                      AnBayes_4<-BayesA(a,X,alpha)
                      resultado$step4 <- AnBayes_4
                      r4<-as.data.frame(resultado$step4)
                      mat<-as.matrix(r4[,c(7,8)])
                      datanum<-as.numeric(mat)
                      matx<-matrix(datanum,nrow(mat),2)
                      rownames(matx)<-r4[,1]
                      colnames(matx)<-c("OddPos","OddNeg")
                      Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                      nSig<-matrix(Significant,,2)
                      #Step 5 ----------------------------------------------------------

                      if(nrow(nSig)==0){
                        gmessage("There are no significant effects",title="Warning")
                        res<-list(r1,r2,r3,r4)
                        return(res)
                      }

                      if(nrow(nSig)==1){
                        for (i in 1:nrow(matx)){
                          if(matx[i,1]==nSig[1]){
                          cad<-as.character(r4[i,1])
                          }
                        }
                          NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_5<-BayesA(a,X,alpha)
                          resultado$step5 <- AnBayes_5
                          r5<-as.data.frame(resultado$step5)

                          res<-list(r1,r2,r3,r4,r5)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_5<-BayesA(a,X,alpha)
                           resultado$step5 <- AnBayes_5
                           r5<-as.data.frame(resultado$step5)

                           res<-list(r1,r2,r3,r4,r5)
                           return(res)

                          }
                        }
                      }

                      if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                        namesSig<-rownames(Significant)
                        Effect<-sort(namesSig)
                        X<-cbind(X0,base_datos[,c(Effect)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                      if(nrow(nSig)>3){
                        SigMax<- pmax(Significant[,1],Significant[,2])
                        SortSigMax<-sort(SigMax,decreasing = TRUE)
                        Significant1<-SortSigMax[c(1:3)]
                        NEnames<-names(Significant1)
                        effectnames<-sort(NEnames)
                        X<-cbind(X0,base_datos[,c(effectnames)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                    }

              }

              if(nrow(nSig)>3){
                SigMax<- pmax(Significant[,1],Significant[,2])
                SortSigMax<-sort(SigMax,decreasing = TRUE)
                Significant1<-SortSigMax[c(1:3)]
                NEnames<-names(Significant1)
                mainP<- c("A.","B.","C.","D.","E.","F.")
                effectnames<-sort(c(NEnames,mainP))
                effectnames1<-effectnames[!duplicated(effectnames)]
                X<-cbind(X0,base_datos[,c(effectnames1)])
                AnBayes_3<-BayesA(a,X,alpha)
                resultado$step3 <- AnBayes_3
                r3<-as.data.frame(resultado$step3)
                mat<-as.matrix(r3[,c(7,8)])
                datanum<-as.numeric(mat)
                matx<-matrix(datanum,nrow(mat),2)
                rownames(matx)<-r3[,1]
                colnames(matx)<-c("OddPos","OddNeg")
                Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                nSig<-matrix(Significant,,2)
                #Step 4 ----------------------------------------------------------
                    if(nrow(nSig)==0){
                      gmessage("There are no significant effects",title="Warning")
                      res<-list(r1,r2,r3)
                      return(res)
                    }

                    if(nrow(nSig)==1){
                      for (i in 1:nrow(matx)){
                        if(matx[i,1]==nSig[1]){
                        cad<-as.character(r3[i,1])
                        }
                      }
                        NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_4<-BayesA(a,X,alpha)
                          resultado$step4 <- AnBayes_4
                          r4<-as.data.frame(resultado$step4)

                          res<-list(r1,r2,r3,r4)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_4<-BayesA(a,X,alpha)
                           resultado$step4 <- AnBayes_4
                           r4<-as.data.frame(resultado$step4)

                           res<-list(r1,r2,r3,r4)
                           return(res)

                          }
                        }
                    }

                    if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                      namesSig<-rownames(Significant)
                      nuevos<- namesSig[ !(namesSig %in% mainP)]
                      nuevos1<- namesSig[ (namesSig %in% mainP)]
                      cruzados <- outer(nuevos1,nuevos1,paste,sep="")
                      EffectCruz <- cruzados[upper.tri(cruzados,diag = TRUE)]
                      Effect<-sort(c(nuevos1,nuevos,EffectCruz))
                      X<-cbind(X0,base_datos[,c(Effect)])
                      AnBayes_4<-BayesA(a,X,alpha)
                      resultado$step4 <- AnBayes_4
                      r4<-as.data.frame(resultado$step4)
                      mat<-as.matrix(r4[,c(7,8)])
                      datanum<-as.numeric(mat)
                      matx<-matrix(datanum,nrow(mat),2)
                      rownames(matx)<-r4[,1]
                      colnames(matx)<-c("OddPos","OddNeg")
                      Significant<-matx[which(matx[,1]>=max_step2|matx[,2]>=max_step2),]
                      nSig<-matrix(Significant,,2)
                      #Step 5 ----------------------------------------------------------

                      if(nrow(nSig)==0){
                        gmessage("There are no significant effects",title="Warning")
                        res<-list(r1,r2,r3,r4)
                        return(res)
                      }

                      if(nrow(nSig)==1){
                        for (i in 1:nrow(matx)){
                          if(matx[i,1]==nSig[1]){
                          cad<-as.character(r4[i,1])
                          }
                        }
                          NEnames<-cad
                        for(i in 1:6){
                          if(cad==mainP[i]){

                          cruzados <- outer(cad,cad,paste,sep="")
                          effectnames<-c(cad,cruzados)
                          X<-cbind(X0,base_datos[,c(effectnames)])
                          AnBayes_5<-BayesA(a,X,alpha)
                          resultado$step5 <- AnBayes_5
                          r5<-as.data.frame(resultado$step5)

                          res<-list(r1,r2,r3,r4,r5)
                          return(res)

                          }else{

                           effectnames<-cad
                           X<-cbind(X0,base_datos[,c(effectnames)])
                           AnBayes_5<-BayesA(a,X,alpha)
                           resultado$step5 <- AnBayes_5
                           r5<-as.data.frame(resultado$step5)

                           res<-list(r1,r2,r3,r4,r5)
                           return(res)

                          }
                        }
                      }

                      if((nrow(nSig)>=2) & (nrow(nSig)<=3)){
                        namesSig<-rownames(Significant)
                        Effect<-sort(namesSig)
                        X<-cbind(X0,base_datos[,c(Effect)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                      if(nrow(nSig)>3){
                        SigMax<- pmax(Significant[,1],Significant[,2])
                        SortSigMax<-sort(SigMax,decreasing = TRUE)
                        Significant1<-SortSigMax[c(1:3)]
                        NEnames<-names(Significant1)
                        effectnames<-sort(NEnames)
                        X<-cbind(X0,base_datos[,c(effectnames)])
                        AnBayes_5<-BayesA(a,X,alpha)
                        resultado$step5 <- AnBayes_5
                        r5<-as.data.frame(resultado$step5)
                        res<-list(r1,r2,r3,r4,r5)
                        return(res)
                      }

                    }

              }


        }

      }

      print(paste("..................","Estimation",".................."),quote=FALSE)
      max_step1<-YY1
      max_step2<-YY2
      max_step3<-YY2
      max_step4<-YY2

      sequential_bayesian_strategy(a,max_step1,max_step2,max_step3,max_step4,alpha)
    }

    ##g3
    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g2)
    tbl <- glayout(container=gr1, horizontal=FALSE)
    tbl[1,c(1:7)]<-(outputArea <- gtext(container=tbl, expand=TRUE,width = 580,height= 350))
    out <- capture.output(SBAYES(a,YY1,YY2,alpha,Vari,V1))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  # 1 -------------------------------------------------------------------------------------------
  #Bayes
  LASSO<-function(h,...){

    gdata<-get("gdata",envir =mi)
    a<-get("a",envir =mi)
    YY1<-get("YY1",envir =mi)
    YY2<-get("YY2",envir =mi)
    alpha<-get("alpha",envir =mi)
    Vari<-get("Vari",envir =mi)
    V1<-get("V1",envir =mi)


    LASS<-function(a){
      varnames<-Vari[V1]
      A<-a
      aa<-matrix(a,1,)
      nn<-ncol(aa)
      rownames(aa)<-""
      colnames(aa)<-c(paste0("V.",1:nn))
      print(paste(".................","Response:",varnames,"................."),quote=FALSE)
      print("",quote=FALSE)
      print(aa)

      Constant <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
      A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
      B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
      C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
      D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
      E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
      F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
      AB <- A.*B.;BA <- A.*B.;AC <- A.*C.; CA <- A.*C.;AD <- A.*D.;AE <- A.*E.;AF <- A.*F.;
      BC <- B.*C.;BD <- B.*D.;BE <- B.*E.;BF <- B.*F.;CD <- C.*D.;CE <- C.*E.;CF<- C.*F.;
      DE<- D.*E.;DF<- D.*F.;EF<- E.*F.;DA <- A.*D.;EA <- A.*E.;FA <- A.*F.;CB <- B.*C.;
      DB <- B.*D.;EB <- B.*E.;FB <- B.*F.;DC <- C.*D.;EC <- C.*E.;FC<- C.*F.;ED<- D.*E.;FD<- D.*F.;
      FE<- E.*F.;AA<- A.*A.;BB <-B.*B.;CC <- C.*C.;DD <-D.*D.;EE <- E.*E.;FF<- F.*F.;
      base_datos <-cbind(Constant,A.,B.,C.,D.,E.,F.,AB,AC,AD,AE,AF,BC,BD,BE,BF,CD,CE,CF,DE,DF,EF,AA,BB,CC,DD,EE,FF)
      resultado <- list()

      #######################################################################################
      #Lasso
      ######################################################
      X <- base_datos
      fit <-glmnet(x = X , y = a)
      COn<-as.matrix(coef(fit))
      z<-as.matrix(rownames(COn))
      nz<-nrow(z)
      color<-c("#FFC300","darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black","#FF9999","#99FF99","#99CCFF","#9999FF","#CC99FF","#FF99CC","green","yellow")
      dev.new()
      plot(fit, label=TRUE,col=color)
      vn<-nz-1
      legend("topleft",legend=paste(c(1:vn),"-",z[c(2:nz),]),cex=0.6,box.lwd = 0,box.col = "transparent",bg = "transparent",ncol=nz/8,xpd = TRUE,title="CODIFICATION")
      CO<-coef(fit, s = 0.3)
      print(paste("..................","Estimation",".................."),quote=FALSE)
      print("",quote=FALSE)
      r<-as.matrix(CO)
      colnames(r)<-c("Coefficients")
      res<-round(r,digits = 3)
      print(res)
    }

    ##g3
    tbl<-glayout(container=g3)
    gseparator(horizontal=TRUE, container=g3)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g3)
    tbl[1,c(1:7)]<-(outputArea <- gtext(container=tbl, expand=TRUE,width = 580,height= 350))
    out <- capture.output(LASS(a))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  # 2 -------------------------------------------------------------------------------------------
  #Custom
  CUSTOM<-function(h,...){

    gdata<-get("gdata",envir =mi)
    a<-get("a",envir =mi)
    YY1<-get("YY1",envir =mi)
    YY2<-get("YY2",envir =mi)
    alpha<-get("alpha",envir =mi)
    Vari<-get("Vari",envir =mi)
    V1<-get("V1",envir =mi)


    CUST<-function(){
      varnames<-Vari[V1]
      A<-a
      aa<-matrix(a,1,)
      nn<-ncol(aa)
      rownames(aa)<-""
      colnames(aa)<-c(paste0("V.",1:nn))
      print(paste(".................","Response:",varnames,"................."),quote=FALSE)
      print("",quote=FALSE)
      print(aa)
      print("",quote=FALSE)
      dt1<-get("dt1",envir =mi)
      dtm<-matrix(c(dt1),,1)
      nd<-nrow(dtm)

      X0 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
      A. <- c( 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,0)
      B. <- c( 1,-1, 0, 0,-1, 1, 1,-1,-1, 1, 1,-1,0)
      C. <- c(-1, 1,-1, 1, 0, 0, 1,-1, 1,-1, 1,-1,0)
      D. <- c(-1, 1, 1,-1, 1,-1, 0, 0,-1, 1, 1,-1,0)
      E. <- c(-1, 1, 1,-1,-1, 1, 1,-1, 0, 0,-1, 1,0)
      F. <- c(-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 0, 0,0)
      AB <- A.*B.;BA <- A.*B.;AC <- A.*C.; CA <- A.*C.;AD <- A.*D.;AE <- A.*E.;AF <- A.*F.;
      BC <- B.*C.;BD <- B.*D.;BE <- B.*E.;BF <- B.*F.;CD <- C.*D.;CE <- C.*E.;CF<- C.*F.;
      DE<- D.*E.;DF<- D.*F.;EF<- E.*F.;DA <- A.*D.;EA <- A.*E.;FA <- A.*F.;CB <- B.*C.;
      DB <- B.*D.;EB <- B.*E.;FB <- B.*F.;DC <- C.*D.;EC <- C.*E.;FC<- C.*F.;ED<- D.*E.;FD<- D.*F.;
      FE<- E.*F.;AA<- A.*A.;BB <-B.*B.;CC <- C.*C.;DD <-D.*D.;EE <- E.*E.;FF<- F.*F.;
      base_datos <-cbind(A.,B.,C.,D.,E.,F.,AB,AC,AD,AE,AF,BC,BD,BE,BF,CD,CE,CF,DE,DF,EF,AA,BB,CC,DD,EE,FF)
      nbd<-ncol(base_datos)
      names<-matrix(colnames(base_datos))

      x<-matrix(,nd,)
      for(i in 1:nd){
        for(j in 1:nbd){
          if(names[j,]==dtm[i,]){
            x[i,]<-j
          }
        }
      }
      ox<-sort(x)
      mat<-base_datos[,c(ox)]


      denstn<-function(mu,sigma,nu,alfa,name){
        a<-mu-5*sigma
        b<-mu+5*sigma
        delta<-(b-a)/100
        x<-seq(from=a,to=b,by=delta)
        fn<-(1/sigma)*dt((x-mu)/sigma,nu)

        # Line above zero and horizontal axis
        f0<-(1/sigma)*dt(-mu/sigma,nu)

        # Posterior probability Interval
        alfa2<-alfa/2
        Q1<- sigma*qt(alfa2,nu)+mu
        Q2<-sigma*qt(1-alfa2,nu)+mu

        # Posterior Odds
        probpos<-1-pt(-mu/sigma,nu)
        probneg<-1-probpos
        momioP <-probpos/(1-probpos)
        momioN <-probneg/(1-probneg)

        # Output of results
        salida <- list(name,round(mu,3),round(Q1,3),round(Q2,3),
                       round(probpos,3),round(probneg,3),
                       round(momioP,1),round(momioN,1))
        return(salida)
      }

      BayesA<-function(a,X,alpha){
        name <- unlist(dimnames(X))
        Y<-a
        alfa<-alpha
        name1 <- list("Effects","Estim","Q1","Q2","PPos","PNeg","OddPos","OddNeg")

        # Number of parameters in beta vector and number of effects

        kbeta<-length(name)
        nefectos<-kbeta-1
        nefectos

        Results<- matrix (0, nefectos,8)
        colnames(Results)<-name1

        # Estimation of Effects (BHAT); and Standard Error of Regression (S)
        IXX <- solve(t(X)%*%X)
        BHAT <- solve(t(X)%*%X)%*%t(X)%*%Y
        S <- sqrt((t(Y)%*%Y-t(BHAT)%*%t(X)%*%Y)/(13-kbeta))
        Cii <- sqrt(diag(IXX))

        for (i in 2:kbeta) {
          Bayes<-denstn(BHAT[i],S*Cii[i],13-kbeta,alfa,name=name[i])
          Results[i-1,]<-unlist(Bayes)
        }

        return(Results)
      }

      sequential_bayesian_strategy <- function(a,alpha,mat){

        X0 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
        resultado <- list()
        #######################################################################################
        #Step 1
        ######################################################
        X <- cbind(X0,mat)
        AnBayes<-BayesA(a,X,alpha)
        resultado$step1 <- AnBayes
        res<-as.data.frame(resultado)
        colnames(res)<-c("Effects","Estim","Q1","Q2","PPos","PNeg","OddPos","OddNeg")
        return(res)
      }


      print(paste("..................","Estimation",".................."),quote=FALSE)
      print("",quote=FALSE)
      sequential_bayesian_strategy(a,alpha,mat)

    }

    ##g3
    tbl<-glayout(container=g4)
    gseparator(horizontal=TRUE, container=g4)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g4)
    tbl <- glayout(container=gr1, horizontal=FALSE)
    tbl[1,c(1:7)]<-(outputArea <- gtext(container=tbl, expand=TRUE,width = 580,height= 350))
    out <- capture.output(CUST())
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  #-------------------------------------------------------------------------------------------
  # MENUS
  abrir2<-list(one=gaction("csv",handler=abrirc),two=gaction("txt",handler=abrirt),three=gaction("xlsx",handler=openex))
  menulistaA<-list(Open=abrir2,u2=gaction("View",handler=ver),u3=gaction("Refresh",handler=inicio),u4=gaction("Close",handler=cerrar))
  imp<-list(cero=gaction("Design",handler=parm3),one=gaction("Bayesian",handler=parm),two=gaction("Lasso",handler=parm2),three=gaction("Custom",handler=parm1))

  #Manual
  y1<- function(h,..) gmessage("http://www.uv.mx/personal/nehuerta/bayesdef/",title="Link")

  menulistaY<-list(u0=gaction("Handbook",handler=y1))

  ##MENU
  mb_list <-list(File=menulistaA,Method=imp,Help=menulistaY)
  gmenu(mb_list, container=g)

  ##g1
  #Information
  tmp1 <- gframe("", container=g0, expand=TRUE,horizontal=FALSE)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                    Bayesian Analysis of DSD                  ",container=tmp1)
  font(tg) <- list(weight="bold",size= 26,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                          ITAM                          UV                        ",container=tmp1)
  font(tg) <- list(weight="bold",size= 24,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                        Statistics Deparment                  Universidad Veracruzana             ",container=tmp1)
  font(tg) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
  visible(w) <- TRUE
}
