# geo_ter v1.3 - add-ins for R 2.13.0
# made in: <<TerraNoNames [http://mykaralw.narod.ru/]>>
# 10.08.2011.

# Functions:
# function [xp,yp,ztri] <- tri.param(Tri,X,Y,Z)                     --- O(N)
# function [stri,ztri] <- tri.area(Tri,X,Y,Z)                       --- O(N)
# function [sPov,zPov] <- tri.area.full(stri,ztri)                  --- O(N)
# function [sPov,zPov] <- tri.area.fast(Tri,X,Y,Z)                  --- O(N)
# function [cmap] <- hot.colors(n)                                  --- O(N)
# function [cmap] <- rb.colors(n)                                   --- O(N)
# function [cmap] <- rg.colors(n)                                   --- O(N)
# function color <- rb.color(a,b,i)                                 --- O(N)
# function color <- rg.color(a,b,i)                                 --- O(N)
# function color <- gray.color(a,b,i)                               --- O(N)
# function tri.plot.2d(Tri,X,Y,Z)                                   --- O(N)
# function tri.plot.3d(Tri,X,Y,Z,[teta],[zeta])                     --- O(N)


tri.param <- function(Tri,X,Y,Z)
{
#base parameter triangle.
#EXAMPLE:
#    gr=convexhull2d(X,Y)
#    tri=delaunay2d(X,Y,gr)
#    [xp,yp,zp,ztri]=triparam(tri,X,Y,Z);
#O(N)
# Copyright Akon&TerraNoNames
 m <- length(Tri)
 n <- length(Tri$A)
 triA <- Tri$A
 triB <- Tri$B
 triC <- Tri$C
 xpA <- X[triA]
 ypA <- Y[triA]
 zpA <- Z[triA]
 xpB <- X[triB]
 ypB <- Y[triB]
 zpB <- Z[triB]
 xpC <- X[triC]
 ypC <- Y[triC]
 zpC <- Z[triC]
 ztri <- (zpA+zpB+zpC)/3
 triT <- data.frame(xA=xpA,yA=ypA,xB=xpB,yB=ypB,xC=xpC,yC=ypC,zT=ztri)
 return(triT)
}

tri.area <- function(Tri,X,Y,Z)
{
#areas triangle.
#EXAMPLE:
#    gr <- convexhull2d(X,Y)
#    tri <- delaunay2d(X,Y,gr)
#    [stri,ztri] <- triarea(tri,X,Y,Z)
#O(N)
# Copyright Akon&TerraNoNames
 tri.data <- tri.param(Tri,X,Y,Z)
 xA <- tri.data$xA
 xB <- tri.data$xB
 xC <- tri.data$xC
 yA <- tri.data$yA
 yB <- tri.data$yB
 yC <- tri.data$yC
 stri <- 1/2*abs((xA+xB)*(yA-yB)+(xB+xC)*(yB-yC)+(xC+xA)*(yC-yA))
 ztri <- tri.data$zT
 triS <- data.frame(sT=stri,zT=ztri)
 return(triS)
}

tri.area.full <- function(tridata)
{
#global area triangle.
#EXAMPLE:
#    gr <- convexhull2d(X,Y)
#    tri <- delaunay2d(X,Y,gr)
#    triSZ <- tri.area(tri,X,Y,Z)
#    [sarea,zarea] <- tri.area.fill(triSZ)
#O(N)
# Copyright Akon&TerraNoNames
 sarea <- sum(tridata$sT)
 zarea <- sum(tridata$sT*tridata$zT)/sarea
 triSZ <- data.frame(S=sarea,Z=zarea)
 return(triSZ)
}

tri.area.fast <- function(Tri,X,Y,Z)
{
#global area triangle.
#EXAMPLE:
#    gr <- convexhull2d(X,Y)
#    tri <- delaunay2d(X,Y,gr)
#    [sarea,zarea] <- tri.area.fast(tri,X,Y,Z)
#O(N)
# Copyright Akon&TerraNoNames
 tridata <- tri.area(Tri,X,Y,Z)
 triSZ <- tri.area.full(tridata)
 return(triSZ)
}

hot.colors <- function(n)
{
	m <- trunc(n/3)
	m3 <- n-2*m
	pas <- 0.75
	
	r <- c(seq(0,pas,len=m),rep(1,n-m))
	g <- c(rep(0,m),seq(0,pas,len=m),rep(1,n-2*m))
	b <- c(rep(0,2*m),seq(0,pas,len=m3))
	rgb(r,g,b)
}

rb.colors <- function(n)
{
	m <- trunc(n)
	r <- seq(0,1,len=m)
	b <- r[m:1]
	g <- (1-sqrt(r*r+b*b))/2
	rgb(r,g,b)
}

rg.colors <- function(n)
{
	m <- trunc(n)
	r <- seq(0,1,len=m)
	g <- r[m:1]
	b <- (1-sqrt(r*r+g*g))/2
	rgb(r,g,b)
}

rb.color <- function(a,b,i)
{
	r <- (i-a)/(b-a)
	b <- 1-r
	g <- (1-sqrt(r*r+b*b))/2
	rgb(r,g,b)
}

rg.color <- function(a,b,i)
{
	r <- (i-a)/(b-a)
	g <- 1-r
	b <- (1-sqrt(r*r+g*g))/2
	rgb(r,g,b)
}

gray.color <- function(a,b,i)
{
	r <- (i-a)/(b-a)
	b <- r
	g <- r
	rgb(r,g,b)
}

tri.plot.2d <- function(Tri,X,Y,Z)
{
 m <- length(Tri)
 n <- length(Tri$A)
 triA <- Tri$A
 triB <- Tri$B
 triC <- Tri$C
 xpA <- X[triA]
 ypA <- Y[triA]
 zpA <- Z[triA]
 xpB <- X[triB]
 ypB <- Y[triB]
 zpB <- Z[triB]
 xpC <- X[triC]
 ypC <- Y[triC]
 zpC <- Z[triC]
 ztri <- (zpA+zpB+zpC)/3
 minz <- min(ztri)
 maxz <- max(ztri)
 plot(X,Y)
 for (i in 1:n)
 {
  tr.x <- c(xpA[i],xpB[i],xpC[i],xpA[i])
  tr.y <- c(ypA[i],ypB[i],ypC[i],ypA[i])
  tr.c <- rg.color(minz,maxz,ztri[i])
  polygon(tr.x,tr.y,col=tr.c)
 }
 minz <- trunc(1000*minz+0.5)/1000
 maxz <- trunc(1000*maxz+0.5)/1000
 legend("topright", legend = c(maxz, minz),lty = c(1,1),col = c(rg.color(minz,maxz,maxz),rg.color(minz,maxz,minz)),bg = 'white')
}

tri.plot.3d <- function(Tri,XX,YY,ZZ,teta=0.7,zeta=0.7)
{
 X <- (XX-mean(XX))
 Y <- (YY-mean(YY))
 Z <- (ZZ)
 ttc <- cos(teta)
 tts <- sin(teta)
 ztc <- cos(zeta)
 zts <- sin(zeta)
 res <- matrix(c(ttc,-tts*ztc,-tts*zts,0,tts,ttc*ztc,ttc*zts,0,0,-zts,ztc,0,0,0,0,1),nrow=4,ncol=4)
 m <- length(Tri)
 n <- length(Tri$A)
 triA <- Tri$A
 triB <- Tri$B
 triC <- Tri$C
 xpA <- X[triA]
 ypA <- Y[triA]
 zpA <- Z[triA]
 xpB <- X[triB]
 ypB <- Y[triB]
 zpB <- Z[triB]
 xpC <- X[triC]
 ypC <- Y[triC]
 zpC <- Z[triC]
 ztri <- (zpA+zpB+zpC)/3
 minz <- min(ztri)
 maxz <- max(ztri)
 xy <- trans3d(X,Y,Z,res)
 plot(xy,asp=1)
 for (i in 1:n)
 {
  tr.x <- c(xpA[i],xpB[i],xpC[i],xpA[i])
  tr.y <- c(ypA[i],ypB[i],ypC[i],ypA[i])
  tr.z <- c(zpA[i],zpB[i],zpC[i],zpA[i])
  tr.c <- rg.color(minz,maxz,ztri[i])
  trs.xy <- trans3d(tr.x,tr.y,tr.z,res)
  trs.x <- trs.xy$x
  trs.y <- trs.xy$y
  polygon(trs.x,trs.y,col=tr.c)
 }
 minz <- trunc(1000*minz+0.5)/1000
 maxz <- trunc(1000*maxz+0.5)/1000
 legend("topright", legend = c(maxz, minz),lty = c(1,1),col = c(rg.color(minz,maxz,maxz),rg.color(minz,maxz,minz)),bg = 'white')
}

print("Library geo_ter_v1.3 load...")
print(" Functions:")
print("  [xp,yp,ztri] <- tri.param(Tri,X,Y,Z)          --- O(N)")
print("  [stri,ztri] <- tri.area(Tri,X,Y,Z)            --- O(N)")
print("  [sPov,zPov] <- tri.area.full(stri,ztri)       --- O(N)")
print("  [sPov,zPov] <- tri.area.fast(Tri,X,Y,Z)       --- O(N)")
print("  [cmap] <- hot.colors(n)                       --- O(N)")
print("  [cmap] <- rb.colors(n)                        --- O(N)")
print("  [cmap] <- rg.colors(n)                        --- O(N)")
print("  color <- rb.color(a,b,i)                      --- O(N)")
print("  color <- rg.color(a,b,i)                      --- O(N)")
print("  color <- gray.color(a,b,i)                    --- O(N)")
print("  tri.plot.2d(Tri,X,Y,Z)                        --- O(N)")
print("  tri.plot.3d(Tri,X,Y,Z,[teta],[zeta])          --- O(N)")
