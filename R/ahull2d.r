# ahull2d v6.1 - add-ins for SciLab 4.1.2
# made in: <<TerraNoNames [http://mykaralw.narod.ru/]>>
# 29.06.2011.

# Functions:
# function tumb <- tumbling2d(x1,y1,x2,y2,xt,yt)   --- O(N)
# function gr <- convexhull2d(X,Y)                 --- O(N*log(N))
# function tri <- delaunay2d(X,Y,gr)               --- O(N*log(N)~N^2)


tumbling2d<-function(x1,y1,x2,y2,xt,yt)
{
#determination sides tumbling:
#tumb>0 - to the left;
#tumb<0 - to the right.
#EXAMPLE:
#    pright<-(tumbling2d(0,0,10,0,c(2,5,7),c(6,-1,4))<0)
#O(N)
# Copyright Akon&TerraNoNames
	dyt <- yt - y1
	dxt <- xt - x1
	dyp <- y2 - y1
	dxp <- x2 - x1
	tumb <- dxp * dyt - dxt * dyp
	return(tumb)
}

convexhull2d<-function(Xc,Yc)
{
#Searching for of the proturberant shell.
#The algorithm Grehema.
#point - vector point shells.
#EXAMPLE:
#    gr<-convexhull2d(X,Y)
#O(N*log(N)): N=1000points, W=2000GH, Memory=2000Gb, Time=0.3c.
# Copyright Akon&TerraNoNames
	nx <- length(Xc)
	ny <- length(Yc)
	if (nx==ny)
	{
		n<-nx
		if (n < 4)
		{
			point <- seq(n)
		}
		else
		{
			xt <- Xc[1]
			yt <- Yc[1]
			for (i in 2:n)
			{
				if (xt > Xc[i])
				{
					xt <- Xc[i]
					yt <- Yc[i]
					zt <- i
				}
			}
			dx <- Xc - xt
			dy <- Yc - yt
			dl <- sqrt(dx*dx+dy*dy)
			tt <- dy/dl
			tt[zt] <- 0
			tts <- sort(tt,index.return=TRUE)
			ttsix <- tts$ix
			sols<-seq(n)
			sols[1] <- zt
			sols[2] <- ttsix[1]
			sols[3] <- ttsix[2]
			tis <- 3
			for (i in 3:n)
			{
				tis <- tis + 1
				sols[tis] <- ttsix[i]
				while((tumbling2d(Xc[sols[tis - 2]],Yc[sols[tis - 2]],Xc[sols[tis - 1]],Yc[sols[tis - 1]],Xc[sols[tis]],Yc[sols[tis]]) <= 0) & (tis > 3))
				{
					tis <- (tis - 1)
					sols[tis] = sols[tis + 1]
				}
			}
			point = sols[1:tis]
		}
	}
	else
	{
		tis <- 0
		point <- "incompatible dimensions X and Y"
	}
	return(point)
}

#function [counttr,tri]=delaunay2d(Xr,Yr,grr)
delaunay2d<-function(Xc,Yc,grc)
{
#The triangulation Delone.
#The method of direct searching for.
#Iterration sort.
#tri - table triangle.
#EXAMPLE:
#    gr<-convexhull2d(X,Y)
#    tri<-delaunay2dis(X,Y,gr)
#O(N*log(N)~N^2): N=1000points, W=2000GH, Memory=2000Gb, Time=24c.
# Copyright Akon&TerraNoNames
	nx <- length(Xc);
	ny <- length(Yc);
	ngr <- length(grc);
	grt <- grc
	tric <- c()
	if (nx == ny)
	{
		n <- nx
		nt <- n * 10;
		i <- ngr;
		if (grc[1] != grc[ngr])
		{
			grt <- c(grc,grc[1])
			i <- (i+1)
		}
		counta <- (i-1)
		bega <- 1
		alive1 <- grt[1:counta]
		alive2 <- grt[2:i]
		alive3 <- rep(-1,counta)
		alive4 <- rep(0,counta)
		avec <- matrix(rep(0,n*n),nrow=n,ncol=n)
		counttr <- 0
		tric1<-c()
		tric2<-c()
		tric3<-c()
		for (i in 1:counta)
		{
			avec[alive1[i],alive2[i]] <- i
			avec[alive2[i],alive1[i]] <- i
		}		
		ap <- rep(1,n)
		while (counta >= bega)
		{
			i <- alive1[bega]
			j <- alive2[bega]
			k <- alive3[bega]
			compla <- alive4[bega]
			hn <- (-1)
			if (compla == 0)
			{
				x1 <- Xc[i]
				y1 <- Yc[i]
				x2 <- Xc[j]
				y2 <- Yc[j]
				x0 <- (x1+x2)/2
				y0 <- (y1+y2)/2
				r2t <- (Xc-x0)*(Xc-x0)+(Yc-y0)*(Yc-y0)
				r2si <- sort(r2t,index.return=TRUE)
				r2s <- r2si$x
				nts <- r2si$ix
				t1 <- x1*x1+y1*y1
				t2 <- x2*x2+y2*y2
				t3 <- Xc*Xc+Yc*Yc;
				sc <- (-x1)*(Yc-y2)+x2*(Yc-y1)+Xc*(y1-y2)
				sa <- (-t1)*(Yc-y2)+t2*(Yc-y1)+t3*(y1-y2)
				sb <- (-t1)*(Xc-x2)+t2*(Xc-x1)+t3*(x1-x2)
				tc <- tumbling2d(x1,y1,x2,y2,Xc,Yc)
				hi <- 0
				while ((hi < n) & (hn < 0))
				{
					hi <- (hi+1)
					h <- nts[hi]
					if ((h != i) & (h != j) & (h != k) & (ap[h] > 0) & (tc[h] > 0) & (sc[h] != 0))
					{
						xc <- 0.5*sa[h]/sc[h]
						yc <- -0.5*sb[h]/sc[h]
						r2c <- (x1-xc)*(x1-xc)+(y1-yc)*(y1-yc)
						hn <- h
						r2 <- (Xc-xc)*(Xc-xc)+(Yc-yc)*(Yc-yc)
						for (li in 1:n)
						{
							l <- nts[li]
							if ((l != i) & (l != j) & (l != k) & (l != h))
							{
								if ((tc[l] > 0) & (r2[l] < r2c))
								{
									hn <- (-1)
								}
							}
						}
					}
				}
				alive4[bega] <- hn
			}
			if (hn > 0)
			{
				k <- 0
				if (avec[i,hn] > 0)
				{
					k <- avec[i,hn]
				}
				if (k == 0)
				{
					counta <- (counta+1)
					alive1 <- c(alive1,i)
					alive2 <- c(alive2,hn)
					alive3 <- c(alive3,j)
					alive4 <- c(alive4,0)
					avec[i,hn] <- counta
					avec[hn,i] <- counta
				}
				else
				{
					alive4[k] <- j
					ap[i] <- (-1)
				}
				k <- 0
				if (avec[j,hn] > 0)
				{
					k <- avec[j,hn]
				}
				if (k == 0)
				{
					counta <- (counta+1)
					alive1 <- c(alive1,hn)
					alive2 <- c(alive2,j)
					alive3 <- c(alive3,i)
					alive4 <- c(alive4,0)
					avec[j,hn] <- counta
					avec[hn,j] <- counta
				}
				else
				{
					alive4[k] <- i
					ap[j] <- (-1)
				}
				counttr <- (counttr+1)
				tric1 <- c(tric1,i)
				tric2 <- c(tric2,j)
				tric3 <- c(tric3,hn)
				h <- 1
			}
			bega <- (bega+1)
		}
		tri <- data.frame(A=tric1,B=tric2,C=tric3)
	}
	else
	{
		tri <- "incompatible dimensions X and Y"
	}
	return(tri)
}

print("Library ahull2d_v1.1 load...")
print(" Functions:")
print("   tumb <- tumbling2d(x1,y1,x2,y2,xt,yt)   --- O(N)")
print("   gr <- convexhull2d(X,Y)                 --- O(N*log(N))")
print("   tri <- delaunay2d(X,Y,gr)               --- O(N*log(N)~N^2)")
