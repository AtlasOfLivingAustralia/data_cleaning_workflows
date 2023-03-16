# ================================================================================
# ahull.IUCN 
# Written by Beatriz Pateiro-López
# Based on:
# - Guidelines for Using the IUCN Red List Categories and Criteria
# - Bias in species range estimates from minimum convex polygons: implications for conservation and options for improved planning Burgman and Fox (2003)
# ================================================================================

library(interp)

# INPUT
# x, y  ----- The x and y arguments provide the x and y coordinates of a set of points. Alternatively, a single argument x can be provided.
# alpha  ---- Value of alpha (The method delete all lines that are longer than a mulitple alpha of the average line length)
 
ahull.IUCN <- function(x, y = NULL, alpha = 2){
X <- xy.coords(x, y)
x <- cbind(X$x, X$y)
n<-nrow(x)
if (dim(x)[1] <= 2) {
        stop("At least three non-collinear points are required")
}

# Algorithm Burgman and Fox (2003)
# ------------------------------------
## 1) Delauney triangulation
tri.obj <- tri.mesh(X)
tri  <- triangles(tri.obj)
arcs <- arcs(tri.obj)

## 2) Length of lines
lengtharcs<-sqrt(rowSums((x[arcs[,1],]-x[arcs[,2],])^2))
avl<-mean(lengtharcs)

## 3) Delete all lines that are longer than a multiple (alpha) of the average line length. 
karcs<-(lengtharcs<=alpha*avl)       # arcs to keep (T/F)
wkarcs<-which(lengtharcs<=alpha*avl) # which arcs to keep
ktri<-apply(matrix(tri[,c("arc1","arc2","arc3")]%in%wkarcs,ncol=3),1,all) # triangles to keep (those with all 3 arcs in wkarcs) 
wktri<-which(ktri)                   # which triangles to keep 

aux<-tri[,c("tr1","tr2","tr3")]%in%wktri
triIUCN<-tri[,c("tr1","tr2","tr3")]
triIUCN[!aux]<-0

aux<-tri[,c("arc1","arc2","arc3")]%in%wkarcs
arcIUCN<-tri[,c("arc1","arc2","arc3")]
arcIUCN[!aux]<-0

# For each triangle in Delaunay triangulation (for each row in tri)
# triIUCN[,1]   ---- TRUE/FALSE indicating whether the complete triangle i belongs to the ahull
# triIUCN[,2:4] ---- indices of neighbour triangles belonging to the ahul  (0 if they do no belong to the ahull)
# triIUCN[,5:7] ---- indices of arcs of the triangle belonging to the ahul (0 if they do no belong to the ahull)

triIUCN<-cbind("tri.in.ah"=ktri,triIUCN,arcIUCN)  

##(4) Calculate the area of habitat by summing the areas of all remaining triangles.
areaIUCN<-sum(area(tri.obj)[ktri]) # Area of the ahull (sum of the areas of the triangles belonging to the ahull)

aux<-as.numeric(triIUCN[triIUCN[,1]==0,c("arc1","arc2","arc3")])
aux<-aux[aux!=0]
barcsIUCN<-unique(aux)  # Arcs in the boundary of the ahull from triangles that don't belong to the ahull
barcsIUCN<-sort(union(barcsIUCN,tri[ktri,7:9][tri[ktri,4:6]==0])) # Arcs in the boundary of the ahull
isolp<-sort(setdiff(1:n,arcs[karcs])) # Isolated points belonging to the ahull (those that do not belong to an arc in the ahull)

tri.ah.IUCN<-matrix(tri[ktri,1:3],ncol=3)
colnames(tri.ah.IUCN)<-c("node1","node2","node3")

bd.ah.IUCN=matrix(arcs[barcsIUCN,],ncol=2)
colnames(bd.ah.IUCN)<-c("from","to")

ahullIUCN.obj <- list(tri.ah.IUCN=tri.ah.IUCN, bd.ah.IUCN=bd.ah.IUCN, ip.ah.IUCN=isolp, area=areaIUCN, tri = tri.obj, alpha = alpha)
class(ahullIUCN.obj) <- "ahull.IUCN"
invisible(ahullIUCN.obj)
}


# ================================================================================
# Very simple plot function
# x --------------- data
# ah.IUCN.obj ----- ahull.IUCN object from ahull.IUCN
# ================================================================================


plot.ahull.IUCN<-function(x,ah.IUCN.obj,...){
tri.ah<-ah.IUCN.obj$tri.ah.IUCN
edges.ah<-ah.IUCN.obj$bd.ah.IUCN
ip.ah<-ah.IUCN.obj$ip.ah.IUCN

plot(x,main=paste("IUCN ahull for alpha =",ah.IUCN.obj$alpha,"\n Area =",round(ah.IUCN.obj$area,dig=4)),xlab="",ylab="",...)
if(nrow(tri.ah)>0){
for(i in 1:nrow(tri.ah)){
polygon(x[tri.ah[i,],1],x[tri.ah[i,],2],col=3,lty=2)       # Triangles in the ahull
}
}
segments(x[edges.ah[,1],1],x[edges.ah[,1],2],x[edges.ah[,2],1],x[edges.ah[,2],2],col=4,lwd=3) # Boundary edges in the ahull in blue
points(x[ip.ah,],col=2,pch=19)  # Isolated points in the ahull in red (if any)
}


# ================================================================================
# ================================================================================
# Example 1
# ================================================================================
set.seed(1234)
n <- 500
theta<-runif(n,0,2*pi)
r<-sqrt(runif(n,0.25^2,0.5^2))
x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
alpha<-2.5

ah.IUCN.obj<-ahull.IUCN(x,alpha=alpha)
plot.ahull.IUCN(x,ah.IUCN.obj)

# ================================================================================
# ================================================================================
# Example 2
# ================================================================================
library(ggplot2)
library(ozmaps)
library(sf)

aus <- st_as_sf(ozmap_country, crs = 4236)

# Create example dataset 
data <- data.frame(longtitude = c(125.1847,
                                  125.1850,
                                  125.1861,
                                  125.1886,
                                  125.1862,
                                  125.1852,
                                  125.1842),
                   latitude = c(-14.58750,
                                -14.58770,
                                -14.58694,
                                -14.59250,
                                -14.58700,
                                -14.58900,
                                -14.59080)
)

# Put data in correct format
sf_data <- st_as_sf(data, coords = c("longtitude", "latitude"), crs = 4236)

# Plot data
ggplot() + 
  geom_sf(data = aus, colour = "black", fill = "white")  +
  geom_sf(data = sf_data, colour = "red", size = 1) 

# Plot zoomed in data
ggplot() + 
  geom_sf(data = aus, colour = "black", fill = "white")  +
  geom_sf(data = sf_data, colour = "red", size = 1) + 
  coord_sf(xlim=c(124, 126), 
           ylim=c(-14,-16))

# Compute alpha hull using ahull.IUCN
ah.IUCN.obj<-ahull.IUCN(data,alpha=2)
plot.ahull.IUCN(data,ah.IUCN.obj)


# ================================================================================
# ================================================================================
# Example 3
# ================================================================================
library(ggplot2)
library(ozmaps)
library(sf)

# Create Australia map in correct format
aus <- st_as_sf(ozmap_country, crs = 4236)

# Create example dataset 
data <- data.frame(longtitude = c( 145.380,
                                   144.500,
                                   141.530,
                                   148.241),
                   latitude = c(-38.530,
                                -38.080,
                                -38.420,
                                -40.214)
)

# Put data in correct format
sf_data <- st_as_sf(data, coords = c("longtitude", "latitude"), crs = 4236)

# Plot data
ggplot() + 
  geom_sf(data = aus, colour = "black", fill = "white")  +
  geom_sf(data = sf_data, colour = "red", size = 1) 

# Plot zoomed in data
ggplot() + 
  geom_sf(data = aus, colour = "black", fill = "white")  +
  geom_sf(data = sf_data, colour = "red", size = 1) + 
  coord_sf(xlim=c(140, 150), 
           ylim=c(-36,-41))


# Compute alpha hull using ahull.IUCN
ah.IUCN.obj<-ahull.IUCN(data,alpha=2)
plot.ahull.IUCN(data,ah.IUCN.obj)




