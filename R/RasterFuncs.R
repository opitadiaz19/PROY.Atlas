#
# RasterVels.R
#
# Source: https://www.r-spatial.org/r/2017/01/30/mapedit_intro.html
# Observa: simpleMapedit.R y ../Proy.CEMIE.ALGUNDIA/ViewTest.R
# ===================

# La diferencia con el ejemplo de la liga es que cambié
# mapview por leaflet.
# También voy a agrear el editor de leafpm 

library(ncdf4)
library(leaflet)
# library(mapview)
library(mapedit)
library(leafpm)
library(raster)

sourdir <- '/media/checo/7B2C787106E895ED/Proy.CEMIE.ALGUNDIA/'

miPath <- "file:///media/checo/7B2C787106E895ED/PROY.Atlas/"

source("/media/checo/7B2C787106E895ED/RR/MiniBiblioteca.R", chdir = T)
source(sourdir %,% "RRarrows.R", chdir = T)
# debugSource(sourdir %,% "RRarrows.R", chdir = T)

# =========== PROCESAMIENTO DE INFO RASTER y VECTORES =============

VelMap <- function (f0, profs=1:3, npts=2000, t0=1) { # prof IN 1:3
  # f0: Nombre del archivo nc
  #     p.ej. sourdir %,% "problema_I2.nc"
  # prof: profundidad codificada IN 1:3
  # t0: tiempo
  
  m <- leaflet() %>% addTiles() # mapa inicial sin nada
  
  for (prof in profs) {
    U <- raster(f0, var = "u_psi", band = prof, lvar=4, level=t0) 
    V <- raster(f0, var = "v_psi", band = prof, lvar=4, level=t0) 
    rapidez<-sqrt(U^2+V^2)
    
    # npts <- 2000 # tentativo aunque hay muchos NAs
    
    # Elegir la función de muestreo
    ff0 <- sampleRegular # muestreo regular
    
    Us <- ff0(U, size=npts, asRaster=TRUE)
    Vs <- ff0(V, size=npts, asRaster=TRUE)
    Rs <- ff0(rapidez, size=npts, asRaster=TRUE)
    
    # Los puntos con valores son:
    u.pts<-rasterToPoints(Us)
    v.pts<-rasterToPoints(Vs)
    r.pts<-rasterToPoints(Rs)
    
    # Hagamos un data.frame con toda la información
    tt <- as.data.frame(u.pts)
    names(tt)[3] <- "U"
    tt$V <- v.pts[,3]
    tt$R <- r.pts[,3]
    
    # Espacio entre puntos:
    #   Resolución de los raster
    rs <- res(rapidez)
    ddx <- rs[1]
    ddy <- rs[2]
    ddr <- sqrt(ddx^2+ddy^2)
    
    # Este será el valor de escala de los vectores
    #  La máxima magnitud de un vector
    ddv <- max(tt$R)
    # factor de escala
    ss <- ddr/ddv*15
    # El tamaño de la punta de las flechitas:
    aa <- ddr*2
    # El punto inicial de las flechitas serán los puntos (tt$x, tt$y)
    # Hagamos los puntos finales:
    tt$xf <- tt$x + tt$U*ss
    tt$yf <- tt$y + tt$V*ss
    
    # Construyamos las flechitas:
    arrows <- genArrow(tt$x, tt$y, tt$xf, tt$yf, aa)
    colnames(arrows) <- c("lng", "lat")
    
    # ============== CONSTRUCCIÓN MAPA ==================
    
    # editor=NULL # usa el editor default ("leaflet.extras")
    # editor="leaflet.extras"
    # editor="leafpm"
    
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "yellow", "red"))(15),
                        domain = values(rapidez), na.color = "transparent")
    
    m <- m %>% 
      addPolylines(
        data=arrows, 
        color = "black", 
        opacity = 1.0, 
        weight = 1,
        group = "prof" %,% prof
      ) %>%
      # addProviderTiles("CartoDB") %>%
      # addProviderTiles(providers$CartoDB.Positron) %>% # ) %>% # 
      addRasterImage(rapidez, colors = pal, opacity = 0.5, group = "prof" %,% prof) %>%
      addLegend(position = "bottomright",
                pal = pal, 
                values = values(rapidez), 
                title = "Rapidez prof" %,% prof,
                group = "prof" %,% prof)
  }
  
  m <- m %>%
    addLayersControl(
      # baseGroups = ,
      overlayGroups = "prof" %,% profs,
      position = "bottomleft",
      options = layersControlOptions(collapsed = F, hideSingleBase =  T)
    )
  
  
  return(m)
}

MuestraVar <- function (f0, v, profs=1:3, t0=1) { # prof IN 1:3
  # f0: Nombre del archivo nc
  #     p.ej. sourdir %,% "problema_I2.nc"
  # v: Nombre de la variable a graficar
  # prof: profundidad codificada IN 1:3
  
  m <- leaflet() %>% addTiles() # mapa inicial sin nada
  
  for (prof in profs) {
    V <- raster(f0, var = v, band = prof, lvar=4, level=t0) 

    # ============== CONSTRUCCIÓN MAPA ==================
    
    # editor=NULL # usa el editor default ("leaflet.extras")
    # editor="leaflet.extras"
    # editor="leafpm"
    
    pal <- colorNumeric(palette = colorRampPalette(c("blue", "green", "red"))(15),
                        domain = values(V), na.color = "transparent")
    
    m <- m %>% 
      # addProviderTiles("CartoDB") %>%
      # addProviderTiles(providers$CartoDB.Positron) %>% # ) %>% # 
      addRasterImage(V, colors = pal, opacity = 0.5, group = "prof" %,% prof) %>%
      addLegend(position = "bottomright",
                pal = pal, 
                values = values(V), 
                title = v %,% " prof" %,% prof,
                group = "prof" %,% prof)
  }
  
  m <- m %>%
    addLayersControl(
      # baseGroups = ,
      overlayGroups = "prof" %,% profs,
      position = "bottomleft",
      options = layersControlOptions(collapsed = F, hideSingleBase =  T)
    )
  
  return(m)
}

