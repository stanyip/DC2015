library(maptools)
library(rgeos)
library(xlsx)
library(leaflet)
library(HK80)
library(htmltools)
library(stringi)
library(stringr)
library(dplyr)
setwd('C:/Users/Stan/Desktop/electionpublic')
dctable = read.xlsx('matchup_2015to2011.xlsx',1,encoding='UTF-8')
results2015 = read.xlsx('dc2015_2.xlsx',1,encoding='UTF-8')
setwd('C:/Users/Stan/Desktop/electionpublic/Shapefiles') # change the working directory
sh2015 = readShapePoly('DC2015/GIH3_DC_2015_POLY.shp')
results2015 = read.xlsx('dc2015_2.xlsx',1,encoding='UTF-8')
results2015$Affiliation = str_replace_all(results2015$Affiliation,'\\*','')
chnname = dctable$Name2015[match(sh2015$CACODE,dctable$Code2015)]
chnname = sapply(sapply(str_split(str_replace_all(stri_trans_general(as.character(chnname),'any-hex'),'\\\\u','u0x'),'u'),function(x) strtoi(x[x!=''])),function(y) paste0('&#',paste(y,collapse='&#')))
chnname = paste0('(2015 ',sh2015$CACODE,') ',chnname)
results2015$Name = sapply(sapply(str_split(str_replace_all(stri_trans_general(results2015$Name,'any-hex'),'\\\\u','u0x'),'u'),function(x) strtoi(x[x!=''])),function(y) paste0('&#',paste(y,collapse='&#')))
results2015$Affiliation = sapply(sapply(str_split(str_replace_all(stri_trans_general(results2015$Affiliation,'any-hex'),'\\\\u','u0x'),'u'),function(x) strtoi(x[x!=''])),function(y) paste0('&#',paste(y,collapse='&#')))
results2015$Affiliation[results2015$Affiliation=='&#'] = ''
u = split(results2015,results2015$Code)
votes = sapply(u,function(X) paste(apply(X[c('Number','Name','Vote','Affiliation')],1,function(x) paste(x,collapse=' ')),collapse='<br>'))
n = length(sh2015)
showvotes = rep('',n)
showvotes[match(names(u),sh2015$CACODE)] = votes
sh2015poly = list()
for (i in 1:n) {
sh2015poly[[i]] = data.frame(t(apply(sh2015[i,]@polygons[[1]]@Polygons[[1]]@coords,1,function(x) rev(unlist(HK1980GRID_TO_WGS84GEO(x[2],x[1]))))))
}
id = sh2015$CACODE
n = length(id)
shp2015 = SpatialPolygons(sapply(1:n,function(x) Polygons(list(Polygon(sh2015poly[[x]])),id[x])))
htmls = paste0('<head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><head>',chnname,'<br>',showvotes)
sh2011 = readShapePoly('DC2011/dcbdry_polygon.shp')
n11 = length(sh2011)
sh2011poly = list()
for (i in 1:n11) {
sh2011poly[[i]] = data.frame(t(apply(sh2011[i,]@polygons[[1]]@Polygons[[1]]@coords,1,function(x) rev(unlist(HK1980GRID_TO_WGS84GEO(x[2],x[1]))))))
}
shp2011 = SpatialPolygons(sapply(1:n11,function(x) Polygons(list(Polygon(sh2011poly[[x]])),id[x])))
leaflet() %>% addTiles() %>% 
addPolygons(data=shp2011,weight=4, color = 'red',opacity=0.8,fill=F, group='2011') %>%
addPolygons(data=shp2015,weight=3, color = 'black',opacity=0.8,fill=T, fillOpacity=0, popup=htmls,group='2015') %>%
 addLayersControl(
    overlayGroups = c('2011','2015'),
    options = layersControlOptions(collapsed = FALSE)
)

