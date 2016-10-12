library(ggplot2)

elev <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-01-elevation.csv')
codes <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-02-species-codes.csv')
trees <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-03-trees-2014.csv')
stems <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-04-stems-2014.csv')

codes[agrep('castanea',codes[,'latin'],ign=T),]

cade.tr <- trees[trees[,'sp'] == 'castde',]
cade.st <- stems[stems[,'sp'] == 'castde',]
cade <- rbind(cade.tr[,colnames(cade.tr) %in% colnames(cade.st)],cade.st[,colnames(cade.st) %in% colnames(cade.tr)])
cade <- cade[duplicated(cade[,'tree.id']),]

head(cade)
table(cade[,'df.status'])

cade.plot <- ggplot(data=cade,aes(x=gx,y=gy,color=df.status)) + geom_point() 
contour <- ggplot(data=elev,aes(x=col.x,y=col.y,z=col.elev)) + geom_contour(color='darkgrey')

contour + geom_point(data=cade,aes(x=gx,y=gy,color=df.status))

ggplot() +
    stat_contour(data = elev, aes(x = col.x, y = col.y, z = col.elev, color = ..level..)) + 
        labs(color = "density") + 
            geom_point(data = cade, aes(x = gx, y = gy, fill = factor(df.status)), pch = 21) + 
                scale_color_continuous(guide=guide_legend(title='Elevation')) + 
                    scale_fill_manual(guide=guide_legend(title='Status'),values=c('lightgreen','darkred')) 

