### Analysis of chestnut data from the HF megaplot
### MKLau 12October2016

### Check/install/library dependencies
packs <- c('ggplot2')
sapply(packs,function(x) if (!(x %in% installed.packages()[,1])){install.packages(x)})
sapply(packs,require,quiet=T,char=T)


### Import data from HF archives HF253
elev <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-01-elevation.csv')
codes <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-02-species-codes.csv')
trees <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-03-trees-2014.csv')
stems <- read.csv('http://harvardforest.fas.harvard.edu/data/p25/hf253/hf253-04-stems-2014.csv')

### pick species
search.spp <- 'castan';as.character(codes[agrep(search.spp,codes[,'latin'],ign=T),'sp'])
my.sp <- 'castde'

### delimit the dataset to chestnut, merging trees and stems
cade.tr <- trees[trees[,'sp'] == my.sp,]
cade.st <- stems[stems[,'sp'] == my.sp,]
cade <- rbind(cade.tr[,colnames(cade.tr) %in% colnames(cade.st)],cade.st[,colnames(cade.st) %in% colnames(cade.tr)])
cade <- cade[duplicated(cade[,'tree.id']),]

### counts of chestnut
table(cade[,'df.status'])

### chestnut map with elevation contours
ggplot() +
    stat_contour(data = elev, aes(x = col.x, y = col.y, z = col.elev, color = ..level..)) + 
        labs(color = "density") + 
            geom_point(data = cade, aes(x = gx, y = gy, fill = factor(df.status), size=dbh), pch = 21) + 
                scale_fill_manual(guide=guide_legend(title=''),values=c('darkgreen','darkred')) + 
                    scale_color_continuous(guide=guide_legend(title='Elevation')) + 
                        xlab('x') + ylab('y')

