getTsFromDat <- function(dat) {
    
    #dat <- datCartTipoCredito
    require(forecast)
    require(tsbox)
    
    for (nameColumn in names(dat)[c(-1,-2)]) {
        
        print(nameColumn)
        id <- nameColumn
        
        dat$TIPO_DE_ENTIDAD <- gsub('_', ' ', dat$TIPO_DE_ENTIDAD)
        
        gestInc <- min(as.numeric(format(dat$FECHA, format='%Y')))
        gestFn <- max(as.numeric(format(dat$FECHA, format='%Y')))
        
        tsDat <- ts(matrix(0, nrow=(gestFn-gestInc+1)*12, ncol=length(unique(dat$TIPO_DE_ENTIDAD))), 
                    start=gestInc, frequency=12)
        
        for (i in 1:length(unique(dat$TIPO_DE_ENTIDAD))) {
            
            tipoEnt <- as.character(unique(dat$TIPO_DE_ENTIDAD)[i])
            colnames(tsDat)[i] <- tipoEnt
            
            x <- dat[dat$TIPO_DE_ENTIDAD==tipoEnt,][order(dat[dat$TIPO_DE_ENTIDAD==tipoEnt,'FECHA']),id]
            
            x <- sapply(x, as.numeric)
            tsDat[,i] <- x
        }
        
        for (i in 1:length(unique(dat$TIPO_DE_ENTIDAD))) {
            tsDat[,i] <-  na.interp(tsDat[,i])
            
        }
        
        
        dat[,id] <- ts_df(tsDat)[,'value']
        
    }
    
    return(dat)
}

