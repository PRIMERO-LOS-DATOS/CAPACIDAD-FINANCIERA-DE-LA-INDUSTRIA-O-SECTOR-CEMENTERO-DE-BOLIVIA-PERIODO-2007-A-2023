getDatFlujosDescontados <- function(datCashFlow, nFlows=12) {
    #lead
    
    datVan <- datCashFlow
    
    for (i in 1:nFlows) {
        datVan <- datVan %>%
            group_by(TIPO_DE_ENTIDAD) %>%
            mutate("FLUJO_{i}_PERIODO_ADELANTE" := lead(FLUJO_EFECTIVO_ACTUAL, n=i)) %>% 
            ungroup()
        
    }
    

    datVan$TASA_DESCUENTO <- rep(0.001438725,nrow(datVan)) # Esto se cambiara por tasa de inflación
    
    datVan <- datVan %>% mutate("TOTAL_FLUJOS_DESCONTADOS" := rep(0,nrow(datVan)))
    
    datVan <- datVan %>%
        group_by(TIPO_DE_ENTIDAD) %>%
        mutate("TIEMPO" = seq(n())) %>% 
        ungroup()
    
    for (i in 1:nFlows) {
        
        colNameFlow <- paste0('FLUJO_',i,'_PERIODO_ADELANTE')
        colNameFlowTotal <- paste0('TOTAL_FLUJOS_DESCONTADOS')
        
        datVan <- datVan %>%
            group_by(TIPO_DE_ENTIDAD) %>%
            mutate("TOTAL_FLUJOS_DESCONTADOS" := (!!as.name(colNameFlowTotal)) + (!!as.name(colNameFlow)/((1+TASA_DESCUENTO)^i)) ) %>% 
            ungroup()
        
    }
    
    
    return(datVan)
}
