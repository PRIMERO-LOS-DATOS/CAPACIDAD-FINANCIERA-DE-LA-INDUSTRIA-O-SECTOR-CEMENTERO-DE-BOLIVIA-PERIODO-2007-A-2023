
getDatCashFlow <- function(dat, desagregados=FALSE) {
    
    if (!desagregados) {
        
        datCashFlow <- data.frame(
            
            FECHA = dat$FECHA,
            TIPO_DE_ENTIDAD = dat$TIPO_DE_ENTIDAD,
            
            DISPONIBILIDADES_PERIODO_ACTUAL = dat$ACTIVO_DISPONIBILIDADES,
            
            ACTIVO_PERIODO_ACTUAL = dat$ACTIVO,
            
            PASIVO_PERIODO_ACTUAL = dat$PASIVO,
            
            PATRIMONIO_PERIODO_ACTUAL = dat$PATRIMONIO
        )
        
    }else{
        
        datCashFlow <- data.frame(
            
            FECHA = dat$FECHA,
            TIPO_DE_ENTIDAD = dat$TIPO_DE_ENTIDAD,
            
            DISPONIBILIDADES_PERIODO_ACTUAL = dat$CD_110_00,
            
            ACTIVO_PERIODO_ACTUAL = dat$CD_100_00,
            
            PASIVO_PERIODO_ACTUAL = dat$CD_200_00,
            
            PATRIMONIO_PERIODO_ACTUAL = dat$CD_300_00
        )
    }
    
  
    
    datCashFlow <- datCashFlow %>%
        group_by(TIPO_DE_ENTIDAD) %>%
        mutate(DISPONIBILIDADES_PERIODO_ANTERIOR = lag(DISPONIBILIDADES_PERIODO_ACTUAL, n=1),
               ACTIVO_PERIODO_ANTERIOR = lag(ACTIVO_PERIODO_ACTUAL, n=1),
               PASIVO_PERIODO_ANTERIOR = lag(PASIVO_PERIODO_ACTUAL, n=1),
               PATRIMONIO_PERIODO_ANTERIOR = lag(PATRIMONIO_PERIODO_ACTUAL, n=1)) %>%
        ungroup()
    
    
    datCashFlow$FLUJO_EFECTIVO_POR_ACTIVO <-  
        (datCashFlow$ACTIVO_PERIODO_ANTERIOR -
        datCashFlow$DISPONIBILIDADES_PERIODO_ANTERIOR) -
        
        (datCashFlow$ACTIVO_PERIODO_ACTUAL -
        datCashFlow$DISPONIBILIDADES_PERIODO_ACTUAL )
    
    datCashFlow$FLUJO_EFECTIVO_POR_PASIVO <-  
        datCashFlow$PASIVO_PERIODO_ACTUAL -
        datCashFlow$PASIVO_PERIODO_ANTERIOR
    
    datCashFlow$FLUJO_EFECTIVO_POR_PATRIMONIO <-  
        datCashFlow$PATRIMONIO_PERIODO_ACTUAL -
        datCashFlow$PATRIMONIO_PERIODO_ANTERIOR
    
    datCashFlow$FLUJO_EFECTIVO_ACTUAL <- 
        datCashFlow$FLUJO_EFECTIVO_POR_ACTIVO + 
        datCashFlow$FLUJO_EFECTIVO_POR_PASIVO + 
        datCashFlow$FLUJO_EFECTIVO_POR_PATRIMONIO
    
    datCashFlow$FLUJO_FINAL <- 
        datCashFlow$FLUJO_EFECTIVO_ACTUAL +
        datCashFlow$DISPONIBILIDADES_PERIODO_ANTERIOR
    
    
    return(datCashFlow)
    

    
    
}


#####################################################################################################

getDatCashFlowHard <- function(dat, desagregados=FALSE) {
    
    if (!desagregados) {
        
        datCashFlow <- data.frame(
            
            FECHA = dat$FECHA,
            TIPO_DE_ENTIDAD = dat$TIPO_DE_ENTIDAD,
            
            DISPONIBILIDADES_PERIODO_ACTUAL = dat$ACTIVO_DISPONIBILIDADES_CAJA,
            
            ACTIVO_PERIODO_ACTUAL = dat$ACTIVO,
            
            PASIVO_PERIODO_ACTUAL = dat$PASIVO,
            
            PATRIMONIO_PERIODO_ACTUAL = dat$PATRIMONIO
        )
    }else{
        
        datCashFlow <- data.frame(
            
            FECHA = dat$FECHA,
            TIPO_DE_ENTIDAD = dat$TIPO_DE_ENTIDAD,
            
            DISPONIBILIDADES_PERIODO_ACTUAL = dat$CD_111_00,
            
            ACTIVO_PERIODO_ACTUAL = dat$CD_100_00,
            
            PASIVO_PERIODO_ACTUAL = dat$CD_200_00,
            
            PATRIMONIO_PERIODO_ACTUAL = dat$CD_300_00
        )
    }
    
   
    datCashFlow <- datCashFlow %>%
        group_by(TIPO_DE_ENTIDAD) %>%
        mutate(DISPONIBILIDADES_PERIODO_ANTERIOR = lag(DISPONIBILIDADES_PERIODO_ACTUAL, n=1),
               ACTIVO_PERIODO_ANTERIOR = lag(ACTIVO_PERIODO_ACTUAL, n=1),
               PASIVO_PERIODO_ANTERIOR = lag(PASIVO_PERIODO_ACTUAL, n=1),
               PATRIMONIO_PERIODO_ANTERIOR = lag(PATRIMONIO_PERIODO_ACTUAL, n=1)) %>%
        ungroup()
    
    
    datCashFlow$FLUJO_EFECTIVO_POR_ACTIVO <-  
        (datCashFlow$ACTIVO_PERIODO_ANTERIOR -
             datCashFlow$DISPONIBILIDADES_PERIODO_ANTERIOR) -
        
        (datCashFlow$ACTIVO_PERIODO_ACTUAL -
             datCashFlow$DISPONIBILIDADES_PERIODO_ACTUAL )
    
    datCashFlow$FLUJO_EFECTIVO_POR_PASIVO <-  
        datCashFlow$PASIVO_PERIODO_ACTUAL -
        datCashFlow$PASIVO_PERIODO_ANTERIOR
    
    datCashFlow$FLUJO_EFECTIVO_POR_PATRIMONIO <-  
        datCashFlow$PATRIMONIO_PERIODO_ACTUAL -
        datCashFlow$PATRIMONIO_PERIODO_ANTERIOR
    
    datCashFlow$FLUJO_EFECTIVO_ACTUAL <- 
        datCashFlow$FLUJO_EFECTIVO_POR_ACTIVO + 
        datCashFlow$FLUJO_EFECTIVO_POR_PASIVO + 
        datCashFlow$FLUJO_EFECTIVO_POR_PATRIMONIO
    
    datCashFlow$FLUJO_FINAL <- 
        datCashFlow$FLUJO_EFECTIVO_ACTUAL +
        datCashFlow$DISPONIBILIDADES_PERIODO_ANTERIOR
    
    
    return(datCashFlow)
    
}
