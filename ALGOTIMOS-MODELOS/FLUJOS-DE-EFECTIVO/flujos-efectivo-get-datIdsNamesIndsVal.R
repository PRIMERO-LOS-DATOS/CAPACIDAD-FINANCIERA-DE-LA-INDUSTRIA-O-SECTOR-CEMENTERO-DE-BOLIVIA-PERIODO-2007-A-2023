getDatIdsNamesValIndicadores <- function() {
    
    indicadoresValId <- data.frame(
        INDICADOR = c("FLUJO_EFECTIVO_POR_ACTIVO", 
                      "FLUJO_EFECTIVO_POR_PASIVO",
                      "FLUJO_EFECTIVO_POR_PATRIMONIO",
                      "FLUJO_EFECTIVO_ACTUAL",
                      
                      "ROE", 
                      "WACC",
                      "VALOR_GENERADO",
                      
                      "TOTAL_FLUJOS_DESCONTADOS"
                      
        ),
        NOMBRES = c('Flujo generado por el activo (FGPA)',
                    'Flujo generado por el pasivo (FGPP)',
                    'Flujo generado por el patrimonio (FGPPAT)',
                    'Flujo de efectivo generado en el periodo (FP)',
                    
                    'Rendimiento sobre patrimonio',
                    'Costo promedio de capital',
                    'Valor generado',
                    
                    'Total flujos descontados'
        ),
        DIRECCION = c('ASCENDENTE',        
                      'ASCENDENTE',
                      'ASCENDENTE',
                      'ASCENDENTE',
                      
                      'ASCENDENTE',
                      'DESCENDIENTE',
                      'ASCENDENTE', 
                      
                      'ASCENDENTE'
                      
        )
    )
    
    indicadoresValId$DECRECIENTE <- ifelse(indicadoresValId$DIRECCION=='ASCENDENTE', TRUE, FALSE)
    
    
    return(indicadoresValId)
    
}
