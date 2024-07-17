getDatWacc <- function(dat, desagregados = FALSE) {
    
    if (!desagregados) {
        datWACC <- 
            dat %>% 
            select(
                FECHA,
                TIPO_DE_ENTIDAD, 
                PASIVO, 
                PATRIMONIO,
                GASTOS_FINANCIEROS,
                EERR_S2_RESULTADO_NETO_DE_LA_GESTION,
                ACTIVO)
    }else{
        
        datWACC <- 
            dat %>% 
            mutate(
                FECHA=FECHA,
                TIPO_DE_ENTIDAD=TIPO_DE_ENTIDAD,
                PASIVO = CD_200_00,
                PATRIMONIO= CD_300_00,
                GASTOS_FINANCIEROS= CD_410_00,
                EERR_S2_RESULTADO_NETO_DE_LA_GESTION=CD_CC0_07,
                ACTIVO=CD_100_00
            )
    }
    

    
    
    
    
    datWACC <- datWACC %>% mutate("D" = PASIVO, 
                                  "E" = PATRIMONIO) 
    datWACC <- datWACC %>% mutate("ROA" = EERR_S2_RESULTADO_NETO_DE_LA_GESTION/ACTIVO, 
                                  "ROE" = EERR_S2_RESULTADO_NETO_DE_LA_GESTION/PATRIMONIO) 
    
    datWACC <- datWACC %>% 
        mutate("Kd" = abs(GASTOS_FINANCIEROS/PASIVO)) %>% 
        mutate("Kd"=ifelse(is.nan(Kd), 0, Kd))
    
    
    datWACC <- datWACC %>% mutate("Ke" = rep(0.0126285,n())) # Se debe buscar una tasa libre de riesgo
     
    datWACC <- datWACC %>% mutate("t" = rep(0.25/12,n())) # IUE mensual
    
    datWACC <- datWACC %>% mutate("WACC" = ((E*Ke)+(D*Kd*(1-t)))/(D+E))
    
    
    
    datWACC <- datWACC %>% mutate("VALOR_GENERADO" = ROE-WACC)

    return(datWACC)
}
