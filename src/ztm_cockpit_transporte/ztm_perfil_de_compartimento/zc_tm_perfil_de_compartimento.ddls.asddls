@EndUserText.label: 'CDS Consumo - Perfil de Compartimento'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_TM_PERFIL_DE_COMPARTIMENTO provider contract transactional_query
    as projection on ZI_TM_PERFIL_DE_COMPARTIMENTO 
{
    @EndUserText.label: 'Perfil do compartimento'
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_PERFIL_DE_COMPARTIMENTO', element: 'Cmprofile' }}]
    key Cmprofile, //  
    
    key Sequence,
    
    @EndUserText.label: 'Tipo de Compartimento'
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_Tipo_Compartimento', element: 'CType' }, 
                           additionalBinding: [{  element: 'CTCapa', localElement: 'CTCapa' }, {  element: 'CTUnit', localElement: 'CTUnit', usage: #RESULT }]
    }]
    CType,
    CTypeCriticality,    
    
    @EndUserText.label: 'Descrição'
    Text,
    
    @EndUserText.label: 'Cap. Compartimento'
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_Tipo_Compartimento', element: 'CTCapa' }, 
                   additionalBinding: [{ element: 'CTUnit', localElement: 'CTUnit', usage: #RESULT }, { element: 'CType', localElement: 'CType' }]
    }]
    CTCapa,
    
    @EndUserText.label: 'UM'
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'I_UnitOfMeasure', element: 'UnitOfMeasure' }}]
    CTUnit,
    
    ZzSealNumberTotal
}
