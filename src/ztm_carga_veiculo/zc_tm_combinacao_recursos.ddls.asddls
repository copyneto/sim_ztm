@EndUserText.label: 'CDS Proj. - Carga de combinações de veículos em massa'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZC_TM_COMBINACAO_RECURSOS
  provider contract transactional_query
  as projection on ZI_TM_COMBINACAO_RECURSOS
{

//      @EndUserText.label: 'COMBINATION_RESOURCE_ID'
      key combination_resource_id,
//      @EndUserText.label: 'SEQUENCE_NO'
      key SeqNum,
//      @EndUserText.label: 'EQUIPMENT_GROUP'
      EquiType,
//      @EndUserText.label: 'EQUIPMENT_TYPE'
      EquiCode,
//      @EndUserText.label: 'RESOURCE_ID'
      resource_id,
      @EndUserText.label: 'Log'
      LogMessage,
      @EndUserText.label: 'Tipo Log'
      LogType,
      Botao
      
}
