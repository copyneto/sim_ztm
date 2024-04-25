@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de pesquisa - Cenário'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_CENARIO_VH
  as select from dd07t
{
  key domvalue_l as Domname,
      ddtext     as Ddtext
}
where
      domname    = 'ZD_CENARIO'
  and ddlanguage = $session.system_language
