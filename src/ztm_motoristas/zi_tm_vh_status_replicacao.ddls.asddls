@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help - Status da Replicação'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZI_TM_VH_STATUS_REPLICACAO
  as select from    dd07l as Objeto
    left outer join dd07t as Text on  Text.domname    = Objeto.domname
                                  and Text.as4local   = Objeto.as4local
                                  and Text.valpos     = Objeto.valpos
                                  and Text.as4vers    = Objeto.as4vers
                                  and Text.ddlanguage = $session.system_language
{
      @EndUserText.label: 'Status'
      @ObjectModel.text.element: ['StatusText']
  key cast( Objeto.domvalue_l as ze_tm_status_replicacao ) as Status,
      @Semantics.text: true
      Text.ddtext                                          as StatusText
}
where
  Objeto.domname = 'ZD_TM_STATUS_REPLICACAO'
