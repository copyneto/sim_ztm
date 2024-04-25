@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Motorista Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_MOT
  as select from    /scmtms/d_torrot as torrot

    inner join      /scmtms/d_torite as torite       on torrot.db_key = torite.parent_key

    left outer join dfkkbptaxnum     as docMotorista on  torite.res_id        = docMotorista.partner
                                                     and docMotorista.taxtype = 'BR2'

    left outer join but000           as bp           on bp.partner = torite.res_id
{
  key torrot.tor_id                                     as Orp_Id,
      torite.res_id                                     as BpMotorista,
      //      torite.item_descr   as Motorista,
      docMotorista.taxnum                               as DocMotorista,
      concat_with_space(bp.name_first, bp.name_last, 1) as Motorista
}
where
  torite.item_cat = 'DRI'
