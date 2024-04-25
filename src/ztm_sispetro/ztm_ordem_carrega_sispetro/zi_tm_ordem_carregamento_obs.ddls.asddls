@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Observação Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_OBS
  as select from /scmtms/d_torrot     as torrot

    inner join   /scmtms/d_torite     as torite         on torrot.db_key = torite.parent_key

    inner join   ZI_CA_GET_PARAMETER  as _ParamMaterial on  _ParamMaterial.Modulo = 'TM'
                                                        and _ParamMaterial.Chave1 = 'APP_ORDEM_CARREGAMENTO'
                                                        and _ParamMaterial.Chave2 = 'FORM_ORDEM_RETIRADA'
                                                        and _ParamMaterial.Chave3 = 'CLASSE_AVALIACAO_MATERIAL'

    inner join   mbew                 as mbew_A003      on  mbew_A003.matnr = torite.product_id
                                                        and mbew_A003.bklas = _ParamMaterial.Low
//                                                        and mbew_A003.bwkey = torite.erp_plant_id

    inner join   A_BillOfMaterial     as bill_mat       on  torite.product_id   = bill_mat.Material
                                                        and torite.erp_plant_id = bill_mat.Plant

    inner join   A_BillOfMaterialItem as bill_mat_ite   on bill_mat.BillOfMaterial = bill_mat_ite.BillOfMaterial

    inner join   ZI_CA_GET_PARAMETER  as _ParamAditivo  on  _ParamAditivo.Modulo = 'TM'
                                                        and _ParamAditivo.Chave1 = 'APP_ORDEM_CARREGAMENTO'
                                                        and _ParamAditivo.Chave2 = 'FORM_ORDEM_RETIRADA'
                                                        and _ParamAditivo.Chave3 = 'CLASSE_AVALIACAO_ADITIVO'

    inner join   mbew                 as mbew_A021      on  mbew_A021.matnr = bill_mat_ite.BillOfMaterialComponent
                                                        and mbew_A021.bklas = _ParamAditivo.Low
//                                                        and mbew_A021.bwkey = torite.erp_plant_id


{
  key torrot.tor_id       as Orp_Id,
      torite.item_descr,
      torite.gro_wei_uni  as UnidadeMedida,
      @Semantics.quantity.unitOfMeasure: 'UnidadeMedida'
      torite.gro_vol_val,
      bill_mat_ite.BillOfMaterialItemUnit,
      @Semantics.quantity.unitOfMeasure: 'BillOfMaterialItemUnit'
      bill_mat_ite.BillOfMaterialItemQuantity,
      bill_mat_ite.ComponentDescription,
      mbew_A021.bklas,
      torite.erp_plant_id as Centro
}
group by
  torrot.tor_id,
  torite.item_descr,
  torite.gro_wei_uni,
  torite.gro_vol_val,
  bill_mat_ite.BillOfMaterialItemUnit,
  bill_mat_ite.BillOfMaterialItemQuantity,
  bill_mat_ite.ComponentDescription,
  mbew_A021.bklas,
  torite.erp_plant_id
