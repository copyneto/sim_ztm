@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Transporte - Quantidade de lacres'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_GET_QTY_SEALS
  as select from /SCMTMS/CV_EquipResourceRoot as cv_equipresourceroot

    inner join   /sapapo/cmprf                as cmprf on cmprf.cmprofile = cv_equipresourceroot.CMPROFILE
{
  cv_equipresourceroot.RES_ID as res_id,
  cmprf.sequence              as sequence,
  cmprf.ctype                 as ctype,
  cmprf.zz_seal_number_total  as zz_seal_number_total
}
