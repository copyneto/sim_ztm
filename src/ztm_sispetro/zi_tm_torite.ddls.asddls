@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Torite'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_TORITE
  as select from ZI_TM_TORITE_AUX as _torite
  association [1..1] to but000  as _pnname    on _pnname.partner = $projection.Motorista
{
  key parent_key,
      Centro,
      Placavalo,
      Placarreta1,
      Placarreta2,
      SeqFrota4,
      Motorista,
      concat_with_space( concat_with_space( concat_with_space( concat_with_space(_pnname.name_first, ' ', 1), _pnname.namemiddle, 1 ), ' ', 1 ) , _pnname.name_last, 1 ) as DescMotorista
}
