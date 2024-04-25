@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Perfil de Compartimento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZI_TM_PERFIL_DE_COMPARTIMENTO
  as select from /sapapo/cmprf
  association        to /sapapo/ctcap               as _CTCAP             on _CTCAP.ctype = $projection.CType

  association [0..1] to ZI_TM_VH_Tipo_Compartimento as _TipoCompartimento on _TipoCompartimento.CType = $projection.CType

{
  key cmprofile            as Cmprofile,
  key sequence             as Sequence,
      ctype                as CType,

      case when ctype is initial
           then 0
           when ctype is not initial
            and _TipoCompartimento.CType is not null
            and _TipoCompartimento.CType is not initial
           then 3
           else 1
           end             as CTypeCriticality,

      text                 as Text,
      zz_seal_number_total as ZzSealNumberTotal,
      @Semantics.quantity.unitOfMeasure : 'CTUnit'
      _CTCAP.ctcapa        as CTCapa,
      _CTCAP.ctunit        as CTUnit,


      _CTCAP
}
