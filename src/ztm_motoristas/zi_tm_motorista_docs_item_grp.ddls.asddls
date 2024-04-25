@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Motorista - Documentos Adicionais'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_MOTORISTA_DOCS_ITEM_GRP

  -- Documentos encontrados em Parceiros cadastrados no Aplicativo

  as select from ZI_TM_DOC_ADD as _filho
{
  key _filho.id                as id,
  key _filho.bp                as bp,
      min( NumeroCriticality ) as NumeroCriticality,
      'APP'                    as source
}
group by
  _filho.id,
  _filho.bp

-- Documentos encontrados somente no BP e que não estão nos parceiros

//union select from but0id        as _id
//
//  left outer join ZI_TM_DOC_ADD as _filho on  _filho.bp     = _id.partner
//                                          and _filho.Tipo   = _id.type
//                                          and _filho.Numero = _id.idnumber
//{
//  key _id.partner      as bp,
//      min( case when _filho.Numero is null
//                  or _filho.Numero is initial
//                then 1
//                when _id.institute       <> _filho.Descricao
//                  or _id.region          <> _filho.Uf
//                  or _id.valid_date_to   <> _filho.Validade
//                then 2
//                else 3
//                end  ) as NumeroCriticality,
//      'BP'             as source
//
//}
//where
//  _id.type <> 'ZCNH'
//group by
//  _id.partner
