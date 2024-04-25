@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Doc Adicionais (App)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_DOC_ADD_APP
 
  as select from ZI_TM_DOC_ADD_APP_TM as _add

  association [0..1] to but0id as _id on  _id.partner  = $projection.bp
                                      and _id.type     = $projection.Tipo
                                      and _id.idnumber = $projection.Numero
{
  key _add.id                                       as id,
  key _add.paiid                                    as paiid,
      _add.bp                                       as bp,
      _add.Tipo                                     as Tipo,
      _add.Numero                                   as Numero,

      case when _id.idnumber is null                        -- Se Parceiro não existir
             or _id.idnumber is initial                     -- Se Documento estiver vazio
           then 1                                           -- Cor: Vermelho
           when _id.institute       <> _add.Descricao       -- Se houver alguma informação divergente
             or _id.region          <> _add.Uf
             or _id.valid_date_to   <> _add.Validade
           then 2                                           -- Cor: Amarelo
           else 3                                           -- Cor: Verde
           end                                      as NumeroCriticality,

      case when _add.ValidadeDesde is not initial
            and _add.ValidadeDesde is not null
           then _add.ValidadeDesde
           else cast( '00000000' as abap.dats ) end as ValidadeDesde,

      case when _add.Validade is not initial
            and _add.Validade is not null
           then _add.Validade
           else cast( '00000000' as abap.dats ) end as Validade,

      _add.Uf                                       as Uf,
      _add.Descricao                                as Descricao,
      //      cast( '' as abap_boolean preserving type ) as btnDel,
      @Semantics.user.createdBy: true
      _add.CreatedBy                                as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      _add.CreatedAt                                as CreatedAt,
      @Semantics.user.lastChangedBy: true
      _add.LastChangedBy                            as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      _add.LastChangedAt                            as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _add.LocalLastChangedAt                       as LocalLastChangedAt

}
