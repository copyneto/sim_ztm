@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Doc Adicionais (App)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_DOC_ADD_APP_TM

  as select from zttm_doc_add as _add

  association [0..1] to zttm_motoristas as _motorista on _motorista.id = $projection.id
{
  key _add.id                                            as id,
  key _add.paiid                                         as paiid,
      _motorista.bp                                      as bp,
      _add.tipo                                          as Tipo,
      _add.numero                                        as Numero,

      case when _add.validadedesde is not initial
                 and _add.validadedesde is not null
                then _add.validadedesde
                else cast( '00000000' as abap.dats ) end as ValidadeDesde,

      case when _add.validade is not initial
            and _add.validade is not null
           then _add.validade
           else cast( '00000000' as abap.dats ) end      as Validade,

      _add.uf                                            as Uf,
      _add.descricao                                     as Descricao,
      @Semantics.user.createdBy: true
      _add.created_by                                    as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      _add.created_at                                    as CreatedAt,
      @Semantics.user.lastChangedBy: true
      _add.last_changed_by                               as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      _add.last_changed_at                               as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _add.local_last_changed_at                         as LocalLastChangedAt
      
}
