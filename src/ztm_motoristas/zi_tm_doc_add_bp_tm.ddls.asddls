@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Doc Adicionais (BP)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_DOC_ADD_BP_TM

  as select from but0id as _id

  association [0..1] to but000        as _partner on  _partner.partner = $projection.bp

  association [0..1] to but100        as _role    on  _role.partner = $projection.bp
                                                  and _role.rltyp   = 'TM0001'
                                                  and _role.dfval   = ''

  association [0..1] to ZI_TM_VH_TIPO as _type    on  _type.type = $projection.Tipo

{
  key _partner.partner_guid   as id,
  key _id.idnumber_guid       as paiid,
      _id.partner             as bp,
      _id.type                as Tipo,
      _id.idnumber            as Numero,
      cast( 0 as abap.int1 )  as NumeroCriticality,
      _id.valid_date_from     as ValidadeDesde,
      _id.valid_date_to       as Validade,
      _id.region              as Uf,
      _id.institute           as Descricao,
      @Semantics.user.createdBy: true
      cast( '' as sychar12 )  as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      cast( 0 as timestampl ) as CreatedAt,
      @Semantics.user.lastChangedBy: true
      cast( '' as sychar12 )  as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      cast( 0 as timestampl ) as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      cast( 0 as timestampl ) as LocalLastChangedAt
}
where
      _role.rltyp = 'TM0001'  -- Somente os parceiros com função TM
  and _type.type  is not null -- Somente tipos cadastrados no parâmetro
