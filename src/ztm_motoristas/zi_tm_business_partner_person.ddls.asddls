@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parceiro de negócio - Pessoa Física'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_BUSINESS_PARTNER_PERSON
  as select from    but000       as _partner

    left outer join dfkkbptaxnum as _taxnum on _taxnum.partner = _partner.partner

    left outer join but0id       as _id     on _id.partner = _partner.partner

{
  key _partner.partner                                       as partner,
      _partner.type                                          as type,
      _partner.bu_group                                      as BU_GROUP,

      max( case when _taxnum.taxtype = 'BR1'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR1'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )         as cnpj,

      max( case when _taxnum.taxtype = 'BR2'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR2'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )         as cpf,

      max( case when _taxnum.taxtype = 'BR3'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR3'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )         as ie,

      max( case when _taxnum.taxtype = 'BR5'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR5'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )         as rg,

      max( case when _id.type = 'ZCNH'
                then _id.idnumber
                else cast( '' as bptaxnumxl  ) end )         as cnh,

      max( case when _id.type = 'ZCNH'
                then _id.valid_date_to
                else cast( '' as bu_id_valid_date_to ) end ) as Validadecnh
}
where
  _partner.type = '1' -- Pessoa
group by
  _partner.partner,
  _partner.type,
  _partner.bu_group
