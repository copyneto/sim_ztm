@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Motorista - Documentos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_MOTORISTA_DOCS
  as select from I_BusinessPartner as _partner

  association [0..*] to dfkkbptaxnum as _taxnum                  on  _taxnum.partner = $projection.partner

  association [0..*] to but0id       as _id                      on  _id.partner = $projection.partner

  association [0..*] to but100       as _role                    on  _role.partner = $projection.partner

  association [0..1] to tb002        as _BusinessPartnerGrouping on  _BusinessPartnerGrouping.spras    = $session.system_language
                                                                 and _BusinessPartnerGrouping.bu_group = $projection.BusinessPartnerGrouping

{
  key _partner.BusinessPartner                                                                as partner,

      max( case when _partner.PersonFullName is not initial
           then _partner.PersonFullName
           else _partner.BusinessPartnerFullName
           end )                                                                              as partnername,

      _partner.BusinessPartnerUUID                                                            as partner_guid,

      case when BusinessPartnerGrouping = 'ZPUN' -- Funcionários
           then cast( 'X' as abap_boolean )
           else cast( '' as abap_boolean )
           end                                                                                as BusinessPartnerRedeSim,

      _partner.BusinessPartnerGrouping                                                        as BusinessPartnerGrouping,
      max( _BusinessPartnerGrouping.txt40 )                                                   as BusinessPartnerGroupingText,

      max( case when _taxnum.taxtype = 'BR1'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR1'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )                                          as cnpj,

      max( case when _taxnum.taxtype = 'BR2'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR2'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )                                          as cpf,

      max( case when _taxnum.taxtype = 'BR3'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR3'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )                                          as ie,

      max( case when _taxnum.taxtype = 'BR5'
                 and _taxnum.taxnum is not initial
                then _taxnum.taxnum
                when _taxnum.taxtype = 'BR5'
                 and _taxnum.taxnumxl is not initial
                then _taxnum.taxnumxl
                else cast( '' as bptaxnumxl  ) end )                                          as rg,

      max( case when _id.type = 'ZCNH'
                then _id.idnumber
                else cast( '' as bptaxnumxl  ) end )                                          as cnh,

      max( case when _id.type = 'ZCNH'
                then _id.valid_date_to
                else cast( '00000000' as bu_id_valid_date_to ) end )                          as Validadecnh,

      max( case when _id.type = 'ZCNH'
                then _id.institute
                else cast( '' as bu_id_institute  ) end )                                     as Categoriacnh,

      max( case when _role.rltyp = 'TM0001'
                then cast( 'X' as abap_boolean )
                else cast( '' as abap_boolean  ) end )                                        as TM,

      max( _partner.CreatedByUser )                                                           as CreatedBy,

      max( cast( concat( _partner.CreationDate, _partner.CreationTime ) as timestampl ) )     as CreatedAt,

      max( _partner.LastChangedByUser )                                                       as LastChangedBy,

      max( cast( concat( _partner.LastChangeDate, _partner.LastChangeTime ) as timestampl ) ) as LastChangedAt,

      max( cast( concat( _partner.LastChangeDate, _partner.LastChangeTime ) as timestampl ) ) as LocalLastChangedAt

}
where
     _partner.BusinessPartnerGrouping = 'ZPPF' -- Parceiros pessoa fisica
  or _partner.BusinessPartnerGrouping = 'ZPUN' -- Funcionários
group by
  _partner.BusinessPartner,
  _partner.BusinessPartnerUUID,
  _partner.BusinessPartnerGrouping
