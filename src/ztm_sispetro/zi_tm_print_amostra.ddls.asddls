@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Informações Impressão de Amostra'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_PRINT_AMOSTRA
  as select distinct from    /scmtms/d_torite      as Rite
    inner join      /scmtms/d_torrot      as Rot        on Rot.db_key = Rite.parent_key

  //    inner join      /scmtms/d_torrot      as RotFu     on Rot.db_key = Rite.zz_fu_db_key
    inner join      /scmtms/d_torrot      as RotFu      on RotFu.tor_id = Rite.zz_fu_tor_id

    left outer join /scmtms/d_torite      as RiteFuPRD  on  RiteFuPRD.parent_key = RotFu.db_key
                                                        and RiteFuPRD.item_cat   = 'PRD'

 
  //    left outer join /scmtms/d_torite      as RitePRD   on  RitePRD.parent_key      = Rite.parent_key
  //                                                       and RitePRD.item_parent_key = Rite.db_key
  //                                                       and RitePRD.item_cat        = 'PRD'



    left outer join makt                  as Makt       on  makt.matnr = RiteFuPRD.product_id
                                                        and makt.spras = $session.system_language
    left outer join t001k                 as T001K      on T001K.bwkey = RiteFuPRD.erp_plant_id
    left outer join t001w                 as T001w      on T001w.werks = RiteFuPRD.erp_plant_id
    left outer join dfkkbptaxnum          as Distrb     on  Distrb.partner = T001w.lifnr
                                                        and Distrb.taxtype = 'BR1'
    left outer join dfkkbptaxnum          as Transp     on  Transp.partner = Rot.tspid
                                                        and Transp.taxtype = 'BR1'
    left outer join /BOFU/CV_BPRoot       as TranspDesc on  TranspDesc.partner                = Rot.tspid
                                                        and TranspDesc.life_cycle_status_code = '2'
    left outer join I_BR_NFDocumentFlow_C as DocFlow    on DocFlow.PredecessorReferenceDocument = substring(
      RotFu.base_btd_id, 26, 35
    )

  //    left outer join I_BR_NFDocumentFlow_C as DocFlow   on DocFlow.PredecessorReferenceDocument = substring(
  //      RitePRD.base_btd_id, 26, 35
  //    )
    left outer join I_BR_NFeActive        as NfActive   on  NfActive.BR_NotaFiscal        = DocFlow.BR_NotaFiscal
                                                        and NfActive.BR_NFIsCanceled  <> 'X'
    left outer join /scmtms/d_torite      as Motrst     on  Motrst.parent_key = Rite.parent_key
                                                        and Motrst.item_cat   = 'DRI'
    left outer join dfkkbptaxnum          as TaxMotrst  on  TaxMotrst.partner = Motrst.res_id
                                                        and TaxMotrst.taxtype = 'BR5'
    left outer join ZI_TM_SISPETRO        as Sispetro   on Sispetro.db_key = Rite.parent_key
    left outer join /BOFU/CV_BPRoot       as Revend     on  Revend.partner                = RotFu.consigneeid
                                                        and Revend.life_cycle_status_code = '2'
    left outer join dfkkbptaxnum          as TaxRevend  on  TaxRevend.partner = RotFu.consigneeid
                                                        and TaxMotrst.taxtype = 'BR1'
    left outer join but000                as _pnname    on _pnname.partner = Motrst.res_id

{
  key Rite.db_key,
  key Rite.parent_key,
  key Rot.tor_id,
      RiteFuPRD.product_id,
      makt.maktx,
      Rot.created_on,
      t001k.bukrs,
      T001w.name1                                                                                                as NameDistrib,
      Distrb.taxnum                                                                                              as CnpjDistrib,
      case when Distrb.taxnum is initial or Distrb.taxnum is null then ''
           when length(Distrb.taxnum) <> 14 then Distrb.taxnum
       else concat( substring(Distrb.taxnum, 1, 2),
                     concat( '.',
                     concat( substring(Distrb.taxnum, 3, 3),
                     concat( '.',
                     concat( substring(Distrb.taxnum, 6, 3),
                     concat( '/',
                     concat( substring(Distrb.taxnum, 9, 4),
                     concat( '-',  substring(Distrb.taxnum, 13, 2) ) ) ) ) ) ) ) ) end                           as CnpjDistrib_Text,
      NfActive.BR_NotaFiscal,
      TranspDesc.description                                                                                     as Transp,
      Transp.taxnum                                                                                              as CnpjTransp,
      case when Transp.taxnum is initial or Transp.taxnum is null then ''
           when length(Transp.taxnum) <> 14 then Transp.taxnum
       else concat( substring(Transp.taxnum, 1, 2),
                    concat( '.',
                    concat( substring(Transp.taxnum, 3, 3),
                    concat( '.',
                    concat( substring(Transp.taxnum, 6, 3),
                    concat( '/',
                    concat( substring(Transp.taxnum, 9, 4),
                    concat( '-',  substring(Transp.taxnum, 13, 2) ) ) ) ) ) ) ) ) end                            as CnpjTransp_Text,
      Motrst.item_descr                                                                                          as NameMotrst,
      TaxMotrst.taxnumxl                                                                                         as RgMotrst,
      case when Rite.mtr = 'ZMTR-TRK' or Rite.mtr = 'ZMTR-CAV'
            then concat( Sispetro.Placavalo, concat(( case when Sispetro.Placarreta1 is not initial then concat( '-', Sispetro.Placarreta1 ) else '' end ),
                                                      concat( ( case when Sispetro.Placarreta2 is not initial then concat( '-', Sispetro.Placarreta2 ) else '' end ),
                                                              ( case when Sispetro.SeqFrota4 is not initial then concat( '-', Sispetro.SeqFrota4 ) else '' end ) ) ) )
          else '' end                                                                                            as Placa,
      Rite.ct_seq,
      Revend.description                                                                                         as RzSociRevend,
      TaxRevend.taxnum                                                                                           as CnpjRevend,
      case when TaxRevend.taxnum is initial or TaxRevend.taxnum is null then ''
           when length(TaxRevend.taxnum) <> 14 then TaxRevend.taxnum
       else concat( substring(TaxRevend.taxnum, 1, 2),
                    concat( '.',
                    concat( substring(TaxRevend.taxnum, 3, 3),
                    concat( '.',
                    concat( substring(TaxRevend.taxnum, 6, 3),
                    concat( '/',
                    concat( substring(TaxRevend.taxnum, 9, 4),
                    concat( '-',  substring(TaxRevend.taxnum, 13, 2) ) ) ) ) ) ) ) ) end                         as CnpjRevend_Text,
      Revend.description                                                                                         as RespReceb,
      Rite.zz_sample_envelope,

      Rite.zz_seal_number_1,
      Rite.zz_seal_number_2,
      Rite.zz_seal_number_3,
      Rite.zz_seal_number_4,
      Rite.zz_seal_number_5,
      Rite.zz_seal_number_6,
      Rite.zz_seal_number_7,
      Rite.zz_seal_number_8,
      concat( NfActive.BR_NFeNumber, concat( '/', NfActive.BR_NFeSeries ) )                                      as NotaFiscal,
      concat_with_space( concat_with_space( concat_with_space( concat_with_space(_pnname.name_first, ' ', 1), _pnname.namemiddle, 1 ), ' ', 1 ) , _pnname.name_last, 1 ) as pnfullname

}
where
      Rite.item_cat           = 'CT'
  and Rite.zz_sample_envelope is not initial
  and NfActive.BR_NotaFiscal  is not initial
