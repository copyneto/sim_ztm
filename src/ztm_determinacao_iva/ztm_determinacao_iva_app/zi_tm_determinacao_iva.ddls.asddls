@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Determinação de IVA'

define root view entity ZI_TM_DETERMINACAO_IVA
  as select from zttm_determ_iva as _Determ
{
  key    cenario        as Cenario,
//  key    tomador        as Tomador,
  key    uforigem       as Uforigem,
  key    ufdestino      as Ufdestino,
  key    uftransp       as Uftransp,
         transportadora as Transportadora,
         clientefornec  as Clientefornec,
         iva            as Iva
         
}
