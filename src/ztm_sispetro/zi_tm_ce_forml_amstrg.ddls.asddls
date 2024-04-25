@EndUserText.label: 'Custom Entity - Formulário de Amostragem'
@ObjectModel.query.implementedBy: 'ABAP:ZCLTM_CE_FORM_AMSTRG'
define custom entity ZI_TM_CE_FORML_AMSTRG
  with parameters
    p_ordemFrete : char10
{
  key stream_data : ze_stream_carregamento;
      no_envelope : char1;
}
