@EndUserText.label: 'Popup - Campos dados adicionais'

define abstract entity ZI_TM_POPUP_DOC_ADD
{
      @UI.lineItem  :        [{ position: 10 }]
      @UI.identification:    [{ position: 10 }]

      @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_TIPO', element: 'type' } }]

      @EndUserText.label: 'Tipo'
  key Tipo          : bu_id_type;

      @UI.lineItem  :        [{ position: 20 }]
      @UI.identification:    [{ position: 20 }]

      @EndUserText.label: 'Nº Documento'
  key Numero        : bu_id_number;

      @UI.lineItem  :        [{ position: 30 }]
      @UI.identification:    [{ position: 30 }]

      @EndUserText.label: 'Válido Desde'
  key ValidadeDesde : bu_id_valid_date_from;

      @UI.lineItem  :        [{ position: 40 }]
      @UI.identification:    [{ position: 40 }]

      @EndUserText.label: 'Válido Até'
  key Validade      : bu_id_valid_date_to;

      @UI.lineItem  :        [{ position: 50 }]
      @UI.identification:    [{ position: 50 }]

      @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_UF', element: 'Region' } }]

      @EndUserText.label: 'UF'
  key Uf            : regio;
      @UI.lineItem  :        [{ position: 60 }]
      @UI.identification:    [{ position: 60 }]

      @EndUserText.label: 'Descrição'
  key Descricao     : bu_id_institute;

}
