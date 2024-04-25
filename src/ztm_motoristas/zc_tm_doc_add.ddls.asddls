@EndUserText.label: 'CDS de Projeção - Doc Adicionais'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_TM_DOC_ADD
  as projection on ZI_TM_DOC_ADD

  association [0..1] to ZI_TM_VH_TIPO as _Tipo on _Tipo.type = $projection.Tipo
  association [0..1] to ZI_TM_VH_UF   as _Uf   on _Uf.Region = $projection.Uf

{
      @EndUserText.label: 'ID'
  key id,
  key paiid,
      @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_TIPO', element: 'type' } }]
      @EndUserText.label: 'Tipo'
  key Tipo,
      @EndUserText.label: 'Nº Documento'
      Numero,
      @EndUserText.label: 'Nº Documento (cor)'
      NumeroCriticality,
      @EndUserText.label: 'Válido Desde'
      ValidadeDesde,
      @EndUserText.label: 'Válido Até'
      Validade,
      @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_UF', element: 'Region' } }]
      @EndUserText.label: 'UF'
      Uf,
      @EndUserText.label: 'Descrição'
      Descricao,

      @EndUserText.label: 'Criado por'
      CreatedBy,
      @EndUserText.label: 'Criado em'
      CreatedAt,
      @EndUserText.label: 'Atualizado por'
      LastChangedBy,
      @EndUserText.label: 'Atualizado em'
      LastChangedAt,
      @EndUserText.label: 'Registrado em'
      LocalLastChangedAt,

      /* Associations */
      _pai : redirected to parent ZC_TM_MOTORISTAS,
      _Tipo,
      _Uf
}
