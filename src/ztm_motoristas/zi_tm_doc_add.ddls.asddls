@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Doc Adicionais'

define view entity ZI_TM_DOC_ADD

  as select from ZI_TM_DOC_ADD_UNION as _filho

  association to parent ZI_TM_MOTORISTAS as _pai on $projection.id = _pai.id

{
  key _filho.id,
  key _filho.paiid,
  key _filho.Tipo, 
  
      _filho.bp,
      _filho.Numero,
      _filho.NumeroCriticality,
      _filho.ValidadeDesde,
      _filho.Validade,
      _filho.Uf,
      _filho.Descricao,
      @Semantics.user.createdBy: true
      _filho.CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      _filho.CreatedAt,
      @Semantics.user.lastChangedBy: true
      _filho.LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      _filho.LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _filho.LocalLastChangedAt,

      -- Indica se a informação veio do Aplicativo (APP) ou Parceiro (BP)
      StatusReplicacao,

      _pai // Make association public
}
