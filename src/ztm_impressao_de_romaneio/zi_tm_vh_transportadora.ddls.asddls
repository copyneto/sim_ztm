@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help - Transportadora'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

///*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
//define view entity ZI_TM_VH_TRANSPORTADORA as select from I_BusinessPartner 
//    association [1..1] to I_BusinessPartnerToBPRole as _BPRole on _BPRole.BusinessPartner = $projection.Transportadora
//                                                 
//{
//    key BusinessPartner as Transportadora,
//    BusinessPartnerName as TransportadoraDesc
//  
//}
//where BusinessPartnerCategory = '2'
//  and _BPRole.BusinessPartnerRole = 'CRM010'
//  or _BPRole.BusinessPartnerRole = 'WTRA01'
//
//group by BusinessPartner,
//    BusinessPartnerName
//
//    
//    
    

define view entity ZI_TM_VH_TRANSPORTADORA as select from vbpa
    association [1..1] to lfa1 as _Lfa1 on _Lfa1.lifnr = $projection.Transportadora
{ 
   @EndUserText.label: 'Transportadora'
   key lifnr as Transportadora,
   @EndUserText.label: 'Nome Transportadora'
    _Lfa1.name1 as TransportadoraDesc

}
where parvw = 'SP'
group by lifnr, _Lfa1.name1
    
    
   
