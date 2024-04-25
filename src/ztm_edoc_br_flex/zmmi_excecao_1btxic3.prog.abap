*&---------------------------------------------------------------------*
*& Include          ZMMI_EXCECAO_1BTXIC3
*&---------------------------------------------------------------------*


    NEW zclmm_enhancements( )->m_oil_cte_exc_dinamica_icms( EXPORTING iv_freight = i_freight
    iv_caller = i_caller
    is_global_komp = global_komp
    is_global_komk = global_komk
    is_global_007a = global_007a
    it_j_1btxic1 = j_1btxic1
    it_j_1btxic3 = tj_1btxic3
    CHANGING cv_rate = e_rate
    ct_nf_laws = nf_laws )
