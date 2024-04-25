"Name: \PR:SAPLJ1BR\EX:DETERMINE_ICMS_PARAMETERS_03\EN:OIH_SAPLJ1BR\SE:END\EI
ENHANCEMENT 0 ZEIMM_EXCECAO_1BTXIC3.

DATA lt_1btxic3 TYPE zctgtm_j_1btxic3.

lt_1btxic3[] = tj_1btxic3[].

NEW zclmm_enhancements( )->m_oil_cte_exc_dinamica_icms( EXPORTING iv_freight = i_freight
                                                                  iv_caller = i_caller
                                                                  is_global_komp = global_komp
                                                                  is_global_komk = global_komk
                                                                  is_global_007a = global_007a
                                                                  it_j_1btxic1 = j_1btxic1
                                                                  it_j_1btxic3 = lt_1btxic3
                                                                  CHANGING cv_rate = e_rate
                                                                  cs_nf_laws = nf_laws ).

ENDENHANCEMENT.
