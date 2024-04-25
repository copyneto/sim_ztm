CLASS lcl_zeitm_edoc_br_flex_cl DEFINITION DEFERRED.
CLASS cl_lj1bif02 DEFINITION LOCAL FRIENDS lcl_zeitm_edoc_br_flex_cl.
CLASS lcl_zeitm_edoc_br_flex_cl DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zeitm_edoc_br_flex_cl.   "#EC NEEDED
    DATA core_object TYPE REF TO cl_lj1bif02 .              "#EC NEEDED
 INTERFACES  IOW_ZEITM_EDOC_BR_FLEX_CL.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_lj1bif02 OPTIONAL.
ENDCLASS.
CLASS lcl_zeitm_edoc_br_flex_cl IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD iow_zeitm_edoc_br_flex_cl~nf_header_create.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods NF_HEADER_CREATE
*"  importing
*"    !IV_BRANCH type J_1BBRANC_
*"    !IV_REFERENCE_DOCNUM type LFBNR
*"    !IS_EKKO type EKKO
*"    !IS_J_1BAA type J_1BAA
*"    !IS_RBKPV type MRM_RBKPV
*"    !IT_EKBE type CL_LJ1BIF02=>EKBE_TTYPE
*"    !IT_X4_RSEG type CL_LJ1BIF02=>MMCR_DRSEG_TTYPE
*"    !IV_IN_XML type C
*"    !IV_SHIPPING_POINT type VSTEL
*"    !IV_COMPARE_FLAG type ABAP_BOOL
*"  exporting
*"    !EV_CONSI_FLAG type CHAR1
*"    !EV_NF_NUMBER type XBLNR1
*"    !EV_REF_GJAHR type GJAHR
*"    !EV_DOCREF type J_1BDOCREF
*"    !EV_REF_BELNR type BELNR_D
*"    !ET_WT type CL_LJ1BIF02=>WT_TTYPE
*"    !EV_SCH_MACO type C
*"    !ET_CONSITAB type CL_LJ1BIF02=>CONSITAB_TTYPE
*"    !EV_CONSI_LINES type SYTFILL
*"    !EV_ZW_DOCREF type J_1BDOCREF
*"    !EV_ZW_LIF type ELIFN
*"    !ES_NFHEADER type J_1BNFDOC
*"    !ES_C_NFHEADER type J_1BNFDOC
*"  changing
*"    !CV_Z_MC type SY-TABIX
*"    !CV_STORNO_FLAG type C
*"    !CV_REOBJN type J_1BDOCNUM
*"    !CV_POPUP_DOCREF type J_1BDOCREF
*"    !CS_ACTIVE type J_1BNFE_ACTIVE optional
*"    !CV_REFERENCE_DOCREF type XBLNR1
*"    !CT_CTE_DOCREF type CL_LJ1BIF02=>CTE_DOCREF_TTYPE
*"    !CT_NF_TRANS_VOLUMES type J_1BNFTRANSVOL_TAB
*"    !CT_NF_TRAILER_INFO type J_1BNFTRAILER_TAB
*"    !CT_NF_TRADE_NOTES type J_1BNFTRADENOTES_TAB
*"    !CT_NF_ADD_INFO type J_1BNFADD_INFO_TAB
*"    !CT_NF_REF_PROC type J_1BNFREFPROC_TAB
*"    !CT_NF_SUGAR_SUPPLY type J_1BNFSUGARSUPPL_TAB
*"    !CT_NF_SUGAR_TAX_CONTRIB type J_1BNFSUGARDEDUC_TAB .
*"------------------------------------------------------------------------*
    DATA: lo_lj1bif02_da    TYPE REF TO cl_lj1bif02_da,
          lo_external_calls TYPE REF TO cl_lj1bif02_ext_calls.

    DATA: ls_access_key TYPE j_1b_nfe_access_key,
          ls_active     TYPE j_1bnfe_active.

    DATA: lv_company_code TYPE bukrs.

    REFRESH: et_wt,                                         "2376421
             et_consitab.                                   "2376421
    CLEAR: es_nfheader,                                     "2376421
           es_c_nfheader,                                   "2376421
           cv_reobjn,                                       "2376421
           et_wt,                                           "2376421
           ev_ref_belnr,                                    "2376421
           ev_sch_maco,                                     "2376421
           ev_consi_flag,                                   "2376421
           ev_zw_docref,                                    "2376421
           ev_ref_gjahr.                                    "2376421

    IF iv_compare_flag = abap_false.
      CLEAR: ct_nf_trans_volumes,
             ct_nf_trailer_info,
             ct_nf_trade_notes,
             ct_nf_add_info,
             ct_nf_ref_proc,
             ct_nf_sugar_supply,
             ct_nf_sugar_tax_contrib.
    ENDIF.

    lo_lj1bif02_da = cl_lj1bif02_da=>get_instance( ).
    lo_external_calls = cl_lj1bif02_ext_calls=>get_instance( ).

* Shipping point
    es_nfheader-vstel = iv_shipping_point.

    lv_company_code = is_rbkpv-bukrs.

* Get currency from company code
    es_nfheader-waerk = lo_lj1bif02_da->get_company_code_currency( lv_company_code ).

* Fill company code and branch info.
    es_nfheader-bukrs = lv_company_code.
    es_nfheader-branch = iv_branch.

* posting date and document date from invoice.
    es_nfheader-pstdat = is_rbkpv-budat.
    es_nfheader-docdat = is_rbkpv-bldat.
    es_nfheader-gjahr = is_rbkpv-gjahr.

* Fill incoterms from PO.
    es_nfheader-inco1 = is_ekko-inco1.
    es_nfheader-inco2 = is_ekko-inco2.

* Get payment terms and baseline due date.
    es_nfheader-zterm = is_rbkpv-zterm.
    es_nfheader-zfbdt = is_rbkpv-zfbdt.

* Fill document type, direction, flag for entrada, model, form,
* nota fiscal type, flag for NF-e and its number range.
    MOVE-CORRESPONDING is_j_1baa TO es_nfheader.

* incoming NF-e are always authorized
    IF  is_j_1baa-direct = core_object->mc_incoming_nf
    AND is_j_1baa-form IS INITIAL.
      es_nfheader-docstat = core_object->mc_nfe_authorized.
    ENDIF.

* Get intended NF number from invoice
    IF is_rbkpv-xblnr IS NOT INITIAL.
      ev_nf_number = is_rbkpv-xblnr.
    ENDIF.

* In a return of incoming NF-e, or NF-e of "Entrada",
* NF number should not come from invoice reference.
    IF is_j_1baa-form IS NOT INITIAL AND
       is_j_1baa-nfe = abap_true AND
      ( is_j_1baa-doctyp = core_object->mc_return_nf OR
        is_j_1baa-entrad = abap_true ).
      CLEAR ev_nf_number.
    ENDIF.

* Validate intended NF/NF-e number
    IF NOT ev_nf_number IS INITIAL.
      lo_external_calls->nf_num_separate( EXPORTING
                                            iv_ref_number  = ev_nf_number
                                            ix_nfeflag     = es_nfheader-nfe
                                            iv_nf_model    = es_nfheader-model
                                          IMPORTING
                                            ev_nf_number   = es_nfheader-nfnum
                                            ev_series      = es_nfheader-series
                                            ev_subseries   = es_nfheader-subser
                                            ev_ref_number  = ev_nf_number
                                            ev_nf_number9  = es_nfheader-nfenum
                                            ev_nf_number_utilities = es_nfheader-nfnum_utilities ). "2383340
    ENDIF.

* Read NF-e incoming XML if needed
    IF iv_in_xml = abap_true.
      core_object->map_incoming_xml(
        CHANGING
          cs_nfheader = es_nfheader
          ct_cte_docref  = ct_cte_docref  ).
    ENDIF.

*---- by cancel reference and GR, checks are not done ------------------*
    CHECK cv_storno_flag IS INITIAL.

* If reference document is filled, it comes from goods movement.
    IF iv_reference_docnum IS NOT INITIAL.
      lo_external_calls->gr_based_iv(
        EXPORTING
          iv_lfbnr        = iv_reference_docnum
          is_j_1baa       = is_j_1baa
          iv_save_xblnr   = ev_nf_number
          iv_company_code = lv_company_code
          iv_vendor       = is_rbkpv-lifnr
          iv_po_vendor    = is_ekko-lifnr
          iv_branch_vendor = is_rbkpv-filkd
          it_ekbe         = it_ekbe
        CHANGING
          cv_zw_docref    = ev_zw_docref
          cv_zw_lif       = ev_zw_lif
          cv_ref_belnr    = ev_ref_belnr
          cv_year         = ev_ref_gjahr
          cv_grgi_xblnr   = cv_reference_docref
          cv_sch_maco     = ev_sch_maco
          cs_nfheader     = es_nfheader
          cv_z_mc         = cv_z_mc
          cv_popup_docref = cv_popup_docref
          cv_reobjn       = cv_reobjn ).

    ELSE. "Not GR Based IV / future delivery.
      lo_external_calls->ref_not_grgi(
        EXPORTING
          iv_save_xblnr  = ev_nf_number
          is_rbkpv       = is_rbkpv
          is_j_1baa      = is_j_1baa
          iv_po_vendor   = is_ekko-lifnr
          iv_po_lifre    = is_ekko-lifre
          iv_ref_lifnr   = is_rbkpv-lifnr
          it_x4_rseg     = it_x4_rseg
          it_ekbe        = it_ekbe
        CHANGING
          cs_nfheader    = es_nfheader
          ct_consitab    = et_consitab
          cv_consi_lines = ev_consi_lines ).
    ENDIF.

* If NF-e, a global variable must be set as well
    IF es_nfheader-nfe IS NOT INITIAL.

*   Fill ls_active structure
      lo_external_calls->nf_fill_monitor_table(
        EXPORTING
          is_doc          = es_nfheader
          iv_docnum       = es_nfheader-docnum
        IMPORTING
          es_active       = ls_active ).

*   Export ls_active to global wk_active
      lo_external_calls->nfe_data_transfer( ls_active ).
    ENDIF.

**** Lógica de Ampliação
*Import Memory - ZFMTM_ALT_PROC_LOGISTICA
    DATA: ls_export     TYPE zstm_export_flex.
    DATA: ls_export2    TYPE zstm_export_flex.
*    DATA: lv_id(20)     TYPE c.
    DATA: lv_id     TYPE indx_srtfd.

    DATA(lv_mandt) = sy-mandt.

    DATA(lv_number) = ev_nf_number.

    SHIFT lv_number LEFT DELETING LEADING '0'.

    lv_id = |{ lv_company_code }| & |{ lv_number }|.

    IMPORT  ls_export TO ls_export2 FROM DATABASE indx(zz) CLIENT lv_mandt ID lv_id.


*    DELETE FROM DATABASE indx(zz) ID lv_id.


    IF ls_export2 IS NOT INITIAL.
      DELETE FROM DATABASE indx(zz) CLIENT sy-mandt ID lv_id.
      es_nfheader-authcod   = ls_export2-authcod.
      es_nfheader-authdate  = ls_export2-authdate.
      es_nfheader-authtime  = ls_export2-authtime.
      es_nfheader-hemi      = ls_export2-hemi.
      es_nfheader-inco1     = 'CIF'.
      es_nfheader-inco2     = 'CIF'.
      es_nfheader-xmlvers   = ls_export2-xmlvers.

      lo_external_calls->nf_fill_monitor_table(

      EXPORTING

      is_doc = es_nfheader

      iv_docnum = es_nfheader-docnum

      IMPORTING

      es_active = ls_active ).

      ls_active-docnum9 = es_nfheader-authcod(9).
      ls_active-tpemis  = es_nfheader-authcod(1).
      ls_active-cdv     = es_nfheader-authcod+9(1).

      lo_external_calls->nfe_data_transfer( ls_active ).

    ENDIF.
**** Lógica de Ampliação

  ENDMETHOD.
ENDCLASS.
