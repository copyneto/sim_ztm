class ZTMCL_BADI_SFIR definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_WZRE_DET_CONTR_FIELDS_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZTMCL_BADI_SFIR IMPLEMENTATION.


  METHOD if_wzre_det_contr_fields_badi~determine_ab_control_fields.

    DATA:
      lt_sfir_itm_selp  TYPE /bobf/t_frw_query_selparam,
      ls_sfir_itm_selp  TYPE /bobf/s_frw_query_selparam,
      lr_srv_mgr_sfir   TYPE REF TO /bobf/if_tra_service_manager,
      lr_srv_mgr_tor    TYPE REF TO /bobf/if_tra_service_manager,
      lt_sfir_keys      TYPE /bobf/t_frw_key,
      lt_sfir_root      TYPE /scmtms/t_sfir_root_k.

    lr_srv_mgr_sfir   = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_suppfreightinvreq_c=>sc_bo_key ).

    CLEAR ls_sfir_itm_selp.
    ls_sfir_itm_selp-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_query_attribute-root-query_by_elements-sfir_id.
    ls_sfir_itm_selp-low = is_posting_item-ext_ref_doc_no.
    ls_sfir_itm_selp-sign = /bobf/if_conf_c=>sc_sign_option_including.
    ls_sfir_itm_selp-option =  /bobf/if_conf_c=>sc_sign_equal.
    APPEND ls_sfir_itm_selp TO lt_sfir_itm_selp.

    CALL METHOD lr_srv_mgr_sfir->query
      EXPORTING
        iv_query_key            = /scmtms/if_suppfreightinvreq_c=>sc_query-root-query_by_elements
        iv_fill_data            = abap_true
        it_selection_parameters = lt_sfir_itm_selp
      IMPORTING
        et_data                 = lt_sfir_root.

    CHECK lt_sfir_root IS NOT INITIAL.

    LOOP AT lt_sfir_root INTO DATA(ls_sfir_root).
      /scmtms/cl_common_helper=>check_insert_key( EXPORTING iv_key = ls_sfir_root-key CHANGING ct_key = lt_sfir_keys ).
    ENDLOOP.

    DATA(lo_sfir_post_helper) = NEW /scmtms/cl_sf_posting_helper( ).

    lo_sfir_post_helper->get_data_for_sfir_posting(
      EXPORTING
        it_sfir_keys                = lt_sfir_keys
        iv_use_charge_query         = abap_true "Use query to retrieve charges details
      IMPORTING
        et_tor_root_data            = DATA(lt_tor_root_data)
    ).


    CHECK lt_tor_root_data IS NOT INITIAL.

    DATA(lv_tor_type) = lt_tor_root_data[ 1 ]-tor_type.

    CHECK lv_tor_type = 'ZOF5'.

    LOOP AT ct_ab_control_fields ASSIGNING FIELD-SYMBOL(<fs_ctrl_fields>).

      <fs_ctrl_fields>-use_case = if_wzre_posting_const=>wftypv-incoming_invoice.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
