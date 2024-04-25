CLASS zcltm_det_centro_po_frete DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scmtms/if_sfir_posting .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLTM_DET_CENTRO_PO_FRETE IMPLEMENTATION.


  METHOD /scmtms/if_sfir_posting~modify_po_creation.

    DATA: lo_post_sfir      TYPE REF TO /scmtms/cl_post_sfir_for_accru.

    DATA: lt_tor_root_data TYPE /scmtms/t_tor_root_k,
          lt_chaves_tor    TYPE /bobf/t_frw_key,
          lt_data_item     TYPE /scmtms/t_tor_item_tr_k,
          lo_message       TYPE REF TO /bobf/if_frw_message.

    lo_post_sfir = /scmtms/cl_post_sfir_for_accru=>get_instance( ).
    lo_post_sfir->get_data( IMPORTING et_tor_root_data = lt_tor_root_data ).
    READ TABLE lt_tor_root_data INTO DATA(ls_root_data) INDEX 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    APPEND VALUE #( key = ls_root_data-key ) TO lt_chaves_tor.

    DATA(lo_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).
    TRY.
        lo_srv_mgr->retrieve_by_association(
          EXPORTING
            iv_node_key    = /scmtms/if_tor_c=>sc_node-root
            it_key         = lt_chaves_tor
            iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr
            iv_fill_data   = abap_true
          IMPORTING
            eo_message     = lo_message
            et_data        = lt_data_item ).

      CATCH /bobf/cx_frw_contrct_violation.
        RETURN.
    ENDTRY.

    LOOP AT lt_data_item ASSIGNING FIELD-SYMBOL(<fs_data_item>).
      CHECK <fs_data_item>-item_cat = 'PRD'.
      DATA(lv_plant_id) = <fs_data_item>-erp_plant_id.
      EXIT.
    ENDLOOP.

    IF lv_plant_id IS INITIAL.
      RETURN.
    ENDIF.

    SORT: ct_po_itemsx BY po_item.
    LOOP AT ct_po_items ASSIGNING FIELD-SYMBOL(<fs_po_item>).
      READ TABLE ct_po_itemsx ASSIGNING FIELD-SYMBOL(<fs_po_itemx>)
        WITH KEY po_item = <fs_po_item>-po_item BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      <fs_po_item>-plant  = lv_plant_id.
      <fs_po_itemx>-plant = abap_true.
    ENDLOOP.

**********************************************************************
* INSERT - 06.02/2024 - GAPTM01 - Determinação de IVA
    DATA(lo_det_iva) = NEW zcltm_modify_po_iva_det( ).
    lo_det_iva->det_iva( EXPORTING it_tor_root    = lt_tor_root_data  " Root Node
                                   it_item_tr     = lt_data_item      " Transportation Order Item
                         CHANGING  ct_po_items    = ct_po_items       " Tipo de tabela para BAPIMEPOITEM
                                   ct_po_itemsx   = ct_po_itemsx      " Tipo de tabela para BAPIMEPOITEMX
                                   ct_po_services = ct_po_services ). " Tipo de tabela para BAPIESLLC

**********************************************************************

  ENDMETHOD.
ENDCLASS.
