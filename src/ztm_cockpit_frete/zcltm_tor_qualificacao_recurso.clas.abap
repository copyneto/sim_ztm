CLASS zcltm_tor_qualificacao_recurso DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tmsresid,
        res_key TYPE /scmtms/s_tor_item_tr_k-res_key,
      END OF ty_tmsresid,
      tt_ty_tmsresid TYPE SORTED TABLE OF ty_tmsresid  WITH NON-UNIQUE KEY res_key.
ENDCLASS.



CLASS ZCLTM_TOR_QUALIFICACAO_RECURSO IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    DATA: lt_item_tr TYPE /scmtms/t_tor_item_tr_k.

    io_read->retrieve_by_association(
      EXPORTING
        iv_node        = is_ctx-node_key
        it_key         = it_key
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr
        iv_fill_data   = abap_true
      IMPORTING
        eo_message     = eo_message
        et_data        = lt_item_tr
        et_failed_key  = et_failed_key
    ).
    DATA(lo_message) = /bobf/cl_frw_factory=>get_message( ).
*
    DATA(lt_res_keys) = VALUE zcltm_tor_qualificacao_recurso=>tt_ty_tmsresid(
      FOR <fs_item_tr> IN lt_item_tr WHERE ( multi_item IS INITIAL AND res_key IS NOT INITIAL ) "#EC CI_SORTSEQ
      ( res_key = <fs_item_tr>-res_key )
    ).

    SELECT tmsresuid, valid_from, valid_to
    FROM /scmtms/cv_rscequalifnrqmt AS _Qual
    INNER JOIN @lt_res_keys AS _resKeys
    ON _resKeys~res_key = _Qual~tmsresuid
    INTO TABLE @DATA(lt_qualificacao) .

    DATA lv_currentTimeStamp TYPE timestamp.
    lv_currentTimeStamp = sy-datum && '000000'.
*    CONVERT DATE sy-datum TIME '000000' INTO TIME STAMP DATA(lv_currentTimeStamp) TIME ZONE sy-zonlo.

    LOOP AT lt_item_tr ASSIGNING FIELD-SYMBOL(<fs_item_tr_aux>).
      CHECK <fs_item_tr_aux>-multi_item IS INITIAL.
      IF NOT line_exists( lt_qualificacao[ tmsresuid = <fs_item_tr_aux>-res_key ] ). "#EC CI_STDSEQ
        MESSAGE w013(ztm_cockpit_frete) WITH <fs_item_tr_aux>-res_id INTO DATA(lv_dummy).
      ELSE.
        DATA(lv_nao_existe) = REDUCE abap_bool(
        INIT lv_erro = abap_false
        FOR ls_qualificacao IN lt_qualificacao
        WHERE ( tmsresuid = <fs_item_tr_aux>-res_key AND ( valid_from > lv_currentTimeStamp OR valid_to < lv_currentTimeStamp ) ) "#EC CI_STDSEQ
        NEXT lv_erro = abap_true
        ).
        IF lv_nao_existe = abap_true.
          MESSAGE e012(ztm_cockpit_frete) WITH <fs_item_tr_aux>-res_id INTO lv_dummy.
          DATA(lv_error) = abap_true.
        ENDIF.
      ENDIF.
      /scmtms/cl_common_helper=>msg_helper_add_symsg(
        EXPORTING
*         iv_attribute = /scmtms/if_tor_c=>sc_node_attribute-root-shipping_type
          iv_key      = it_key[ 1 ]-key
          iv_node_key = /scmtms/if_tor_c=>sc_node-root
        CHANGING
          co_message  = lo_message ).
      eo_message = lo_message.
    ENDLOOP.
    et_failed_key = COND #( WHEN lv_error = abap_true THEN it_key ELSE et_failed_key ).

  ENDMETHOD.
ENDCLASS.
