class ZTMCL_TCC_CALC_BASE definition
  public
  inheriting from /SCMTMS/CL_TCC_CB_LOCATION
  create public .

public section.

  methods /SCMTMS/IF_TCC_CALC_BASE~GET_CALC_BASE_VALUES
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZTMCL_TCC_CALC_BASE IMPLEMENTATION.


  METHOD /scmtms/if_tcc_calc_base~get_calc_base_values.
**TRY.
*CALL METHOD SUPER->/SCMTMS/IF_TCC_CALC_BASE~GET_CALC_BASE_VALUES
*  EXPORTING
*    IT_REQUESTS         =
*    IT_CALC_BASE_KEYS   =
*    IO_BUS_DATA_ACCESS  =
*    IO_ANALYSIS_LOG     =
*  CHANGING
*    CT_CALC_BASE_VALUES =
*    .
**ENDTRY.

    TYPES: BEGIN OF ty_original_keys,
             index TYPE sy-tabix,
             key   TYPE /bobf/conf_key,
           END OF  ty_original_keys.

    TYPES: BEGIN OF ty_locations,
             loctype    TYPE /sapapo/loc-loctype,
             locno      TYPE /sapapo/loc-locno,
             loc_uuid   TYPE /sapapo/loc-loc_uuid,
             adrnummer  TYPE /sapapo/loc-adrnummer,
             taxjurcode TYPE adrc-taxjurcode,
             distance   TYPE /sapapo/tr_dist,
           END OF ty_locations.

    DATA lt_locations TYPE TABLE OF ty_locations.
    FIELD-SYMBOLS <fs_locations> TYPE ty_locations.

    DATA lt_original_keys TYPE TABLE OF ty_original_keys.
    DATA ls_original_keys TYPE ty_original_keys.

    DATA l_index_from TYPE i.
    DATA l_index      TYPE i.

    DATA lt_calc_base_keys_change TYPE /scmtms/if_tcc_bus_data_acc=>tt_calc_base_key.
    FIELD-SYMBOLS <fs_calc_base_keys_change> LIKE LINE OF lt_calc_base_keys_change.

    DATA lt_key TYPE /bobf/t_frw_key.
    DATA ls_key TYPE /bobf/s_frw_key.

    DATA lt_stop TYPE /scmtms/t_tor_stop_k.

    DATA: ref TYPE /bobf/s_frw_key-key.

    DATA: lt_stop2 TYPE /scmtms/s_tor_stop.

    DATA lt_root    TYPE /scmtms/t_tor_root_k.
    DATA lt_root_fu TYPE /scmtms/t_tor_root_k.

    DATA: lt_all_items  TYPE /scmtms/t_tor_item_tr_k,
          lt_items_pal  TYPE /scmtms/t_tor_item_tr_k,
          lt_main_items TYPE /scmtms/t_tor_item_tr_k.

    DATA: lt_items     TYPE /scmtms/t_tor_item_tr_k.
    DATA: lt_item_tr   TYPE /scmtms/t_tor_item_tr_k.
    DATA: lt_item_st   TYPE /scmtms/t_tor_item_tr_k.

    DATA: lt_items_un  TYPE /scmtms/t_tor_item_tr_k.
    DATA: lt_items_lo  TYPE /scmtms/t_tor_item_tr_k.

    DATA: lt_d_loc_root     TYPE  /scmtms/t_bo_loc_root_k,

          lt_item           TYPE /scmtms/t_trq_item_k,
          lo_message        TYPE REF TO /bobf/if_frw_message,

          lt_message        TYPE bapirettab,

          lx_tc_calc_engine TYPE REF TO /scmtms/cx_tc_calc_engine,


          l_valor_conv      TYPE /scmtms/amount.

    DATA: l_amt_gdsv_val     TYPE /scmtms/amt_goodsvalue_val.
    DATA: l_base_uom_val     TYPE /scmtms/qua_base_uom_val.

    DATA:
      lt_calc_base_keys TYPE /scmtms/if_tcc_bus_data_acc=>tt_calc_base_key.

    DATA:
      lt_location_key  TYPE /bobf/t_frw_key.

    FIELD-SYMBOLS:
      <ls_location_key>    TYPE /bobf/s_frw_key.

    DATA: l_cur_in TYPE currencyiso,
          l_cur    TYPE currencyiso.

    IF NOT it_requests[] IS INITIAL.
      ls_key-key = it_requests[ 1 ]-root_key.
      APPEND ls_key TO lt_key.
    ENDIF.

    CALL METHOD /scmtms/cl_tor_helper_read=>get_tor_data
      EXPORTING
        it_root_key   = lt_key
      IMPORTING
        et_root       = lt_root
        et_stop       = lt_stop
        et_all_items  = lt_all_items
        et_main_items = lt_main_items.

    IF NOT lt_main_items[] IS INITIAL.

      l_cur = lt_main_items[ 1 ]-amt_gdsv_cur.

      DELETE lt_all_items WHERE item_cat <> 'PRD'.

    ENDIF.

    DATA(ls_requests) = it_requests[ 1 ].

    DATA(lv_tor_type) = lt_root[ 1 ]-tor_type.

    LOOP AT it_calc_base_keys ASSIGNING FIELD-SYMBOL(<ls_calc_base_keys>).

      CHECK <ls_calc_base_keys>-calc_base_name = /scmtms/cl_tcc_dao_bo=>gc_calc_base-sourceloc.

      READ TABLE ct_calc_base_values ASSIGNING FIELD-SYMBOL(<value>)
        WITH KEY calc_base_key-calc_base_name = /scmtms/cl_tcc_dao_bo=>gc_calc_base-sourceloc.

      CHECK sy-subrc IS INITIAL.

      INSERT <ls_calc_base_keys> INTO TABLE lt_calc_base_keys.

      READ TABLE lt_stop INTO DATA(ls_stop) WITH KEY stop_seq_pos = 'F'.
      CHECK sy-subrc IS INITIAL.

      <value>-qty_value_raw = ls_stop-log_loc_uuid.

      CHECK <value>-qty_value_raw IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_location_key ASSIGNING <ls_location_key>.
      <ls_location_key>-key = <value>-qty_value_raw.


      IF lt_location_key IS INITIAL.
        RETURN.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM lt_location_key USING KEY key_sort.
      read_location(
        EXPORTING
          it_key  = lt_location_key
        IMPORTING
          et_loc  = DATA(lt_location_data)
      ).

      READ TABLE lt_location_data ASSIGNING FIELD-SYMBOL(<ls_location_data>)
        WITH TABLE KEY locid = <value>-qty_value_raw.
      IF sy-subrc IS INITIAL.
        ASSIGN COMPONENT gc_data_field-city OF STRUCTURE <ls_location_data> TO FIELD-SYMBOL(<lv_data_field>).
        IF sy-subrc = 0.
          TRANSLATE <lv_data_field> TO UPPER CASE.
          <value>-qty_value_char = <lv_data_field>.
        ENDIF.
      ENDIF.


    ENDLOOP.

    CHECK lv_tor_type <> 'ZOF4' AND lv_tor_type <> 'ZOF5' AND lv_tor_type <> 'ZOF2'.

*    LOOP AT it_requests INTO DATA(ls_requests).

    LOOP AT ct_calc_base_values ASSIGNING <value>
      WHERE calc_base_key-calc_base_name = 'GOODS_VALUE'.

      LOOP AT ls_requests-rb_stage INTO DATA(ls_stage)
        WHERE key = <value>-calc_base_key-res_base_key-request_key-key.

        CLEAR l_amt_gdsv_val.

*        READ TABLE ls_requests-item INTO DATA(ls_item)
*          WITH KEY item_id = ls_stage-end_stop_id.

        READ TABLE ls_requests-item INTO DATA(ls_item)
          WITH KEY des_stop_key = ls_stage-end_stop_key.

        CHECK sy-subrc IS NOT INITIAL.

        LOOP AT lt_all_items INTO DATA(ls_item2)
          WHERE des_stop_key = ls_item-des_stop_key.

          l_valor_conv = ls_item2-amt_gdsv_val.

          l_cur_in = ls_item2-amt_gdsv_cur.

          IF l_cur <> l_cur_in.

            " Convert to same currency, then compare amounts
            TRY.

                /scmtms/cl_tc_util=>convert_currency(
                  EXPORTING
                    iv_amount   = l_valor_conv
                    iv_curr_in  = l_cur_in
                    iv_curr_out = l_cur
                    iv_date     = sy-datum
                  IMPORTING
                    ev_amount   = l_valor_conv
                  CHANGING
                    ct_message  = lt_message
                ).
              CATCH /scmtms/cx_tc_calc_engine INTO lx_tc_calc_engine. "#EC NO_HANDLER
                RAISE EXCEPTION TYPE /scmtms/cx_tccs_engine
                  EXPORTING
                    previous = lx_tc_calc_engine.

            ENDTRY.

          ENDIF.

          ls_item2-amt_gdsv_val = l_valor_conv.

          l_amt_gdsv_val = l_amt_gdsv_val + ls_item2-amt_gdsv_val.

        ENDLOOP.

        CHECK sy-subrc = 0.

*       AND calc_base_key-res_base_key-request_key-key = ls_stage-key.

        CLEAR <value>-qty_value_cur.
        <value>-qty_value_cur = l_amt_gdsv_val.
      ENDLOOP.

    ENDLOOP.

    DATA: l_qty_val_num     TYPE /scmtms/qua_gro_wei_val.

    LOOP AT ct_calc_base_values ASSIGNING <value>
      WHERE calc_base_key-calc_base_name = 'GROSS_WEIGHT'.

      LOOP AT ls_requests-rb_stage INTO ls_stage
        WHERE key = <value>-calc_base_key-res_base_key-request_key-key.

        CLEAR l_qty_val_num.

        READ TABLE ls_requests-item INTO ls_item
*          WITH KEY item_id = ls_stage-end_stop_id.
          WITH KEY des_stop_key = ls_stage-end_stop_key.

        CHECK sy-subrc = 0.

        LOOP AT lt_all_items INTO ls_item2
          WHERE des_stop_key = ls_item-des_stop_key.

          l_qty_val_num = l_qty_val_num + ls_item2-gro_wei_val.

        ENDLOOP.

        CHECK sy-subrc = 0.

        CLEAR <value>-qty_value_num.
        <value>-qty_value_num = l_qty_val_num.
      ENDLOOP.

    ENDLOOP.

    DATA: l_qty_vol_num     TYPE /scmtms/qua_gro_vol_val.

    LOOP AT ct_calc_base_values ASSIGNING <value>
      WHERE calc_base_key-calc_base_name = 'GROSS_VOLUME'.

      LOOP AT ls_requests-rb_stage INTO ls_stage
        WHERE key = <value>-calc_base_key-res_base_key-request_key-key.

        CLEAR l_qty_vol_num .

        READ TABLE ls_requests-item INTO ls_item
          WITH KEY des_stop_key = ls_stage-end_stop_key.

        CHECK sy-subrc IS INITIAL.

        LOOP AT lt_all_items INTO ls_item2
          WHERE des_stop_key = ls_item-des_stop_key.

          l_qty_vol_num  = l_qty_vol_num  + ls_item2-gro_vol_val.

        ENDLOOP.

        CHECK sy-subrc IS INITIAL.

        CLEAR <value>-qty_value_num.

        IF ls_item2-inc_class_code <> 'FOB' OR lv_tor_type = 'ZOF1'.
          <value>-qty_value_num = l_qty_vol_num / 1000 .
        ENDIF.
      ENDLOOP.

    ENDLOOP.
**********************************************************************
* get field from location data and return as calc base value
**********************************************************************
*  LOOP AT it_calc_base_keys ASSIGNING <ls_calc_base_key>.
*    READ TABLE ct_calc_base_values WITH KEY calc_base_key COMPONENTS calc_base_key = <ls_calc_base_key> ASSIGNING <ls_calc_base_value>.
*    IF sy-subrc NE 0.
*      APPEND VALUE #( calc_base_key = <ls_calc_base_key> ) TO ct_calc_base_values ASSIGNING <ls_calc_base_value>.
*    ENDIF.

*    READ TABLE lt_location ASSIGNING <ls_location> WITH KEY calc_base_key
*      COMPONENTS calc_base_key-res_base_key = <ls_calc_base_key>-res_base_key.
*    IF sy-subrc = 0.
*      READ TABLE lt_location_data ASSIGNING <ls_location_data>
*        WITH TABLE KEY locid = <ls_location>-qty_value_raw.
*      IF sy-subrc = 0.
*        ASSIGN COMPONENT iv_data_field OF STRUCTURE <ls_location_data> TO <lv_data_field>.
*        IF sy-subrc = 0.
*          TRANSLATE <lv_data_field> TO UPPER CASE.
*          <ls_calc_base_value>-qty_value_char = <lv_data_field>.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*      CALL METHOD me->get_location_data
*        EXPORTING
*          iv_location_calc_base = /scmtms/cl_tcc_dao_bo=>gc_calc_base-sourceloc
*          iv_data_field         = gc_data_field-city
*          it_calc_base_keys     = lt_calc_base_keys
*          io_bus_data_access    = io_bus_data_access
*          io_analysis_log       = io_analysis_log
*        CHANGING
*          ct_calc_base_values   = ct_calc_base_values.

*  ENDLOOP.


    lt_items_pal = lt_all_items.
    DELETE lt_items_pal WHERE product_id <> '000000000079990013'.

    LOOP AT ct_calc_base_values ASSIGNING <value>
      WHERE calc_base_key-calc_base_name = 'QUANTITY_VAL_2'.

      LOOP AT ls_requests-rb_stage INTO ls_stage
        WHERE key = <value>-calc_base_key-res_base_key-request_key-key.

        CLEAR l_base_uom_val.

        READ TABLE ls_requests-item INTO ls_item
          WITH KEY des_stop_key = ls_stage-end_stop_key.

        CHECK sy-subrc = 0.

        LOOP AT lt_items_pal INTO ls_item2
          WHERE des_stop_key = ls_item-des_stop_key.

          l_base_uom_val = l_base_uom_val + ls_item2-base_uom_val.

        ENDLOOP.

        CHECK sy-subrc = 0.

        CLEAR <value>-qty_value_cur.
        <value>-qty_value_num = l_base_uom_val.
      ENDLOOP.

    ENDLOOP.


*    ENDLOOP.


*    LOOP AT it_requests INTO DATA(ls_requests).
*
*      LOOP AT ls_requests-rb_stage INTO DATA(ls_stage).
*
*        CLEAR l_amt_gdsv_val.
*
*        READ TABLE ls_requests-item INTO DATA(ls_item)
*          WITH KEY item_id = ls_stage-end_stop_id.
*
*        CHECK sy-subrc = 0.
*
*        LOOP AT ls_requests-item INTO DATA(ls_item2)
*          WHERE des_stop_key = ls_item-des_stop_key.
*
*          l_amt_gdsv_val = l_amt_gdsv_val + ls_item2-amt_gdsv_val.
*
*        ENDLOOP.
*
*        CHECK sy-subrc = 0.
*
*        LOOP AT ct_calc_base_values ASSIGNING FIELD-SYMBOL(<value>)
*          WHERE calc_base_key-calc_base_name = 'GOODS_VALUE'
*            AND calc_base_key-res_base_key-request_key-key = ls_stage-key.
*
*          CLEAR <value>-qty_value_cur.
*          <value>-qty_value_cur = l_amt_gdsv_val.
*        ENDLOOP.
*
*      ENDLOOP.
*
*    ENDLOOP.

*    DATA(lt_items_aux) = lt_all_items.
*    DELETE ADJACENT DUPLICATES FROM lt_items_aux COMPARING fu_root_key.
*
*    DESCRIBE TABLE lt_items_aux LINES DATA(l_lines_item).
*
*    LOOP AT lt_items_aux INTO DATA(line) WHERE item_cat = 'PRD' .
*      ADD 1 TO ref.
*      LOOP AT lt_all_items ASSIGNING FIELD-SYMBOL(<line>) WHERE fu_root_key = line-fu_root_key.
*        <line>-fu_root_key = ref.
*      ENDLOOP.
*    ENDLOOP.
*
*    IF NOT it_calc_base_keys[ 1 ]-res_base_key-res_base_id = me->reference_key.
*      me->reference_key = it_calc_base_keys[ 1 ]-res_base_key-res_base_id.
*
*      ADD 1 TO me->reference.
*
*      IF me->reference > l_lines_item.
*        me->reference = 1.
*      ENDIF.
*
*    ENDIF.
*
*    LOOP AT ct_calc_base_values ASSIGNING FIELD-SYMBOL(<value>) WHERE calc_base_key-calc_base_name = 'GOODS_VALUE'.
*      CLEAR <value>-qty_value_cur.
*      LOOP AT lt_all_items INTO DATA(item) WHERE fu_root_key = me->reference.
*        <value>-qty_value_cur = <value>-qty_value_cur + item-amt_gdsv_val.
*      ENDLOOP.
*    ENDLOOP.

*   Service Manager para BO TOR
    DATA(lo_serv_manager) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).

** Retrieve item data
*    lo_serv_manager->retrieve_by_association(
*        EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-root
*        it_key         = lt_key
*        iv_association = /scmtms/if_tor_c=>sc_association-item_tr-fu_root
*        iv_fill_data   = abap_true
*        iv_edit_mode   = /bobf/if_conf_c=>sc_edit_read_only
*        IMPORTING
*        eo_message     = lo_message
*        et_data        = lt_root_fu ).

*    CLEAR: lt_key.
*    IF NOT ct_calc_base_values[] IS INITIAL.
*      ls_key-key = ct_calc_base_values[ 1 ]-calc_base_key-res_base_key-res_base_id.
*      APPEND ls_key TO lt_key.
*    ENDIF.

*    CHECK sy-uname = 'CONS_META33'.
*
*    CALL METHOD /scmtms/cl_tor_helper_read=>get_tor_data
*      EXPORTING
*        it_root_key   = lt_key
*      IMPORTING
*        et_root       = lt_root
*        et_stop       = lt_stop
*        et_all_items  = lt_all_items
*        et_main_items = lt_main_items.
*
*    CLEAR: lt_key.
*    LOOP AT lt_stop INTO ls_stop.
*      /scmtms/cl_common_helper=>check_insert_key( EXPORTING iv_key = ls_stop-key CHANGING ct_key = lt_key ).
*    ENDLOOP.
*
*    lo_serv_manager->retrieve_by_association(
*        EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-item_tr
*        it_key         = lt_key
*        iv_association = /scmtms/if_tor_c=>sc_association-root-assigned_fus
*        iv_fill_data   = abap_true
*        iv_edit_mode   = /bobf/if_conf_c=>sc_edit_read_only
*        IMPORTING
*        eo_message     = lo_message
*        et_data        = lt_items ).
*
** Retrieve item data
*    lo_serv_manager->retrieve_by_association(
*        EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-root
*        it_key         = lt_key
*        iv_association = /scmtms/if_tor_c=>sc_association-item_tr-fu_root
*        iv_fill_data   = abap_true
*        iv_edit_mode   = /bobf/if_conf_c=>sc_edit_read_only
*        IMPORTING
*        eo_message     = lo_message
*        et_data        = lt_root_fu ).
*
*
*    lo_serv_manager->retrieve_by_association(
*        EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-root
*        it_key         = lt_key
*        iv_association = /scmtms/if_tor_c=>sc_association-root-bo_trq_item_fu_items
*        iv_fill_data   = abap_true
*        iv_edit_mode   = /bobf/if_conf_c=>sc_edit_read_only
*        IMPORTING
*        eo_message     = lo_message
*        et_data        = lt_items ).
*
*    CALL METHOD /scmtms/cl_tor_helper_read=>get_tor_data
*      EXPORTING
*        it_root_key   = lt_key
*      IMPORTING
*        et_root       = lt_root
*        et_stop       = lt_stop
*        et_all_items  = lt_all_items
*        et_main_items = lt_main_items.
*
*    CLEAR: lt_key.
*    LOOP AT lt_stop INTO ls_stop.
*      /scmtms/cl_common_helper=>check_insert_key( EXPORTING iv_key = ls_stop-key CHANGING ct_key = lt_key ).
*    ENDLOOP.
*
** Retrieve item data
*    lo_serv_manager->retrieve_by_association(
*        EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-stop
*        it_key         = lt_key " passar aqui a chave do stop
*        iv_association = /scmtms/if_tor_c=>sc_association-stop-item_tr_unloaded_at_stop
*        iv_fill_data   = abap_true
*        iv_edit_mode   = /bobf/if_conf_c=>sc_edit_read_only
*        IMPORTING
*        eo_message     = lo_message
*        et_data        = lt_items_un ).
*
*    lo_serv_manager->retrieve_by_association(
*        EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-stop
*        it_key         = lt_key " passar aqui a chave do stop
*        iv_association = /scmtms/if_tor_c=>sc_association-stop-item_tr_loaded_at_stop
*        iv_fill_data   = abap_true
*        iv_edit_mode   = /bobf/if_conf_c=>sc_edit_read_only
*        IMPORTING
*        eo_message     = lo_message
*        et_data        = lt_items_lo ).
*
*  /scmtms/cl_tor_helper_quantity=>recalc_qty_and_capa(
*    EXPORTING
*      it_key_item_qty_recalc = lt_key
*      it_tor_root            = lt_root
*      iv_consider_exec_data  = space
**      it_exec_data           = it_exec_info
*    CHANGING
*      ct_item_tr             = lt_item_st ).
*
*    IF NOT it_requests[] IS INITIAL.
*      ls_key-key = it_requests[ 1 ]-root_key.
*      APPEND ls_key TO lt_key.
*    ENDIF.
*
*    CALL METHOD lo_serv_manager->retrieve_by_association(
*      EXPORTING
*        it_key          = lt_key
*        iv_node_key     = /scmtms/if_tor_c=>sc_node-root
*        iv_association  = /scmtms/if_tor_c=>sc_association-root-item_tr
*        iv_fill_data    = abap_true
**        iv_before_image = iv_before_image
*      IMPORTING
*        et_data         = lt_item_tr ).
*
*    CLEAR: lt_key.
*    LOOP AT lt_all_items INTO DATA(ls_all_item).
*      /scmtms/cl_common_helper=>check_insert_key( EXPORTING iv_key = ls_all_item-key CHANGING ct_key = lt_key ).
*    ENDLOOP.
*
**    CALL METHOD /scmtms/cl_tor_helper_read=>get_tor_item_data
**      EXPORTING
**        it_item_key = lt_key
***       io_read     =
***       io_data_prov    =
***       iv_before_image =
**      IMPORTING
**        et_item     = lt_items.
*
**
**    CALL METHOD /scmtms/cl_tor_helper_read=>get_tor_data
**      EXPORTING
**        it_root_key = lt_key
**      IMPORTING
**        et_root     = lt_root
**        et_stop     = lt_stop.
*
*    CHECK NOT lt_stop[] IS INITIAL.
*
*    DATA: lt_calc_base_key   TYPE io_bus_data_access->tt_calc_base_key,
*          ls_calc_base_key   LIKE LINE OF lt_calc_base_key,
*          lt_calc_base_value TYPE io_bus_data_access->tt_calc_base_value.
*
*
**    IF NOT IT_CALC_BASE_KEYS[] IS INITIAL.
**      ls_calc_base_key-res_base_key = IT_CALC_BASE_KEYS[ 1 ]-res_base_key.
**      ls_calc_base_key-res_base_key = IT_CALC_BASE_KEYS[ 1 ]-res_base_key.
**      APPEND ls_key TO lt_key.
**    ENDIF.
*
*    "Esse classe/metodo Z já chamado pelo metodo get_calc_base_values
**    CALL METHOD io_bus_data_access->get_calc_base_values
**      EXPORTING
**        it_calc_base_keys   = it_calc_base_keys
***       io_context          =
**      IMPORTING
**        et_calc_base_values = lt_calc_base_value.
*
*
*    "Read Freight Order Data
*    CALL METHOD /scmtms/cl_tor_helper_read=>get_loc_data
*      EXPORTING
*        it_root_key   = lt_key
**       iv_before_image = ABAP_FALSE
*        it_stop       = lt_stop
**       it_exec       =
**       it_item       =
**       io_read       =
**       io_data_prov  =
*      IMPORTING
*        et_d_loc_root = lt_d_loc_root.

*IT_EXEC  Importing Type  /SCMTMS/T_TOR_EXEC_K
*IT_ITEM  Importing Type  /SCMTMS/T_TOR_ITEM_TR_K
*IO_READ  Importing Type Ref To /BOBF/IF_FRW_READ
*IO_DATA_PROV	Importing	Type Ref To	/SCMTMS/IF_DATA_INTERFACE

*    LOOP AT lt_stop INTO ls_stop.
*
*      ls_stop_key-key = ls_stop-key.
*      APPEND ls_stop_key TO lt_stop_key.
*
*    ENDLOOP.


*    CALL METHOD /scmtms/cl_tor_helper_stop=>get_stop_data
*      EXPORTING
*        it_stop_key                   =
*        io_read                       =
**      IMPORTING
**        et_cap_stop_data              =
**        et_stop_data                  =
**        et_stop_data_bi               =
**        et_stop_succ_all_data         =
**        et_root_data                  =
**        et_req_stop_at_same_capa_stop =
**        et_req_stop_data              =
**        et_root_data_del              =
**        et_capa_root_data             =
*        .


*    get_loc_sequence
*    get_stage
*    get_stage_capa_info
*    get_stage_descr
*    get_stage_items
*    get_stops_for_root
*    get_stop_data
*    get_stop_description
*    get_stop_key_loc_mandatory
*    get_stop_key_loc_readonly
*    get_stop_sequence
*    get_stop_sequence_linear



*    CALL METHOD me->get_location_ids
*      EXPORTING
*        it_bp_stage     =
**        it_stage_cust   =
*        iv_by_stage_cat =
**      IMPORTING
**        et_location     =
**        ev_no_eval      =
**        et_eval_stg     =
*        .
*
*
*
*    CALL METHOD me->get_location
*      EXPORTING
**        iv_to_loc   = ABAP_FALSE
*        is_rb_input =
*      receiving
*        rv_location =
*        .


  ENDMETHOD.
ENDCLASS.
