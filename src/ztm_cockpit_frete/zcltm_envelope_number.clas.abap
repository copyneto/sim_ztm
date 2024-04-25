CLASS zcltm_envelope_number DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_item_env,
             db_key             TYPE /bobf/conf_key,
             parent_key         TYPE /bobf/conf_key,
             zz_sample_envelope TYPE ze_tm_sample_envelope,
           END OF ty_item_env,

           ty_t_item_env TYPE TABLE OF ty_item_env WITH EMPTY KEY.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
protected section.
private section.

  methods BUILD_MESSAGE
    importing
      !IT_RETURN type BAPIRET2_T
      !IV_NODE type /BOBF/OBM_NODE_KEY
      !IV_KEY type /BOBF/CONF_KEY
      !IV_ATTRIBUTE type STRING
    changing
      !CO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !CT_FAILED_KEY type /BOBF/T_FRW_KEY .
ENDCLASS.



CLASS ZCLTM_ENVELOPE_NUMBER IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    " Tabelas internas
    DATA: lt_tor_item     TYPE /scmtms/t_tor_item_tr_k,
          lt_tor_item_env TYPE ty_t_item_env,
          lt_tor_root     TYPE /scmtms/t_tor_root_k,
          lt_return       TYPE bapiret2_t.

    DATA: lr_envelope TYPE RANGE OF ze_tm_sample_envelope,
          lr_key      TYPE RANGE OF /bobf/conf_key.

    io_read->retrieve( EXPORTING iv_node = /scmtms/if_tor_c=>sc_node-item_tr
                                 it_key  = it_key
                       IMPORTING et_data = lt_tor_item ).

    CHECK lt_tor_item IS NOT INITIAL.

    "Selection Parameters
    lr_envelope = VALUE #( FOR <fs_item> IN lt_tor_item
                           WHERE
                         ( zz_sample_envelope IS NOT INITIAL )
                         ( sign           = /bobf/if_conf_c=>sc_sign_option_including
                           option         = /bobf/if_conf_c=>sc_sign_equal
                           low            = <fs_item>-zz_sample_envelope ) ). "#EC CI_SORTSEQ

    lr_key = VALUE #( FOR <fs_item> IN lt_tor_item
                    ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                      option = /bobf/if_conf_c=>sc_sign_equal
                      low    = <fs_item>-key ) ).       "#EC CI_SORTSEQ

    CHECK lr_envelope IS NOT INITIAL.

    SELECT db_key,
           parent_key,
           zz_sample_envelope
      FROM /scmtms/d_torite
      WHERE zz_sample_envelope IN @lr_envelope
        AND db_key         NOT IN @lr_key
      INTO TABLE @lt_tor_item_env.                      "#EC CI_NOFIELD

    CHECK lt_tor_item_env IS NOT INITIAL.

    io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-item_tr                           " Node Name
                                                it_key         = CORRESPONDING #( lt_tor_item_env MAPPING key = db_key )     " Key Table
                                                iv_association = /scmtms/if_tor_c=>sc_association-item_tr-to_root            " Name of Association
                                                iv_fill_data   = abap_true
                                      IMPORTING et_data        = lt_tor_root ).

    CHECK lt_tor_root IS NOT INITIAL.

    IF eo_message IS INITIAL.
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

    LOOP AT lt_tor_item_env ASSIGNING FIELD-SYMBOL(<fs_tor_item>).
      IF line_exists( lt_tor_root[ key = <fs_tor_item>-parent_key ] ).
        IF lt_tor_root[ key = <fs_tor_item>-parent_key ]-lifecycle NE '10'.
          " Envelope &1 utilizado na ordem de frete &2.
          DATA(ls_tor_root) = lt_tor_root[ key = <fs_tor_item>-parent_key ].

          eo_message->add_message( is_msg       = VALUE #( msgty = if_xo_const_message=>error
                                                           msgid = 'ZTM_COCKPIT_FRETE'
                                                           msgno = '014'
                                                           msgv1 = <fs_tor_item>-zz_sample_envelope
                                                           msgv2 = |{ ls_tor_root-tor_id ALPHA = OUT }| )
                                   iv_node      = /scmtms/if_tor_c=>sc_node-item_tr
                                   iv_key       = <fs_tor_item>-db_key
                                   iv_attribute = 'ZZ_SAMPLE_ENVELOPE' ).

          APPEND INITIAL LINE TO et_failed_key ASSIGNING FIELD-SYMBOL(<fs_failed_key>).
          <fs_failed_key>-key = <fs_tor_item>-db_key.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_message.

    CHECK it_return IS NOT INITIAL.

    LOOP AT it_return REFERENCE INTO DATA(ls_return).

      co_message->add_message( is_msg       = VALUE #( msgty = ls_return->type
                                                       msgid = ls_return->id
                                                       msgno = ls_return->number
                                                       msgv1 = ls_return->message_v1
                                                       msgv2 = ls_return->message_v2
                                                       msgv3 = ls_return->message_v3
                                                       msgv4 = ls_return->message_v4 )
                               iv_node      = iv_node
                               iv_key       = iv_key
                               iv_attribute = iv_attribute ).

    ENDLOOP.

    IF iv_key IS NOT INITIAL AND NOT line_exists( ct_failed_key[ KEY key_sort COMPONENTS key = iv_key ] ).
      ct_failed_key = VALUE #( BASE ct_failed_key ( key = iv_key ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
