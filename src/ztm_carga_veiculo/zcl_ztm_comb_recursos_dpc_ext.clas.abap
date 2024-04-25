CLASS zcl_ztm_comb_recursos_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_ztm_comb_recursos_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_stream
        REDEFINITION .

    CONSTANTS: gc_msgtype_e TYPE bapiret2-type VALUE 'E' ##NO_TEXT,
               gc_msgtype_s TYPE bapiret2-type VALUE 'S' ##NO_TEXT,
               gc_id        TYPE bapiret2-id VALUE 'ZFI_CUST_CHEQUE' ##NO_TEXT,
               gc_number    TYPE bapiret2-number VALUE '005' ##NO_TEXT,
               gc_split     TYPE string VALUE ';' ##NO_TEXT.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ztm_comb_recursos_dpc_ext IMPLEMENTATION.
  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.

    TYPES: BEGIN OF ty_entity,
             filename TYPE string,
             message  TYPE string,
           END OF ty_entity.

    DATA: lo_message   TYPE REF TO /iwbep/if_message_container,
          lo_exception TYPE REF TO /iwbep/cx_mgw_busi_exception,
          ls_entity    TYPE ty_entity,
          lv_filename  TYPE string,
          lv_tablename TYPE tablename,
          lt_return    TYPE bapiret2_t,
          lv_mime_type TYPE char100 VALUE 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.

    DATA(lo_comb_recur) = NEW zcltm_comb_recur(  ).

    SPLIT iv_slug AT gc_split INTO lv_filename lv_tablename.

    IF is_media_resource-mime_type <> lv_mime_type.
      ##NO_TEXT
      lt_return[] = VALUE #( BASE lt_return ( type = gc_msgtype_e id = gc_id number = gc_number ) ).
    ELSE.
* ----------------------------------------------------------------------
* Gerencia Botão do aplicativo
* ----------------------------------------------------------------------
      lo_comb_recur->upload_file( EXPORTING iv_file      = is_media_resource-value
                                           iv_filename  = lv_filename
                                 IMPORTING et_return    = lt_return ).

      TRY.
          ls_entity-filename = lv_filename.
          ls_entity-message  = lt_return[ 1 ]-message.
        CATCH cx_root.
      ENDTRY.

    ENDIF.

* ----------------------------------------------------------------------
* Prepara informações de retorno
* ----------------------------------------------------------------------
    copy_data_to_ref( EXPORTING is_data = ls_entity
                      CHANGING  cr_data = er_entity ).

* ----------------------------------------------------------------------
* Ativa exceção em casos de erro
* ----------------------------------------------------------------------
    IF NOT line_exists( lt_return[ type = gc_msgtype_s ] ).       "#EC CI_STDSEQ
      lo_message = mo_context->get_message_container( ).
      lo_message->add_messages_from_bapi( it_bapi_messages = lt_return ).
      CREATE OBJECT lo_exception EXPORTING message_container = lo_message.
      RAISE EXCEPTION lo_exception.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
