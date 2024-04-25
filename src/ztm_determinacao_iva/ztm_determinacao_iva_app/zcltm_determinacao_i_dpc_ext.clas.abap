CLASS zcltm_determinacao_i_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcltm_determinacao_i_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_stream
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcltm_determinacao_i_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.

    TYPES: BEGIN OF ty_entity,
             filename TYPE string,
             message  TYPE string,
             severity TYPE string,
           END OF ty_entity.

    DATA: lo_message   TYPE REF TO /iwbep/if_message_container,
          lo_exception TYPE REF TO /iwbep/cx_mgw_busi_exception,
          ls_entity    TYPE ty_entity,
          lv_filename  TYPE string,
          lv_tablename TYPE tablename,
          lt_return    TYPE bapiret2_t,
          lv_mime_type TYPE char100 VALUE 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.

    DATA(lo_det_iva) = NEW zcltm_determinacao_iva( ).

    SPLIT iv_slug AT ';' INTO lv_filename lv_tablename.

    IF is_media_resource-mime_type <> lv_mime_type.
      ##NO_TEXT
      lt_return[] = VALUE #( BASE lt_return ( type = 'E' id = 'ZTM_DETERMINACAO_IVA' number = '005' ) ).
    ELSE.
* ----------------------------------------------------------------------
* Gerencia Botão do aplicativo
* ----------------------------------------------------------------------
      lo_det_iva->upload_file( EXPORTING iv_file      = is_media_resource-value
                                         iv_filename  = lv_filename
                               IMPORTING et_return    = lt_return ).

      TRY.
          ls_entity-filename = lv_filename.
          ls_entity-message  = lt_return[ 1 ]-message.
          ls_entity-severity = lt_return[ 1 ]-type.
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
    IF NOT line_exists( lt_return[ type = 'S' ] ).       "#EC CI_STDSEQ
      lo_message = mo_context->get_message_container( ).
      lo_message->add_messages_from_bapi( it_bapi_messages = lt_return ).
      CREATE OBJECT lo_exception EXPORTING message_container = lo_message.
      RAISE EXCEPTION lo_exception.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
