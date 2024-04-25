CLASS zcltm_tor_seal_fields DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcltm_tor_seal_fields IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    TRY.
        DATA(lo_event) = zcltm_cockpit_frete_event=>get_instance( ).

        lo_event->execute_update_fo_seal_info( EXPORTING is_ctx        = is_ctx
                                                         it_key        = it_key
                                                         io_read       = io_read
                                                         io_modify     = io_modify
                                               IMPORTING eo_message    = eo_message
                                                         et_failed_key = et_failed_key ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
