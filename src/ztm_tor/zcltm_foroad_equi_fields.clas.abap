class ZCLTM_FOROAD_EQUI_FIELDS definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLTM_FOROAD_EQUI_FIELDS IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    TRY.
        DATA(lo_event) = zcltm_cockpit_frete_event=>get_instance( ).

        lo_event->execute_update_fo_equi_info( EXPORTING is_ctx        = is_ctx
                                                         it_key        = it_key
                                                         io_read       = io_read
                                                         io_modify     = io_modify
                                               IMPORTING eo_message    = eo_message
                                                         et_failed_key = et_failed_key ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
