class ZCLTM_POST_CANCEL definition
  public
  inheriting from /BOBF/CL_LIB_A_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_ACTION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCLTM_POST_CANCEL IMPLEMENTATION.


  method /BOBF/IF_FRW_ACTION~EXECUTE.
    TRY.
        DATA(lt_return) = NEW zcltm_sispetro_send_data( )->cancel_oc( it_key = it_key ).
        IF lt_return IS NOT INITIAL.
          " Classe para converte objetos de msg para tabela de msg
          /scmtms/cl_common_helper=>msg_convert_bapiret2_2_bopf(
            EXPORTING
              iv_node_key   = is_ctx-node_key
              it_key        = it_key
              it_return     = lt_return
            CHANGING
              co_message    = eo_message
              ct_failed_key = et_failed_key
          ).
        ENDIF.
      CATCH cx_root.
        RETURN.
    ENDTRY.
  endmethod.
ENDCLASS.
