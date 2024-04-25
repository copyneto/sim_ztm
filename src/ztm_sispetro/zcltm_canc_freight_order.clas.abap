class ZCLTM_CANC_FREIGHT_ORDER definition
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



CLASS ZCLTM_CANC_FREIGHT_ORDER IMPLEMENTATION.


  METHOD /bobf/if_frw_action~execute.

    TRY.

        DATA(lt_return) = NEW zcltm_sispetro_send_data( )->cancel_oc( it_key = it_key ).

        IF lt_return IS NOT INITIAL.
          " Classe para converte objetos de msg para tabela de msg
          /scmtms/cl_common_helper=>msg_convert_bapiret2_2_bopf( EXPORTING iv_node_key   = is_ctx-node_key        " Node Key
                                                                           it_key        = it_key                 " Key Table
                                                                           it_return     = lt_return              " Table with BAPI Return Information
                                                                 CHANGING  co_message    = eo_message             " Interface of Message Object
                                                                           ct_failed_key = et_failed_key ).       " Failed Key Table

        ENDIF.
      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
