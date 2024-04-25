FUNCTION zfmtm_aloc_manual_insert_stop.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_CT_KEY) TYPE  /BOBF/T_FRW_KEY
*"     VALUE(IT_FU_KEY) TYPE  /BOBF/T_FRW_KEY
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  FREE: et_return.

  DATA(lo_aloc_manual) = NEW zcltm_aloc_manual_action( ).

  lo_aloc_manual->insert_stop_for_fo( EXPORTING it_ct_key   = it_ct_key
                                                it_fu_key   = it_fu_key
                                      IMPORTING et_messages = DATA(lt_messages) ).

  et_return = VALUE #( FOR ls_messages_ IN lt_messages ( id         = ls_messages_-msgid
                                                         number     = ls_messages_-msgno
                                                         type       = ls_messages_-severity
                                                         message_v1 = ls_messages_-parameter_1
                                                         message_v2 = ls_messages_-parameter_2
                                                         message_v3 = ls_messages_-parameter_3
                                                         message_v4 = ls_messages_-parameter_4 ) ).

ENDFUNCTION.
