CLASS zclfi_agrupador_faturas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS agrupar_fatura
      IMPORTING
        iv_ctcnumero      TYPE numc10
        iv_ctcserie       TYPE numc4
        iv_dataBasePopUp  TYPE dzfbdt
        iv_condPgtoPopUp  TYPE dzterm
        iv_formaPgtoPopUp TYPE schzw_bseg
      RETURNING
        VALUE(rt_return)  TYPE bapiret2_t .

    METHODS desagrupar_fatura
      IMPORTING
        iv_bukrs        TYPE rf05r-bukrs
        iv_augbl        TYPE rf05r-augbl
        iv_gjahr        TYPE rf05r-gjahr
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .

    METHODS return_message
      IMPORTING
        p_task TYPE any .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_return TYPE bapiret2_t .
ENDCLASS.



CLASS zclfi_agrupador_faturas IMPLEMENTATION.


  METHOD agrupar_fatura.

    CALL FUNCTION 'ZFMFI_AGRUPAR_FATURA'
      STARTING NEW TASK 'AGRUPAR_FATURA'
      CALLING me->return_message ON END OF TASK
      EXPORTING
        iv_ctcnumero      = iv_ctcnumero
        iv_ctcserie       = iv_ctcserie
        iv_databasepopup  = iv_dataBasePopUp
        iv_condpgtopopup  = iv_condPgtoPopUp
        iv_formapgtopopup = iv_formaPgtoPopUp.
    WAIT UNTIL lines( me->gt_return ) > 0.

    APPEND LINES OF me->gt_return TO rt_return.
    CLEAR: me->gt_return.

  ENDMETHOD.


  METHOD desagrupar_fatura.

    CALL FUNCTION 'ZFMFI_DESAGRUPAR_FATURA'
      STARTING NEW TASK 'DESAGRUPAR_FATURA'
      CALLING me->return_message ON END OF TASK
      EXPORTING
        iv_bukrs = iv_bukrs
        iv_augbl = iv_augbl
        iv_gjahr = iv_gjahr.

    WAIT UNTIL lines( me->gt_return ) > 0.

    APPEND LINES OF me->gt_return TO rt_return.
    CLEAR: me->gt_return.

  ENDMETHOD.


  METHOD return_message.
    CASE p_task.
      WHEN 'AGRUPAR_FATURA'.
        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_AGRUPAR_FATURA'
          IMPORTING
            et_return = me->gt_return.
      WHEN 'DESAGRUPAR_FATURA'.
        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_DESAGRUPAR_FATURA'
          IMPORTING
            et_return = me->gt_return.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
