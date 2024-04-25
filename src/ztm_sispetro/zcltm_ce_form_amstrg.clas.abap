class ZCLTM_CE_FORM_AMSTRG definition
  public
  final
  create public .

public section.

  interfaces IF_RAP_QUERY_PROVIDER .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLTM_CE_FORM_AMSTRG IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA: lt_tab         TYPE zctgtm_return_ce_print_amst,
          lt_master_keys TYPE cl_somu_form_services=>ty_gt_key,
          lt_keys        TYPE cl_somu_form_services=>ty_gt_key,
          lt_ordem       TYPE /scmtms/t_tor_id.

    DATA: ls_ordem LIKE LINE OF lt_ordem.

    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services.

    DATA: lv_content TYPE xstring,
          lv_tor_id  TYPE /scmtms/tor_id.

    TRY.
        " Requested data
        IF io_request->is_data_requested(  ).

          " Paginação
          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                      THEN 0 ELSE lv_page_size )  .

          " Recupera filtros
          TRY.
              DATA(lo_parameters) = io_request->get_parameters(  ).
              DATA(lv_ordemfrete) = lo_parameters[ parameter_name = 'P_ORDEMFRETE' ]-value. "#EC CI_STDSEQ

              TRY.

                  DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK

                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.

*              IF sy-uname = 'CGARCIA'. "1 = 2.

                zclca_conv_routine=>zeros_input( EXPORTING iv_input  = lv_ordemfrete
                                                 IMPORTING ev_output = lv_tor_id ).
                ls_ordem = lv_tor_id.
                APPEND ls_ordem TO lt_ordem.
                CLEAR: ls_ordem,
                       lv_tor_id.

                " Cria instancia
                DATA(lo_controller) = NEW zcltm_print_amostra( ).

                lt_tab = lo_controller->call_form( it_ordem  = lt_ordem[]
                                                   iv_return = abap_true ).

*              ELSE.
*                DATA(lo_controller2) = NEW zcltm_ordem_carregamento( ).
*
*                lt_tab = VALUE #( ( stream_data = lo_controller2->call_form( iv_tor_id = CONV #( |{ lv_ordemfrete ALPHA = IN }| )
*                                                    iv_return = 'X' ) ) ).
*              ENDIF.

              IF lt_tab[] IS NOT INITIAL.
                io_response->set_total_number_of_records( 1 ).

                " -------------- Send the response back to UI------------
                io_response->set_data( lt_tab ).
              ENDIF.

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.

        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
