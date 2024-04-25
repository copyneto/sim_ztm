class ZTMCL_DET_CONTA definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WLF_ACCOUNT_DETERMIN .
protected section.
private section.
ENDCLASS.



CLASS ZTMCL_DET_CONTA IMPLEMENTATION.


METHOD if_ex_wlf_account_determin~change_account_assignment.

  DATA: lo_tor_mgr    TYPE REF TO /bobf/if_tra_service_manager,
        lt_parameters TYPE /bobf/t_frw_query_selparam,
        ls_parameter  TYPE /bobf/s_frw_query_selparam,
        lt_data_root  TYPE /scmtms/t_tor_root_k.


  ASSIGN i_komlfk->* TO FIELD-SYMBOL(<header_structure>).
  ASSIGN COMPONENT 'USE_CASE' OF STRUCTURE <header_structure> TO FIELD-SYMBOL(<source_field>).

  CASE <source_field>.
    WHEN '30'. "compras/transf
      DATA(lv_field_kvslv) = 'KVSL1'.
      DATA(lv_field_kont1) = 'SAKN1'.
      DATA(lv_field_kont2) = 'SAKN2'.
    WHEN '31'. "vendas
      lv_field_kvslv = 'KVSL2'.
      lv_field_kont1 = 'SAKR1'.
      lv_field_kont2 = 'SAKR2'.
  ENDCASE.
  ASSIGN i_komlfp->* TO FIELD-SYMBOL(<source_structure>).
  ASSIGN COMPONENT 'REF_DOC_NR_2' OF STRUCTURE <source_structure> TO <source_field>.

  CHECK sy-subrc = 0.
  lo_tor_mgr ?= /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_tor_c=>sc_bo_key ).

  ls_parameter-attribute_name = /scmtms/if_tor_c=>sc_node_attribute-root-tor_id.
  ls_parameter-sign = 'I'.
  ls_parameter-option = 'EQ'.
  ls_parameter-low = <source_field>.
  APPEND ls_parameter TO lt_parameters.
  lo_tor_mgr->query( EXPORTING iv_query_key = /scmtms/if_tor_c=>sc_query-root-root_elements

    it_selection_parameters = lt_parameters
    iv_fill_data = abap_true
    IMPORTING et_data = lt_data_root ).

  CHECK lt_data_root IS NOT INITIAL.

  DATA(lv_tor_type) = lt_data_root[ 1 ]-tor_type.

  IF lv_tor_type = 'ZOF5'.
    lv_field_kvslv = 'KVSL2'.
    lv_field_kont1 = 'SAKR1'.
    lv_field_kont2 = 'SAKR2'.
  ENDIF.

  ASSIGN c_komlfac->* TO FIELD-SYMBOL(<target_structure>).
  ASSIGN COMPONENT lv_field_kvslv OF STRUCTURE <target_structure> TO FIELD-SYMBOL(<target_field>).

  SELECT SINGLE hkont INTO @DATA(lv_conta)
    FROM ztmt_det_conta
    WHERE tor_type = @lv_tor_type
      AND kvslv = @<target_field>.

  CHECK sy-subrc = 0.

  ASSIGN COMPONENT lv_field_kont1 OF STRUCTURE <target_structure> TO <target_field>.

  CHECK sy-subrc = 0.

  <target_field> = lv_conta.

  ASSIGN COMPONENT lv_field_kont2 OF STRUCTURE <target_structure> TO <target_field>.

  CHECK sy-subrc = 0.

  <target_field> = lv_conta.


ENDMETHOD.


  METHOD if_ex_wlf_account_determin~change_before_determination.
  ENDMETHOD.


  METHOD if_ex_wlf_account_determin~change_before_determination_sd.
  ENDMETHOD.


  METHOD if_ex_wlf_account_determin~change_before_recon_determ.
  ENDMETHOD.


  METHOD if_ex_wlf_account_determin~change_determination_type.
  ENDMETHOD.


  METHOD if_ex_wlf_account_determin~display_accounting_analysis.
  ENDMETHOD.
ENDCLASS.
