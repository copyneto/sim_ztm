class ZCLTM_INTEGRACAO_SIMFRETES_OF definition
  public
  final
  create public .

public section.

  methods EXECUTAR
    returning
      value(RT_RETURN) type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZCLTM_INTEGRACAO_SIMFRETES_OF IMPLEMENTATION.


  METHOD executar.

    DATA:
    lo_message TYPE REF TO /bobf/if_frw_message.

    DATA:
      lt_cond_id       TYPE /scmtms/t_condition_id,
      lt_cond_key      TYPE /bobf/t_frw_key,
      ls_cond_key      TYPE /bobf/s_frw_key,
      ls_tor_type_rule TYPE /scmtms/s_tor_type_det_rule,
      ls_tor_info      TYPE /scmtms/s_tor_fo_info,
      ls_tor_root      TYPE /scmtms/s_tor_root_k,
      ls_fo_key        TYPE /bobf/s_frw_key,
      lt_failed_key    TYPE /bobf/t_frw_key.



    DATA:
    lv_cond_id TYPE /scmtms/condition_id.

*    CLEAR et_fo_key.

    lv_cond_id = 'ZFO_TYPE_DETERMINATION'.
    INSERT lv_cond_id INTO TABLE lt_cond_id.
    lt_cond_key = /scmtms/cl_cond_ol=>condid_get_condkey( it_cond_it = lt_cond_id ).

    READ TABLE lt_cond_key INTO ls_cond_key INDEX 1.
    IF sy-subrc = 0.
      CLEAR ls_tor_type_rule.
      ls_tor_type_rule-det_rule = /scmtms/if_pln_const=>sc_tor_type_det-cond_based.
      ls_tor_type_rule-torty_det_cond = ls_cond_key-key.
    ENDIF.

    CLEAR ls_tor_info.
    ls_tor_info-loc_src_key = ''. "ms_tor_info-src_key. "source location bopf key
    ls_tor_info-loc_dst_key = ''. " ms_tor_info-dst_key. "destination location bopf key
    ls_tor_info-pickup_start_date = ''. " ms_tor_info-pickup_start. "FO start date


*this does the FO creation, you pass the creation type and the Tor type det rule
    /scmtms/cl_tor_factory=>create_tor_fo(
    EXPORTING
    iv_do_modify = abap_true
    iv_creation_type = /scmtms/if_tor_const=>sc_creation_type-manual
    is_tor_type_rules = ls_tor_type_rule
    is_fo_info = ls_tor_info
    IMPORTING
    es_tor_root = ls_tor_root
    et_failed_key = lt_failed_key
    CHANGING
    co_message = lo_message ).

  ENDMETHOD.
ENDCLASS.
