CLASS lcl_zeitm_desable_chgcharges DEFINITION DEFERRED.
CLASS /scmtms/cl_tcc_dao_tor DEFINITION LOCAL FRIENDS lcl_zeitm_desable_chgcharges.
CLASS lcl_zeitm_desable_chgcharges DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zeitm_desable_chgcharges. "#EC NEEDED
    DATA core_object TYPE REF TO /scmtms/cl_tcc_dao_tor .   "#EC NEEDED
 INTERFACES  IPO_ZEITM_DESABLE_CHGCHARGES.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO /scmtms/cl_tcc_dao_tor OPTIONAL.
ENDCLASS.
CLASS lcl_zeitm_desable_chgcharges IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD ipo_zeitm_desable_chgcharges~get_host_specific_prop.
*"------------------------------------------------------------------------*
*" Declaration of POST-method, do not insert any comments here please!
*"
*"methods GET_HOST_SPECIFIC_PROP
*"  importing
*"    !IS_CTX type /BOBF/S_FRW_CTX_DET
*"    !IT_HOST_ROOT_KEYS type /BOBF/T_FRW_KEY
*"  changing
*"    !RT_PROPERTIES type /SCMTMS/T_TCC_DO_PROPERTY .
*"------------------------------------------------------------------------*

    CLEAR: rt_properties.

    /scmtms/cl_tor_helper_common=>get_lead_sys_for_act_multi(
      EXPORTING
        iv_activity                   = /scmtms/if_acp_const=>sc_acp_activity-chg_charges
        it_k_root                     = it_host_root_keys
      IMPORTING
        et_map_root_actvt_is_lead_sys = DATA(lt_map_root_actvt_is_lead_sys) ).

    AUTHORITY-CHECK OBJECT 'Z_TM'
        ID 'ACTVT' FIELD '02' "update
        ID 'TM_ND' FIELD 'TRANSPORTCHARGES'.

    IF sy-subrc IS INITIAL.
      DATA(lv_update_enable) = abap_true.
    ELSE.
      lv_update_enable = abap_false.
    ENDIF.

    LOOP AT it_host_root_keys ASSIGNING FIELD-SYMBOL(<ls_host_root_key>).

      READ TABLE lt_map_root_actvt_is_lead_sys ASSIGNING FIELD-SYMBOL(<act_map>)
           WITH TABLE KEY root_activity COMPONENTS root_key = <ls_host_root_key>-key activity = /scmtms/if_acp_const=>sc_acp_activity-chg_charges.

      IF sy-subrc = 0.

        " host instance specific condition should be added here
        INSERT VALUE #( host_node_key = /scmtms/if_tor_c=>sc_node-root
                        host_root_key = <ls_host_root_key>-key
                        content_cat = /bobf/if_conf_c=>sc_content_nod
                        content_key = is_ctx-node_key
                        property_name = /bobf/if_conf_c=>sc_property_name_update_enable
                        value = lv_update_enable
                        final = abap_true
                        ) INTO TABLE rt_properties.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
