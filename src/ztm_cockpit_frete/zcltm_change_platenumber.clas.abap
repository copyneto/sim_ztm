class ZCLTM_CHANGE_PLATENUMBER definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  types:
    BEGIN OF ty_resource,
           tmsresuid   TYPE /scmtms/uuid_id,
           res_id      TYPE /scmtms/res_name,
           platenumber TYPE /scmtms/resplatenr,
         END OF ty_resource .
  types:
    ty_t_resource TYPE TABLE OF ty_resource .

  constants:
    BEGIN OF gc_parametros,
        modulo TYPE ztca_param_mod-modulo VALUE 'TM',
        chave1 TYPE ztca_param_par-chave1 VALUE 'MTR',
        chave2 TYPE ztca_param_par-chave2 VALUE 'TIPO_MTR',
        chave3 TYPE ztca_param_par-chave3 VALUE '',
      END OF gc_parametros .
  data:
    gr_tipo_mtr TYPE RANGE OF  /scmtms/transmeanstypecode .
  data GT_RESOURCE type TY_T_RESOURCE .
  constants GC_TMSRESUID type CHAR20 value 'TMSRESUID' ##NO_TEXT.
  constants GC_RES_ID type CHAR20 value 'RES_ID' ##NO_TEXT.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
protected section.
private section.

  methods GET_PARAMETERS .
  methods GET_RESOURCE
    importing
      !IT_ITEM_TR type /SCMTMS/T_TOR_ITEM_TR_K .
ENDCLASS.



CLASS ZCLTM_CHANGE_PLATENUMBER IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    DATA: lt_item_tr TYPE /scmtms/t_tor_item_tr_k.

    io_read->retrieve( EXPORTING iv_node       = is_ctx-node_key
                                 it_key        = it_key
                       IMPORTING eo_message    = eo_message
                                 et_data       = lt_item_tr
                                 et_failed_key = et_failed_key ).

    IF et_failed_key IS NOT INITIAL OR lt_item_tr IS INITIAL.
      RETURN.
    ENDIF.

    get_parameters( ).
    CHECK gr_tipo_mtr IS NOT INITIAL.

    get_resource( it_item_tr = lt_item_tr ).
    CHECK gt_resource IS NOT INITIAL.

    LOOP AT lt_item_tr ASSIGNING FIELD-SYMBOL(<fs_item_tr>).

      IF ( <fs_item_tr>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-av_item   OR
           <fs_item_tr>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-pv_item ) AND
         <fs_item_tr>-mtr IN gr_tipo_mtr.

        IF <fs_item_tr>-res_key IS NOT INITIAL.
          IF line_exists( gt_resource[ tmsresuid = <fs_item_tr>-res_key ] ).                          "#EC CI_STDSEQ
            IF gt_resource[ tmsresuid = <fs_item_tr>-res_key ]-platenumber IS NOT INITIAL.            "#EC CI_STDSEQ
              <fs_item_tr>-platenumber = gt_resource[ tmsresuid = <fs_item_tr>-res_key ]-platenumber. "#EC CI_STDSEQ
            ELSE.
              CONTINUE.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        ELSE.
          IF line_exists( gt_resource[ res_id = <fs_item_tr>-res_id ] ).                              "#EC CI_STDSEQ
            IF gt_resource[ res_id = <fs_item_tr>-res_id ]-platenumber IS NOT INITIAL.                "#EC CI_STDSEQ
              <fs_item_tr>-platenumber = gt_resource[ res_id = <fs_item_tr>-res_id ]-platenumber.     "#EC CI_STDSEQ
            ELSE.
              CONTINUE.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF <fs_item_tr>-platenumber IS INITIAL.
          CONTINUE.
        ENDIF.

        TRY.
            io_modify->update(
              iv_node           = is_ctx-node_key
              iv_key            = <fs_item_tr>-key
              is_data           = REF /scmtms/s_tor_item_tr_k( <fs_item_tr> )
              it_changed_fields = VALUE #( ( /scmtms/if_tor_c=>sc_node_attribute-item_tr-platenumber ) )
            ).
          CATCH /bobf/cx_frw_contrct_violation.
            CONTINUE.
        ENDTRY.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_parameters.

    TRY.
        DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).

        CLEAR gr_tipo_mtr.

        lr_param->m_get_range( EXPORTING iv_modulo = gc_parametros-modulo
                                         iv_chave1 = gc_parametros-chave1
                                         iv_chave2 = gc_parametros-chave2
                               IMPORTING et_range  = gr_tipo_mtr ).

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_resource.

    " Tabela Interna
    DATA: lt_item_tr TYPE /scmtms/t_tor_item_tr_k.

    " Variáveis
    DATA: lv_where TYPE string.

    lt_item_tr[] = it_item_tr[].

    DELETE lt_item_tr WHERE item_cat <> /scmtms/if_tor_const=>sc_tor_item_category-av_item
                        AND item_cat <> /scmtms/if_tor_const=>sc_tor_item_category-pv_item. "#EC CI_SORTSEQ

    CHECK lt_item_tr IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_item_tr COMPARING res_key res_id.

    SELECT tmsresuid,
           res_id,
           platenumber
*      FROM /scmtms/cv_rseqr
      FROM /scmtms/cv_equipresourceroot
      INTO TABLE @gt_resource
      FOR ALL ENTRIES IN @lt_item_tr
      WHERE tmsresuid EQ @lt_item_tr-res_key
         OR res_id    EQ @lt_item_tr-res_id.

  ENDMETHOD.
ENDCLASS.
