class ZCLTM_TOR_ITEM_FIELDS definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCLTM_TOR_ITEM_FIELDS IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    DATA:
      lt_item_tr TYPE /scmtms/t_tor_item_tr_k.
    CONSTANTS:
      lc_field_item_descr TYPE string VALUE 'ZZ_ITEM_DESCR'.

    io_read->retrieve(
      EXPORTING
        iv_node                 = is_ctx-node_key
        it_key                  = it_key
      IMPORTING
        eo_message              = eo_message
        et_data                 = lt_item_tr
        et_failed_key           = et_failed_key
    ).

    IF et_failed_key IS NOT INITIAL OR lt_item_tr IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_item_tr ASSIGNING FIELD-SYMBOL(<fs_item_tr>).
      IF <fs_item_tr>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-ct_item.

        <fs_item_tr>-item_descr =  |{ /scmtms/if_tor_const=>sc_tor_item_category-ct_item } { <fs_item_tr>-ct_seq } - { <fs_item_tr>-ct_type }|.
        <fs_item_tr>-zz_item_descr =  VALUE #( lt_item_tr[ KEY item_parent_key COMPONENTS item_parent_key = <fs_item_tr>-key parent_key = <fs_item_tr>-parent_key  ]-item_descr OPTIONAL ).

        TRY.
            io_modify->update(
              iv_node           = is_ctx-node_key
              iv_key            = <fs_item_tr>-key
              is_data           = REF /scmtms/s_tor_item_tr_k( <fs_item_tr> )
              it_changed_fields = VALUE #( ( /scmtms/if_tor_c=>sc_node_attribute-item_tr-item_descr ) ( lc_field_item_descr ) )
            ).
          CATCH /bobf/cx_frw_contrct_violation.
            CONTINUE.
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
