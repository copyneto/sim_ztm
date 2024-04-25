"Name: \TY:/SCMTMS/CL_UI_VIEWEXIT_PLN_NEW\ME:_MODIFY_FU_DATA\SE:END\EI
ENHANCEMENT 0 ZEI_TM_PLN_FU_DATA.
*
  TYPES:
    BEGIN OF ty_keys,
      key_tr       TYPE /bobf/conf_key,
      product_id_i TYPE /scmtms/product_id,
    END OF ty_keys.
  DATA:
    lt_torfu_keys_aux TYPE TABLE OF ty_keys.

  lt_torfu_keys_aux = CORRESPONDING #( ct_data ).
  IF lt_torfu_keys_aux IS NOT INITIAL.
    SORT lt_torfu_keys_aux BY key_tr product_id_i.
    DELETE ADJACENT DUPLICATES FROM lt_torfu_keys_aux.

    SELECT parent_key, product_id, item_descr, erp_comp_code
      FROM /scmtms/d_torite
      INTO TABLE @DATA(lt_tor_item_fu_aux)
     FOR ALL ENTRIES IN @lt_torfu_keys_aux
      WHERE PARENT_KEY = @lt_torfu_keys_aux-key_tr
        AND product_id = @lt_torfu_keys_aux-product_id_i
        AND item_cat = 'PRD'.
    IF sy-subrc = 0.
      SORT lt_tor_item_fu_aux by parent_key product_id.
      LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>).
        ASSIGN COMPONENT 'KEY_TR' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value_key_tr>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        ASSIGN COMPONENT 'PRODUCT_ID_I' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value_product_id_i>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        READ TABLE lt_tor_item_fu_aux ASSIGNING FIELD-SYMBOL(<Fs_tor_item_fu_aux>)
        WITH KEY parent_key   = <fs_value_key_tr>
                 product_id = <fs_value_product_id_i> BINARY SEARCH.
        IF sy-subrc = 0.
         ASSIGN COMPONENT 'ITEM_DESCR_I' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value_item_Descr>).
         IF sy-subrc = 0.
           <fs_value_item_Descr> = <Fs_tor_item_fu_aux>-item_descr.
         ENDIF.
         ASSIGN COMPONENT 'ERP_COMP_CODE_I' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value_erp_comp_code>).
         IF sy-subrc = 0.
           <fs_value_erp_comp_code> = <Fs_tor_item_fu_aux>-erp_comp_code.
         ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
