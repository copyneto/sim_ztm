FUNCTION zfmtm_cad_motoristas2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_PROCESSO) TYPE  I OPTIONAL
*"     VALUE(IS_CENTRALDATAPERSON) TYPE  BAPIBUS1006_CENTRAL_PERSON
*"       OPTIONAL
*"  TABLES
*"      RT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      IT_DOCS STRUCTURE  ZTTM_DOC_ADD
*"  CHANGING
*"     VALUE(CS_MOTORA) TYPE  ZTTM_MOTORISTAS OPTIONAL
*"----------------------------------------------------------------------

  DATA:
    ls_centraldata TYPE bapibus1006_central,
    lv_bp          TYPE zttm_motoristas-bp.

  CASE iv_processo.

    WHEN 1.

      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory   = '1'
          centraldata       = ls_centraldata
          centraldataperson = is_centraldataperson
        IMPORTING
          businesspartner   = cs_motora-bp
        TABLES
          return            = rt_return.

      IF NOT line_exists( rt_return[ type = 'E' ] ).

*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = abap_false.

      ELSE.
        cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
      ENDIF.

    WHEN 2.

      DO 2 TIMES.
        DATA lv_taxtype TYPE bapibus1006tax-taxtype.
        DATA lv_taxnumber TYPE bapibus1006tax-taxnumber.

        lv_taxtype = COND #( WHEN sy-index EQ 1 THEN 'BR5' ELSE 'BR2' ).
        lv_taxnumber = COND #( WHEN sy-index EQ 1 THEN cs_motora-Rg ELSE cs_motora-Cpf ).
        CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
          EXPORTING
            businesspartner = cs_motora-bp
            taxtype         = lv_taxtype
            taxnumber       = lv_taxnumber
          TABLES
            return          = rt_return.

        IF line_exists( rt_return[ type = 'E' ] ).
          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        ENDIF.

      ENDDO.

      RETURN.

    WHEN 3.

      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner     = cs_motora-bp
          businesspartnerrole = 'TM0001'
          validfromdate       = '19000101'
          validuntildate      = '99991231'
        TABLES
          return              = rt_return.

      IF NOT line_exists( rt_return[ type = 'E' ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        CALL FUNCTION '/SCMB/PRR_UPD'
          EXPORTING
            iv_tcode = sy-tcode
            it_prr   = VALUE /scmb/tt_prr_upd( ( calendar = 'BR'
                                                 valid_from = '19000101'
                                                 valid_to = '99991231' ) ).

*        IF sy-subrc EQ 0.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

*        ENDIF.

      ELSE.
        cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
        EXIT.
      ENDIF.

    WHEN 4.

      DO lines( it_docs ) + 1 TIMES.

        DATA(lv_identificationcategory) =  COND #( WHEN sy-index EQ lines( it_docs ) + 1 THEN 'ZCNH' ELSE VALUE #( it_docs[ sy-index ]-tipo OPTIONAL ) ).

        DATA(lv_identificationnumber) =  COND #( WHEN sy-index EQ lines( it_docs ) + 1 THEN cs_motora-Cnh ELSE COND #( WHEN it_docs[ sy-index ]-numero IS INITIAL THEN '0000000000' ELSE VALUE #( it_docs[ sy-index ]-numero OPTIONAL ) ) ).

        DATA(ls_identification) = VALUE bapibus1006_identification(
                                                                    idvalidfromdate = '190000101'
                                                                    idvalidtodate = COND #( WHEN sy-index EQ lines( it_docs ) + 1 THEN cs_motora-Validadecnh ELSE VALUE #( it_docs[ sy-index ]-validade OPTIONAL ) )
                                                                    idinstitute = COND #( WHEN sy-index EQ lines( it_docs ) + 1 THEN cs_motora-Categoriacnh ELSE VALUE #( it_docs[ sy-index ]-descricao OPTIONAL ) )
                                                                    country = COND #( WHEN lv_identificationcategory = 'ZMOPP' THEN  'BR' )
                                                                    region = COND #( WHEN lv_identificationcategory = 'ZMOPP' THEN  VALUE #( it_docs[ sy-index ]-uf OPTIONAL ) )
                                                                 ).

        CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
          EXPORTING
            businesspartner        = cs_motora-bp
            identificationcategory = CONV bu_id_category( lv_identificationcategory )
            identificationnumber   = CONV bu_id_number( lv_identificationnumber )
            identification         = ls_identification
          TABLES
            return                 = rt_return.

        IF NOT line_exists( rt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        ELSE.
          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.
        ENDIF.

      ENDDO.

    WHEN 5.
      DATA lv_idnumber TYPE bapibus1006_identification_key-identificationnumber.

      LOOP AT it_docs ASSIGNING FIELD-SYMBOL(<fs_docs>).
        lv_idnumber = <fs_docs>-numero.

        CALL FUNCTION 'BAPI_IDENTIFICATION_REMOVE'
          EXPORTING
            businesspartner        = cs_motora-bp
            identificationcategory = <fs_docs>-tipo
            identificationnumber   = lv_idnumber
          TABLES
            return                 = rt_return.

        IF NOT line_exists( rt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        ELSE.
          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.
        ENDIF.

      ENDLOOP.

  ENDCASE.

  UPDATE zttm_motoristas FROM cs_motora.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.


ENDFUNCTION.
