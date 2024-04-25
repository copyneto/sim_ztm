FUNCTION zfmtm_gravar_motoristas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_MOTORA) TYPE  ZI_TM_MOTORISTAS
*"     VALUE(IT_DOC_ADD) TYPE  ZCTGTM_DOC_ADD
*"----------------------------------------------------------------------
  DATA: ls_motora  TYPE zttm_motoristas,
        lt_doc_add TYPE STANDARD TABLE OF zttm_doc_add.

  ls_motora  = VALUE #( id                     = is_motora-id
                        businesspartnerredesim = is_motora-BusinessPartnerRedeSim
                        cpf                    = is_motora-cpf
                        nome                   = is_motora-nome
                        rg                     = is_motora-rg
                        cnh                    = is_motora-cnh
                        validadecnh            = is_motora-validadecnh
                        categoriacnh           = is_motora-categoriacnh
                        bp                     = is_motora-bp
                        status                 = is_motora-status
                        created_by             = is_motora-createdby
                        created_at             = is_motora-createdat
                        last_changed_by        = is_motora-lastchangedby
                        last_changed_at        = is_motora-lastchangedat
                        local_last_changed_at  = is_motora-locallastchangedat  ).

  lt_doc_add = VALUE #( FOR ls_doc_ IN it_doc_add (
                         id                    = ls_doc_-id
                         paiid                 = ls_doc_-paiid
                         cpf                   = ls_doc_-cpf
                         tipo                  = ls_doc_-tipo
                         numero                = ls_doc_-numero
                         validadedesde         = ls_doc_-validadedesde
                         validade              = ls_doc_-validade
                         uf                    = ls_doc_-uf
                         descricao             = ls_doc_-descricao
                         created_by            = ls_doc_-created_by
                         created_at            = ls_doc_-created_at
                         last_changed_by       = ls_doc_-last_changed_by
                         last_changed_at       = ls_doc_-last_changed_at
                         local_last_changed_at = ls_doc_-local_last_changed_at ) ).

  IF ls_motora IS NOT INITIAL.
    MODIFY zttm_motoristas FROM ls_motora.
  ENDIF.

  IF lt_doc_add IS NOT INITIAL.
    MODIFY zttm_doc_add FROM TABLE lt_doc_add.
  ENDIF.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
