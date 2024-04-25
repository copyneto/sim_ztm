CLASS lcl__CombinacaoRec DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _CombinacaoRec RESULT result.
    METHODS processar FOR MODIFY
      IMPORTING keys FOR ACTION _combinacaorec~processar.

ENDCLASS.

CLASS lcl__CombinacaoRec IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD processar.
    READ ENTITIES OF ZI_TM_COMBINACAO_RECURSOS IN LOCAL MODE
          ENTITY _CombinacaoRec ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(lt_combinacaorecursos)
          FAILED FINAL(lt_failed).

    IF lt_combinacaorecursos IS INITIAL.
      reported-_combinacaorec = VALUE #( (
        %msg = new_message(
          id       = 'ZTM_CARGA_VEICULO'
          number   = '006'
          severity = if_abap_behv_message=>severity-information
      ) ) ).
      RETURN.
    ENDIF.

    DATA:
      lt_vehicle_resources type /scmtms/t_sb_veh_res_combi_def.
    LOOP AT lt_combinacaorecursos ASSIGNING FIELD-SYMBOL(<fs_combinacaorecursos>).
      APPEND VALUE #(
        combination_resource_id = <fs_combinacaorecursos>-combination_resource_id
        sequence_no     = <fs_combinacaorecursos>-seqnum
        equipment_group = <fs_combinacaorecursos>-equitype
        equipment_type  = <fs_combinacaorecursos>-equicode
        resource_id     = <fs_combinacaorecursos>-resource_id
      ) TO lt_vehicle_resources.
    ENDLOOP.

    DATA(lo_res) = NEW zcltm_carga_veiculo( ).
    lo_res->main( it_vehicle_resources = lt_vehicle_resources ).


    reported-_combinacaorec = VALUE #( (
      %msg = new_message(
        id       = 'ZTM_CARGA_VEICULO'
        number   = '005'
        severity = if_abap_behv_message=>severity-success
    ) ) ).
  ENDMETHOD.

ENDCLASS.
