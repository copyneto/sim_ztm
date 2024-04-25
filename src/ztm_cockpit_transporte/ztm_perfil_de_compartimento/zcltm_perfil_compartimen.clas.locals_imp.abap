CLASS lcl_ZI_TM_PERFIL_DE_COMPARTIME DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    TYPES: tt_reported_perfil TYPE TABLE FOR REPORTED LATE zi_tm_perfil_de_compartimento\\perfil.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR perfil RESULT result.

    METHODS validateFields FOR VALIDATE ON SAVE
      IMPORTING keys FOR perfil~validateFields.

    METHODS get_message
      IMPORTING is_message    TYPE bapiret2
      RETURNING
                VALUE(ro_msg) TYPE REF TO if_abap_behv_message.

ENDCLASS.

CLASS lcl_ZI_TM_PERFIL_DE_COMPARTIME IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD validateFields.
    CONSTANTS:
      lc_id               TYPE bapiret2-id     VALUE 'ZTM_COCKPIT_TRANSP',
      lc_number_000       TYPE bapiret2-number VALUE '000',
      lc_number_001       TYPE bapiret2-number VALUE '001',
      lc_number_002       TYPE bapiret2-number VALUE '002',
      lc_type_erro        TYPE bapiret2-type   VALUE 'E',
      lc_state_are_cmprof TYPE string VALUE 'VALIDATE_CMPROFILE',
      lc_state_are_sequen TYPE string VALUE 'VALIDATE_SEQUENCE',
      lc_state_are_ctype  TYPE string VALUE 'VALIDATE_CTYPE',
      lc_state_are_zzseal TYPE string VALUE 'VALIDATE_ZZSEALNUMBERTOTAL'.


    DATA: lo_msg TYPE REF TO if_abap_behv_message.

    DATA: lv_cmprofile TYPE if_abap_behv=>t_xflag,
          lv_sequence  TYPE if_abap_behv=>t_xflag,
          lv_ctype     TYPE if_abap_behv=>t_xflag,
          lv_text      TYPE if_abap_behv=>t_xflag.

    READ ENTITIES OF zi_tm_perfil_de_compartimento IN LOCAL MODE
      ENTITY Perfil
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_res).

    CHECK lt_res IS NOT INITIAL.

    DATA(ls_res) = lt_res[ 1 ].

    IF ls_res-Cmprofile IS INITIAL.
      APPEND VALUE #( %tky = ls_res-%tky ) TO failed-perfil.

      APPEND VALUE #( %tky        = ls_res-%tky
                      %state_area = lc_state_are_cmprof
                      %msg        = me->get_message( is_message = VALUE bapiret2( id = lc_id  number = lc_number_000 type = lc_type_erro ) )
                      %element-Cmprofile = if_abap_behv=>mk-on ) TO reported-perfil.
    ENDIF.

    IF ls_res-Sequence IS INITIAL.
      APPEND VALUE #( %tky = ls_res-%tky ) TO failed-perfil.

      APPEND VALUE #( %tky        = ls_res-%tky
                      %state_area = lc_state_are_sequen
                      %msg        = me->get_message( is_message = VALUE bapiret2( id = lc_id  number = lc_number_000 type = lc_type_erro ) )
                      %element-Sequence = if_abap_behv=>mk-on ) TO reported-perfil.
    ENDIF.

    IF ls_res-CType IS INITIAL.
      APPEND VALUE #( %tky = ls_res-%tky ) TO failed-perfil.

      APPEND VALUE #( %tky        = ls_res-%tky
                      %state_area = lc_state_are_ctype
                      %msg        = me->get_message( is_message = VALUE bapiret2( id = lc_id  number = lc_number_000 type = lc_type_erro ) )
                      %element-CType = if_abap_behv=>mk-on ) TO reported-perfil.
    ELSE.

      SELECT COUNT(*)
          FROM ZI_TM_VH_Tipo_Compartimento
          WHERE CType = @ls_res-CType.

      IF sy-subrc NE 0.

        APPEND VALUE #( %tky = ls_res-%tky ) TO failed-perfil.

        " Tipo de compartimento não existe.
        APPEND VALUE #( %tky        = ls_res-%tky
                        %state_area = lc_state_are_ctype
                        %msg        = me->get_message( is_message = VALUE bapiret2( id = lc_id  number = lc_number_002 type = lc_type_erro ) )
                        %element-CType = if_abap_behv=>mk-on ) TO reported-perfil.
      ENDIF.
    ENDIF.

    IF ls_res-ZzSealNumberTotal IS INITIAL.
      APPEND VALUE #( %tky = ls_res-%tky ) TO failed-perfil.

      APPEND VALUE #( %tky        = ls_res-%tky
                      %state_area = lc_state_are_zzseal
                      %msg        = me->get_message( is_message = VALUE bapiret2( id = lc_id  number = lc_number_000 type = lc_type_erro ) )
                      %element-ZzSealNumberTotal = if_abap_behv=>mk-on ) TO reported-perfil.

    ENDIF.

    IF ls_res-ZzSealNumberTotal > 8.

      APPEND VALUE #( %tky = ls_res-%tky ) TO failed-perfil.

      " Total de Lacres não pode ultrapassar de &1.
      APPEND VALUE #( %tky        = ls_res-%tky
                      %state_area = lc_state_are_zzseal
                      %msg        = me->get_message( is_message = VALUE bapiret2( id = lc_id  number = lc_number_001 type = lc_type_erro
                                                     message_v1 = |{ CONV char50( zcltm_cockpit_frete_event=>gc_equi_fields-max_seal_number_perfil ) ALPHA = OUT }| ) )
                      %element-ZzSealNumberTotal = if_abap_behv=>mk-on ) TO reported-perfil.

    ENDIF.

  ENDMETHOD.

  METHOD get_message.
    CONSTANTS:
      lc_type_erro TYPE bapiret2-type   VALUE 'E',
      lc_type_a    TYPE bapiret2-type   VALUE 'A',
      lc_type_succ TYPE bapiret2-type   VALUE 'S'.
    DATA lv_msgtx TYPE char200.

    CALL FUNCTION 'FI_MESSAGE_TEXT_GET'
      EXPORTING
        i_langu = sy-langu
        i_msgid = is_message-id
        i_msgno = is_message-number
        i_msgty = is_message-type
        i_msgv1 = is_message-message_v1
        i_msgv2 = is_message-message_v2
        i_msgv3 = is_message-message_v3
        i_msgv4 = is_message-message_v4
      IMPORTING
        e_msgtx = lv_msgtx.

    ro_msg = new_message_with_text(
                  severity = COND #(
                                       WHEN is_message-type EQ lc_type_erro THEN if_abap_behv_message=>severity-error
                                       WHEN is_message-type EQ lc_type_succ THEN if_abap_behv_message=>severity-success
                                       WHEN is_message-type EQ lc_type_a THEN if_abap_behv_message=>severity-warning
                                       ELSE if_abap_behv_message=>severity-none
                                    )
                  text = lv_msgtx
              ).

  ENDMETHOD.

ENDCLASS.
