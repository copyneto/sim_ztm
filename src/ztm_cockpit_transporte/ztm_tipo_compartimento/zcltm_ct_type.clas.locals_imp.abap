CLASS lcl_step DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS validateFields FOR VALIDATE ON SAVE
      IMPORTING keys FOR _Step~validateFields.

    METHODS get_message
      IMPORTING is_message    TYPE bapiret2
      RETURNING VALUE(ro_msg) TYPE REF TO if_abap_behv_message.

ENDCLASS.

CLASS lcl_step IMPLEMENTATION.

  METHOD validateFields.

    CONSTANTS:
      BEGIN OF gc_message,
        id TYPE sy-msgid VALUE 'ZTM_COCKPIT_TRANSP',
      END OF gc_message,

      BEGIN OF gc_fields,
        StepId              TYPE string VALUE 'STEPID',
        compartmentcapacity TYPE string VALUE 'COMPARTMENTCAPACITY',
      END OF gc_fields,

      BEGIN OF gc_state_area,
        StepId              TYPE string VALUE 'VALIDATE_STEPID',
        compartmentcapacity TYPE string VALUE 'VALIDATE_COMPARTMENTCAPACITY',
      END OF gc_state_area.

    READ ENTITIES OF zi_tm_ct_type IN LOCAL MODE
      ENTITY _Step
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_step).

    CHECK lt_step IS NOT INITIAL.

    DATA(ls_step) = lt_step[ 1 ].

    IF ls_step-StepId IS INITIAL.

      APPEND VALUE #( %tky = ls_step-%tky ) TO failed-_step.

      APPEND VALUE #( %tky                          = ls_step-%tky
                      %state_area                   = gc_state_area-compartmentcapacity
                      %msg                          = me->get_message( is_message = VALUE bapiret2( id = gc_message-id  number = 0 type = if_xo_const_message=>error ) )
                      %element-StepId               = if_abap_behv=>mk-on
                      ) TO reported-_step.
    ENDIF.

    IF ls_step-compartmentcapacity IS INITIAL.

      APPEND VALUE #( %tky = ls_step-%tky ) TO failed-_step.

      APPEND VALUE #( %tky                          = ls_step-%tky
                      %state_area                   = gc_state_area-compartmentcapacity
                      %msg                          = me->get_message( is_message = VALUE bapiret2( id = gc_message-id  number = 0 type = if_xo_const_message=>error ) )
                      %element-compartmentcapacity  = if_abap_behv=>mk-on
                      ) TO reported-_step.
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

CLASS lcl_capacity DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS validateFields FOR VALIDATE ON SAVE
      IMPORTING keys FOR _Capacity~validateFields.

    METHODS get_message
      IMPORTING is_message    TYPE bapiret2
      RETURNING VALUE(ro_msg) TYPE REF TO if_abap_behv_message.

ENDCLASS.

CLASS lcl_capacity IMPLEMENTATION.

  METHOD validateFields.

    CONSTANTS:
      BEGIN OF gc_message,
        id TYPE sy-msgid VALUE 'ZTM_COCKPIT_TRANSP',
      END OF gc_message,

      BEGIN OF gc_fields,
        compartmenttype     TYPE string VALUE 'COMPARTMENTTYPE',
        compartmentunit     TYPE string VALUE 'COMPARTMENTUNIT',
        compartmentcapacity TYPE string VALUE 'COMPARTMENTCAPACITY',
      END OF gc_fields,

      BEGIN OF gc_state_area,
        compartmenttype     TYPE string VALUE 'VALIDATE_COMPARTMENTTYPE',
        compartmentunit     TYPE string VALUE 'VALIDATE_COMPARTMENTUNIT',
        compartmentcapacity TYPE string VALUE 'VALIDATE_COMPARTMENTCAPACITY',
      END OF gc_state_area.

    READ ENTITIES OF zi_tm_ct_type IN LOCAL MODE
      ENTITY _Capacity
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_capacity).

    CHECK lt_capacity IS NOT INITIAL.

    DATA(ls_capacity) = lt_capacity[ 1 ].

    IF ls_capacity-CompartmentCapacity IS INITIAL.

      APPEND VALUE #( %tky = ls_capacity-%tky ) TO failed-_capacity.

      APPEND VALUE #( %tky                          = ls_capacity-%tky
                      %state_area                   = gc_state_area-compartmentcapacity
                      %msg                          = me->get_message( is_message = VALUE bapiret2( id = gc_message-id  number = 0 type = if_xo_const_message=>error ) )
                      %element-compartmentcapacity  = if_abap_behv=>mk-on
                      ) TO reported-_capacity.
    ENDIF.

    IF ls_capacity-CompartmentUnit IS INITIAL.

      APPEND VALUE #( %tky = ls_capacity-%tky ) TO failed-_capacity.

      APPEND VALUE #( %tky                          = ls_capacity-%tky
                      %state_area                   = gc_state_area-compartmentunit
                      %msg                          = me->get_message( is_message = VALUE bapiret2( id = gc_message-id  number = 0 type = if_xo_const_message=>error ) )
                      %element-compartmentunit      = if_abap_behv=>mk-on
                      ) TO reported-_capacity.
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

CLASS lcl_Type DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _Type RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR _Type RESULT result.

ENDCLASS.

CLASS lcl_Type IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_features.

* ---------------------------------------------------------------------------
* Recupera os Tipos de Compartimento utilizados no Perfil
* ---------------------------------------------------------------------------
    READ ENTITIES OF zi_tm_ct_type IN LOCAL MODE ENTITY _Type
          ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(lt_type)
          FAILED DATA(lt_failed).

    SORT lt_type BY CompartmentType.

* ---------------------------------------------------------------------------
* Atualiza permissões de cada linha
* ---------------------------------------------------------------------------
    LOOP AT keys REFERENCE INTO DATA(ls_keys).

      READ TABLE lt_type INTO DATA(ls_type) WITH KEY CompartmentType = ls_keys->CompartmentType BINARY SEARCH.

      IF sy-subrc NE 0.
        CLEAR ls_type.
      ENDIF.

      result = VALUE #( BASE result
                      ( %tky             = ls_keys->%tky
                        %delete          = COND #( WHEN ls_type-CompartmentTypeCriticality NE gc_color-positive
                                                   THEN if_abap_behv=>fc-o-enabled
                                                   ELSE if_abap_behv=>fc-o-disabled )
                      ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
