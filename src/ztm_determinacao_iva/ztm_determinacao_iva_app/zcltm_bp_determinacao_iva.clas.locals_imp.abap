CLASS lhc__determinacao DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _determinacao RESULT result.

ENDCLASS.

CLASS lhc__determinacao IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

ENDCLASS.
