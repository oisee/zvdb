CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_vdb_000_progress DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCOL_000_PROGRESS
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_vdb_000_progress.  "class under test

    METHODS: new FOR TESTING.
    METHODS: new_for_table FOR TESTING.

ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD new.

    DATA lt_ TYPE mara_tt.
    SELECT * FROM mara UP TO 100 ROWS
      INTO TABLE lt_ WHERE matnr > ''.


    DATA(lo_) = zcl_vdb_000_progress=>new_for_table( lt_ ).

    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      lo_->next( ).

    ENDLOOP.

  ENDMETHOD.


  METHOD new_for_table.

    DATA lt_ TYPE mara_tt.
    SELECT * FROM mara INTO TABLE lt_ WHERE matnr > ''.

    DATA(lo_) = zcl_vdb_000_progress=>new_for_table( lt_ ).

    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      DATA: lv_ok TYPE sap_bool.
      DATA(lv_) = lo_->next( IMPORTING ev_shown = lv_ok ).
      IF lv_ok = 'X'.
       "WRITE: / lv_, lr_->matnr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
