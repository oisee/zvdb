*"* use this source file for your ABAP unit test classes
CLASS lcl_ DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup.
    METHODS next FOR TESTING .
    METHODS get_stats FOR TESTING .
    DATA: cut TYPE REF TO zcl_vdb_stopwatch.
ENDCLASS.


CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    cut = zcl_vdb_stopwatch=>new( ).
  ENDMETHOD.
  METHOD next.
    cut->reset( ).
    DO 10 TIMES.
      cut->next( |Event { sy-index }| ).
    ENDDO.

    DATA(lt_) = cut->get_log( ).

    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_ )
        exp = 10
    ).

  ENDMETHOD.
  METHOD get_stats.
    cut->reset( ).
    DO 1000 TIMES.
      cut->next( |Event { sy-index }| ).
    ENDDO.

    DATA(lt_) = cut->get_log( ).

    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_ )
        exp = 1000
    ).

    data(ls_) = cut->get_stats(  ).
    cl_abap_unit_assert=>assert_equals(
        act = ls_-lines
        exp = 1000
    ).

    cl_abap_unit_assert=>assert_number_between(
      EXPORTING
        lower            = ls_-min
        upper            = ls_-max
        number           = ls_-average
    ).

    cl_abap_unit_assert=>assert_number_between(
      EXPORTING
        lower            = ls_-min
        upper            = ls_-max
        number           = ls_-median
    ).

  ENDMETHOD.
ENDCLASS.
