
CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_vdb_002_lib.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: dp_8       FOR TESTING.
    METHODS: dp_16      FOR TESTING.
    METHODS: hamming_8  FOR TESTING.
    METHODS: hamming_16 FOR TESTING.
    METHODS: hamming_24 FOR TESTING.
    METHODS: hamming_8x8 FOR TESTING.
    METHODS: fast_hash  FOR TESTING.

ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    cut = zcl_vdb_002_lib=>new( '' ). "default bid = ''
  ENDMETHOD.

  METHOD teardown.


  ENDMETHOD.

  METHOD hamming_8.
    DATA(lt_1) = cut->init_hamming_8bit_d1( ).
    DATA(lt_2) = cut->init_hamming_8bit_d2( ).
    DATA(lt_3) = cut->init_hamming_8bit_d3( ).
    DATA(lt_) = cut->init_hamming_8bit( ).

    cl_abap_unit_assert=>assert_equals(
        exp = lines( lt_1 )
        act = 8
    ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( lt_2 )
        act = 36
    ).
    cl_abap_unit_assert=>assert_equals(
     exp = lines( lt_3 )
     act = 92
    ).
  ENDMETHOD.

  METHOD hamming_8x8.
    RETURN.
    DATA(lt_16) = cut->hamming_16( 'F00F' ).
    DATA(lt_a8) = cut->hamming_8( 'F0' ).
    DATA(lt_b8) = cut->hamming_8( '0F' ).

    DATA(lt_8x8) = lt_16.
    REFRESH lt_8x8.
    DATA(lts_a8) = lt_a8[ 1 ].
    DELETE lt_a8 INDEX 1.
    DATA(lts_b8) = lt_b8[ 1 ].
    DELETE lt_b8 INDEX 1.

    APPEND INITIAL LINE TO lt_8x8 REFERENCE INTO DATA(lr_8x8).
    lr_8x8->sign   = 'I'.
    lr_8x8->option = 'EQ'.
    lr_8x8->low+1(1) = lts_b8-low.
    lr_8x8->low+0(1) = lts_a8-low.

    LOOP AT lt_a8 REFERENCE INTO DATA(lr_a8).
      APPEND INITIAL LINE TO lt_8x8 REFERENCE INTO lr_8x8.
      lr_8x8->sign   = 'I'.
      lr_8x8->option = 'EQ'.
      lr_8x8->low+1(1) = lts_b8-low.
      lr_8x8->low+0(1) = lr_a8->low.
    ENDLOOP.
    LOOP AT lt_b8 REFERENCE INTO DATA(lr_b8).
      APPEND INITIAL LINE TO lt_8x8 REFERENCE INTO lr_8x8.
      lr_8x8->sign   = 'I'.
      lr_8x8->option = 'EQ'.
      lr_8x8->low+1(1) = lr_b8->low.
      lr_8x8->low+0(1) = lts_a8-low.
    ENDLOOP.

    SORT lt_16  BY low.
    SORT lt_8x8 BY low.


    cl_abap_unit_assert=>assert_equals(
        exp = lt_16
        act = lt_8x8
    ).

*    DATA(lt_1) = cut->init_hamming_16bit_d1( ).
*    DATA(lt_2) = cut->init_hamming_16bit_d2( ).
*    DATA(lt_3) = cut->init_hamming_16bit_d3( ).
*    DATA(lt_) = cut->init_hamming_16bit( ).
*
*    cl_abap_unit_assert=>assert_equals(
*        exp = lines( lt_1 )
*        act = 16
*    ).
*    cl_abap_unit_assert=>assert_equals(
*     exp = lines( lt_3 )
*     act = 696
*    ).

  ENDMETHOD.

  METHOD hamming_16.
    DATA(lt_1) = cut->init_hamming_16bit_d1( ).
    DATA(lt_2) = cut->init_hamming_16bit_d2( ).
    DATA(lt_3) = cut->init_hamming_16bit_d3( ).
    DATA(lt_) = cut->init_hamming_16bit( ).

    cl_abap_unit_assert=>assert_equals(
        exp = lines( lt_1 )
        act = 16
    ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( lt_2 )
        act = 136
    ).
    cl_abap_unit_assert=>assert_equals(
     exp = lines( lt_3 )
     act = 696
    ).

  ENDMETHOD.

  METHOD hamming_24.
    DATA(lt_1) = cut->init_hamming_24bit_d1( ).
    DATA(lt_2) = cut->init_hamming_24bit_d2( ).
    DATA(lt_3) = cut->init_hamming_24bit_d3( ).
    DATA(lt_) = cut->init_hamming_24bit( ).

    cl_abap_unit_assert=>assert_equals(
        exp = lines( lt_1 )
        act = 24
    ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( lt_2 )
        act = 300
    ).
    cl_abap_unit_assert=>assert_equals(
     exp = lines( lt_3 )
     act = 2324
    ).

  ENDMETHOD.

  METHOD fast_hash.
    DATA lx_(192) TYPE x.

    lx_ = '555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555'.
    DATA(lx_h1) = cut->hash_fast_fold( lx_ ).

    cl_demo_output=>write( lx_h1 ).
    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD dp_8   .
    DATA(lv_) = cut->dp_8(
                  ix_0 = '0F'
                  ix_1 = '0F'
                ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lv_
        exp                  = 8
    ).

  ENDMETHOD.
  METHOD dp_16  .
    DATA(lv_) = cut->dp_16(
                  ix_0 = '00FF'
                  ix_1 = '0F0F'
                ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lv_
        exp                  = 0
    ).

  ENDMETHOD.


ENDCLASS.
