*&---------------------------------------------------------------------*
*& Report Zvdb_002_UPLOAD_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvdb_002_demo_02_query MESSAGE-ID zvdb_002.

TABLES: sscrfields.

CLASS lcl_ DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_alv.
             INCLUDE TYPE zvdb_002_vector.
           TYPES:
                    hh8         TYPE sap_bool,
                    hh8ha       TYPE sap_bool,
                    hh16        TYPE sap_bool,
                    hh16ha      TYPE sap_bool,
                    hr8         TYPE sap_bool,
                    hr8ha       TYPE sap_bool,
                    hr16        TYPE sap_bool,
                    hr16ha      TYPE sap_bool,
                    bf          TYPE sap_bool,
                    bf_only     TYPE sap_bool,
                    dh1         TYPE i,
                    dh2         TYPE i,
                    dr1         TYPE i,
                    dr2         TYPE i,
                    fh          TYPE xstring,
                    fh_or       TYPE xstring,
                    fh_and      TYPE xstring,
                    fh_rank     TYPE i,
                    fh_rank_or  TYPE i,
                    fh_rank_and TYPE i.

    TYPES: END OF ts_alv.
    TYPES: tt_alv TYPE STANDARD TABLE OF ts_alv WITH KEY id ip.
*
*    METHODS: preview_list IMPORTING ir_ TYPE REF TO tt_ RETURNING VALUE(rv_) TYPE sy-ucomm.
    METHODS qquery IMPORTING iv_ TYPE guid.
    METHODS query IMPORTING iv_ TYPE guid.
    METHODS q     IMPORTING iv_ TYPE guid.
    METHODS embed.
    METHODS answer.
    METHODS upload.
    DATA: gv_msg.
ENDCLASS.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
SELECTION-SCREEN FUNCTION KEY 4.

PARAMETERS: q TYPE guid DEFAULT '000D3A7A9A961EDE9CD283424391F4F2' MATCHCODE OBJECT zvdb_002_vector_sh. "ABAP is programming language for SAP R/3.

*PARAMETERS: p_d AS CHECKBOX.
PARAMETERS: p_bid TYPE zvdb_002_vector-bid MATCHCODE OBJECT zvdb_002_vector_sh.
SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-b05.
PARAMETERS: p_h0 AS CHECKBOX USER-COMMAND r01 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b05.

SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-b10.
PARAMETERS: p_h1 AS CHECKBOX USER-COMMAND r01 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b15 WITH FRAME TITLE TEXT-b15.
PARAMETERS: p_h1_1 AS CHECKBOX DEFAULT 'X' USER-COMMAND r01 MODIF ID r01.
PARAMETERS: p_h1_2 AS CHECKBOX DEFAULT 'X' MODIF ID r01.

SELECTION-SCREEN END OF BLOCK b15.
SELECTION-SCREEN END OF BLOCK b10.

SELECTION-SCREEN BEGIN OF BLOCK b20 WITH FRAME TITLE TEXT-b20.
PARAMETERS: p_h2 AS CHECKBOX DEFAULT 'X' USER-COMMAND r02 .
SELECTION-SCREEN BEGIN OF BLOCK b25 WITH FRAME TITLE TEXT-b25.
PARAMETERS: p_h2_1 AS CHECKBOX DEFAULT 'X' USER-COMMAND r02 MODIF ID r02.
PARAMETERS: p_h2_2 AS CHECKBOX DEFAULT 'X' MODIF ID r02.
SELECTION-SCREEN END OF BLOCK b25.
SELECTION-SCREEN END OF BLOCK b20.

SELECTION-SCREEN BEGIN OF BLOCK b30 WITH FRAME TITLE TEXT-b30.
PARAMETERS: p_h3 AS CHECKBOX USER-COMMAND r03 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b35 WITH FRAME TITLE TEXT-b55.
PARAMETERS: p_h3_1 AS CHECKBOX DEFAULT 'X' USER-COMMAND r03 MODIF ID r03.
PARAMETERS: p_h3_2 AS CHECKBOX DEFAULT 'X' MODIF ID r03.

SELECTION-SCREEN END OF BLOCK b35.
SELECTION-SCREEN END OF BLOCK b30.

SELECTION-SCREEN BEGIN OF BLOCK b40 WITH FRAME TITLE TEXT-b40.
PARAMETERS: p_h4 AS CHECKBOX USER-COMMAND r04 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b45 WITH FRAME TITLE TEXT-b45.
PARAMETERS: p_h4_1 AS CHECKBOX DEFAULT 'X' USER-COMMAND r04 MODIF ID r04.
PARAMETERS: p_h4_2 AS CHECKBOX DEFAULT 'X' MODIF ID r04.

SELECTION-SCREEN END OF BLOCK b45.
SELECTION-SCREEN END OF BLOCK b40.


CLASS lcl_ IMPLEMENTATION.
  METHOD query.
    DATA(lo_sw) = zcl_vdb_002_stopwatch=>new( ).

    DATA(lo_lib) = zcl_vdb_002_lib=>new( iv_ = p_bid ).
    DATA(ls_v) = lo_lib->read_vector( q ).
    lo_sw->reset( ).
    DO 5 TIMES. "doing 5 times to get average
      DATA(lt_q) = lo_lib->query( ls_v-q1b ).
      lo_sw->next( ).
    ENDDO.
    cl_demo_output=>display( lo_sw->get_stats( ) ).

*    APPEND LINES OF lt_q TO lt_q.
*    APPEND LINES OF lt_q TO lt_q.
*    APPEND LINES OF lt_q TO lt_q.

    DATA(lo_s) = zcl_vdb_002_stopwatch=>new( ).
    lo_s->reset( ).
    DO 10 TIMES.
      lo_lib->bf_in_tt( EXPORTING ix_ = ls_v-q1b CHANGING ct_ = lt_q ).
      lo_s->next( ).
    ENDDO.

    "DATA(ls_batch) = lo_s->get_stats_for_batch(  ).
    DATA(ls_batch) = lo_s->get_stats(  ).

    cl_demo_output=>write( ls_batch ).
    cl_demo_output=>write( |One dot product for two 1536 dimensional vectors:{ ls_batch-average_f }| ).
    cl_demo_output=>display( ).

    lo_lib->qf_in_tt( EXPORTING is_ = ls_v CHANGING ct_ = lt_q ).
    SORT lt_q BY rank DESCENDING.

    DATA: lo_alv     TYPE REF TO cl_salv_table,
          lo_columns TYPE REF TO cl_salv_columns_table.

    TRY.
        " Create an instance of cl_salv_table
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_q ).

        " Get reference to the columns of the ALV
        lo_columns = lo_alv->get_columns( ).

        " Set column properties
        lo_columns->get_column( 'Q1B' )->set_long_text( 'Q1B' ).
        lo_columns->get_column( 'Q1B' )->set_technical( 'X' ).
        lo_columns->get_column( 'HH1' )->set_long_text( 'HH1' ).
        lo_columns->get_column( 'HH2' )->set_long_text( 'HH2' ).
        lo_columns->get_column( 'HR1' )->set_long_text( 'HR1' ).
        lo_columns->get_column( 'HR2' )->set_long_text( 'HR2' ).
        lo_columns->get_column( 'RANK' )->set_long_text( 'Rank' ).
        lo_columns->get_column( 'RANK2' )->set_long_text( 'Rank 2' ).

        " Additional settings like layout, variant, filter, sort can be set here
        lo_alv->get_display_settings( )->set_striped_pattern( 'X' ).
        lo_alv->get_functions( )->set_all( abap_true ).
        " Display the ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        " Handle exceptions
    ENDTRY.

  ENDMETHOD.
  METHOD qquery. "query with "quck-force" ranking
    DATA(lo_sw) = zcl_vdb_002_stopwatch=>new( ).
    DATA(lo_lib) = zcl_vdb_002_lib=>new( iv_ = p_bid ).
    DATA(ls_v) = lo_lib->read_vector( q ).
    lo_sw->reset( ).
    DO 5 TIMES.
      DATA(lt_q) = lo_lib->query( ls_v-q1b ).
      lo_sw->next( ).
    ENDDO.
    cl_demo_output=>display( lo_sw->get_stats( ) ).

    DATA(lo_s) = zcl_vdb_002_stopwatch=>new( ).
    lo_s->reset( ).
    DO 10 TIMES.
      lo_lib->qf_in_tt( EXPORTING is_ = ls_v CHANGING ct_ = lt_q ).
      lo_s->next( ).
    ENDDO.

    DATA(ls_batch) = lo_s->get_stats_for_batch( lines( lt_q ) ).

    cl_demo_output=>write( ls_batch ).
    cl_demo_output=>write( |One dot product for 4 hashes: 48 dimensional vectors):{ ls_batch-average_f }| ).
    cl_demo_output=>display( ).

    lo_lib->bf_in_tt( EXPORTING ix_ = ls_v-q1b CHANGING ct_ = lt_q ).
    SORT lt_q BY rank2 DESCENDING.

    DATA: lo_alv     TYPE REF TO cl_salv_table,
          lo_columns TYPE REF TO cl_salv_columns_table.

    TRY.
        " Create an instance of cl_salv_table
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_q ).

        " Get reference to the columns of the ALV
        lo_columns = lo_alv->get_columns( ).

        " Set column properties
        lo_columns->get_column( 'Q1B' )->set_long_text( 'Q1B' ).
        lo_columns->get_column( 'Q1B' )->set_technical( 'X' ).
        lo_columns->get_column( 'HH1' )->set_long_text( 'HH1' ).
        lo_columns->get_column( 'HH2' )->set_long_text( 'HH2' ).
        lo_columns->get_column( 'HR1' )->set_long_text( 'HR1' ).
        lo_columns->get_column( 'HR2' )->set_long_text( 'HR2' ).
        lo_columns->get_column( 'RANK' )->set_long_text( 'Rank' ).
        lo_columns->get_column( 'RANK2' )->set_long_text( 'Rank 2' ).

        " Additional settings like layout, variant, filter, sort can be set here
        lo_alv->get_display_settings( )->set_striped_pattern( 'X' ).
        lo_alv->get_functions( )->set_all( abap_true ).
        " Display the ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        " Handle exceptions
    ENDTRY.
  ENDMETHOD.

  METHOD q. "query with tracking - how this vector was selected from the DB.
    DATA(lo_lib) = zcl_vdb_002_lib=>new( p_bid ).
    DATA(ls_q) = lo_lib->read_vector( q ).
    DATA(ls_h) = lo_lib->hash_s( ls_q-q1b ).

    DATA(ltr_hh1) = lo_lib->hamming_8( ls_h-hh1 ).
    DATA(ltr_hh2) = lo_lib->hamming_16( ls_h-hh2 ).
    DATA(ltr_hr1) = lo_lib->hamming_8( ls_h-hr1 ).
    DATA(ltr_hr2) = lo_lib->hamming_16( ls_h-hr2 ).

* #TODO:search by flags (checkboxes, or, highlite indicate what was retrieval for each)
    DATA:lt_bf    TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_h8    TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_h8ha  TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_h16   TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_h16ha TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_r8    TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_r8ha  TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_r16   TYPE zcl_vdb_002_lib=>tt_vector.
    DATA:lt_r16ha TYPE zcl_vdb_002_lib=>tt_vector.

*--------------------------------------------------------------------*
    IF p_h0 = 'X'.
      SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 111 AS d
        FROM zvdb_002_vector
        INTO CORRESPONDING FIELDS OF TABLE @lt_bf
        WHERE bid = @p_bid AND
              id IS NOT NULL.
    ENDIF.
*--------------------------------------------------------------------*
    IF p_h1 = 'X'.
      IF p_h1_1 = 'X' AND ls_h-hh1 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 1 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_h8
          WHERE bid = @p_bid AND
                hh1 = @ls_h-hh1.
      ENDIF.
      IF p_h1_2 = 'X' AND ls_h-hh1 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 11 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_h8ha
          WHERE bid = @p_bid AND
                hh1 IN @ltr_hh1.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
    IF p_h2 = 'X'.
      IF p_h2_1 = 'X' AND ls_h-hh2 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 2 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_h16
          WHERE bid = @p_bid AND
                hh2 = @ls_h-hh2.
      ENDIF.
      IF p_h2_2 = 'X' AND ls_h-hh2 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 22 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_h16ha
          WHERE bid = @p_bid AND
                hh2 IN @ltr_hh2.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
    IF p_h3 = 'X'.
      IF p_h3_1 = 'X' AND ls_h-hr1 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 3 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_r8
          WHERE bid = @p_bid AND
                hr1 = @ls_h-hr1.
      ENDIF.
      IF p_h3_2 = 'X' AND ls_h-hr1 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 33 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_r8ha
          WHERE bid = @p_bid AND
                hr1 IN @ltr_hr1.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
    IF p_h4 = 'X'.
      IF p_h4_1 = 'X' AND ls_h-hr2 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 4 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_r16
          WHERE bid = @p_bid AND
                hr2 = @ls_h-hr2.
      ENDIF.
      IF p_h4_2 = 'X' AND ls_h-hr2 NE 0.
        SELECT id, q1b, p, hh1, hh2, hr1, hr2, r, 44 AS d
          FROM zvdb_002_vector
          INTO CORRESPONDING FIELDS OF TABLE @lt_r16ha
          WHERE bid = @p_bid AND
                hr2 IN @ltr_hr2.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
*    IF lt_v IS INITIAL.
*      RETURN.
*    ENDIF.
*--------------------------------------------------------------------*
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_bf
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_h8
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_h8ha
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_h16
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_h16ha
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_r8
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_r8ha
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_r16
    ).
    lo_lib->bf_in_tt( EXPORTING ix_ = ls_q-q1b
                    CHANGING  ct_ = lt_r16ha
    ).
*--------------------------------------------------------------------*
    SORT lt_bf        BY id DESCENDING.
    SORT lt_h8        BY id DESCENDING.
    SORT lt_h8ha      BY id DESCENDING.
    SORT lt_h16       BY id DESCENDING.
    SORT lt_h16ha     BY id DESCENDING.
    SORT lt_r8        BY id DESCENDING.
    SORT lt_r8ha      BY id DESCENDING.
    SORT lt_r16       BY id DESCENDING.
    SORT lt_r16ha     BY id DESCENDING.

    DATA: lt_alv TYPE tt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_bf    ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_h8    ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_h8ha  ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_h16   ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_h16ha ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_r8    ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_r8ha  ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_r16   ) TO lt_alv.
    APPEND LINES OF CORRESPONDING tt_alv( lt_r16ha ) TO lt_alv.

    SORT lt_alv BY id DESCENDING d ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING id.

*    data(lv_q_fh)     = lo_lib->hash_fast_fold( ls_q-q1b ).

    LOOP AT lt_alv REFERENCE INTO DATA(lr_alv).
      DATA(lr_h8) = REF #(    lt_h8[    id = lr_alv->id ] OPTIONAL ).
      IF lr_h8 IS BOUND.
        lr_alv->hh8 = 'X'.
      ENDIF.
      DATA(lr_h8ha) = REF #(  lt_h8ha[  id = lr_alv->id ] OPTIONAL ).
      IF lr_h8ha IS BOUND.
        lr_alv->hh8ha = 'X'.
      ENDIF.
      DATA(lr_h16) = REF #(   lt_h16[   id = lr_alv->id ] OPTIONAL ).
      IF lr_h16 IS BOUND.
        lr_alv->hh16 = 'X'.
      ENDIF.
      DATA(lr_h16ha) = REF #( lt_h16ha[ id = lr_alv->id ] OPTIONAL ).
      IF lr_h16ha IS BOUND.
        lr_alv->hh16ha = 'X'.
      ENDIF.
      DATA(lr_r8) = REF #(    lt_r8[    id = lr_alv->id ] OPTIONAL ).
      IF lr_r8 IS BOUND.
        lr_alv->hr8 = 'X'.
      ENDIF.
      DATA(lr_r8ha) = REF #(  lt_r8ha[  id = lr_alv->id ] OPTIONAL ).
      IF lr_r8ha IS BOUND.
        lr_alv->hr8ha = 'X'.
      ENDIF.
      DATA(lr_r16) = REF #(   lt_r16[   id = lr_alv->id ] OPTIONAL ).
      IF lr_r16 IS BOUND.
        lr_alv->hr16 = 'X'.
      ENDIF.
      DATA(lr_r16ha) = REF #( lt_r16ha[ id = lr_alv->id ] OPTIONAL ).
      IF lr_r16ha IS BOUND.
        lr_alv->hr16ha = 'X'.
      ENDIF.
      DATA(lr_bf) = REF #(    lt_bf[    id = lr_alv->id ] OPTIONAL ).
      IF lr_bf IS BOUND.
        lr_alv->bf = 'X'.
      ENDIF.
      IF lr_alv->bf     = 'X' AND
         lr_alv->hh8    = ' ' AND
         lr_alv->hh8ha  = ' ' AND
         lr_alv->hh16   = ' ' AND
         lr_alv->hh16ha = ' ' AND
         lr_alv->hr8    = ' ' AND
         lr_alv->hr8ha  = ' ' AND
         lr_alv->hr16   = ' ' AND
         lr_alv->hr16ha = ' '.
        lr_alv->bf_only = 'X'.
      ENDIF.

      lr_alv->dh1 = lo_lib->dp_8(
                     ix_0 = ls_q-hh1
                     ix_1 = lr_alv->hh1
                   ).
      lr_alv->dh2 = lo_lib->dp_16(
                     ix_0 = ls_q-hh2
                     ix_1 = lr_alv->hh2
                   ).
      lr_alv->dr1 = lo_lib->dp_8(
                     ix_0 = ls_q-hr1
                     ix_1 = lr_alv->hr1
                   ).
      lr_alv->dr2 = lo_lib->dp_16(
                     ix_0 = ls_q-hr2
                     ix_1 = lr_alv->hr2
                   ).

    ENDLOOP.
    SORT lt_alv BY rank DESCENDING.
*--------------------------------------------------------------------*
    DATA: lo_alv     TYPE REF TO cl_salv_table,
          lo_columns TYPE REF TO cl_salv_columns_table.

    TRY.
        " Create an instance of cl_salv_table
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_alv ).

        " Get reference to the columns of the ALV
        lo_columns = lo_alv->get_columns( ).

        " Configure the columns based on original it_mod_catalog
        " Repeat this for each column
        lo_columns->get_column( 'Q1B'        )->set_long_text( 'Q1B' ).
        lo_columns->get_column( 'Q1B'        )->set_technical( 'X'   ).
        lo_columns->get_column( 'HH1'        )->set_long_text( 'HH1' ).
        lo_columns->get_column( 'HH2'        )->set_long_text( 'HH2' ).
        lo_columns->get_column( 'HR1'        )->set_long_text( 'HR1' ).
        lo_columns->get_column( 'HR2'        )->set_long_text( 'HR2' ).
        lo_columns->get_column( 'DH1'        )->set_long_text( 'DH1' ).
        lo_columns->get_column( 'DH2'        )->set_long_text( 'DH2' ).
        lo_columns->get_column( 'DR1'        )->set_long_text( 'DR1' ).
        lo_columns->get_column( 'DR2'        )->set_long_text( 'DR2' ).
        lo_columns->get_column( 'RANK'       )->set_long_text( 'RANK1 BF' ).
        lo_columns->get_column( 'RANK2'      )->set_long_text( 'RANK2 QF' ).
*--------------------------------------------------------------------*
        lo_columns->get_column( 'FH'         )->set_long_text( 'FH'      ).
        lo_columns->get_column( 'FH_RANK'    )->set_long_text( 'FH Rank' ).
        lo_columns->get_column( 'FH_OR'      )->set_long_text( 'FH OR'   ).
        lo_columns->get_column( 'FH_RANK_OR' )->set_long_text( 'FHR OR'  ).
        lo_columns->get_column( 'FH_AND'     )->set_long_text( 'FH AND'  ).
        lo_columns->get_column( 'FH_RANK_AND')->set_long_text( 'FHR AND' ).
*--------------------------------------------------------------------*
        lo_columns->get_column( 'HH8'        )->set_long_text( 'HH8'     ).
        lo_columns->get_column( 'HH8HA'      )->set_long_text( 'HH8HA'   ).
        lo_columns->get_column( 'HH16'       )->set_long_text( 'HH16'    ).
        lo_columns->get_column( 'HH16HA'     )->set_long_text( 'HH16HA'  ).
        lo_columns->get_column( 'HR8'        )->set_long_text( 'HR8'     ).
        lo_columns->get_column( 'HR8HA'      )->set_long_text( 'HR8HA'   ).
        lo_columns->get_column( 'HR16'       )->set_long_text( 'HR16'    ).
        lo_columns->get_column( 'HR16HA'     )->set_long_text( 'HR16HA'  ).
        lo_columns->get_column( 'BF'         )->set_long_text( 'BF'      ).
        lo_columns->get_column( 'BF_ONLY'    )->set_long_text( 'BF Only' ).
        " Additional settings (layout, variant, filter, sort) can be configured here
        lo_alv->get_display_settings( )->set_striped_pattern( 'X' ).
        lo_alv->get_functions( )->set_all( abap_true ).
        " Display the ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        " Handle exceptions, e.g., column not found
        " Example: WRITE lx_salv_msg->get_text( ).
    ENDTRY.

  ENDMETHOD.
  METHOD embed.
    DATA: lt_text TYPE string_table.

    CALL FUNCTION 'TERM_CONTROL_EDIT'
      EXPORTING
        titel          = 'Enter text for embedding'
        langu          = sy-langu
      TABLES
        textlines      = lt_text
      EXCEPTIONS
        user_cancelled = 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    DATA: lv_text TYPE string.
    LOOP AT lt_text REFERENCE INTO DATA(lr_text).
      lv_text = |{ lv_text }{ lr_text->* }{ cl_abap_char_utilities=>cr_lf }|.
    ENDLOOP.

    "DATA(lo_e) = zcl_vdb_002_embedding=>new( p_bid ).
    DATA(lo_e) = zcl_vdb_002_embedding_full=>new( p_bid ).
    q = lo_e->embed_and_save( lv_text )-id.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD answer.
    DATA: lt_text TYPE string_table.

    CALL FUNCTION 'TERM_CONTROL_EDIT'
      EXPORTING
        titel          = 'Enter your question'
        langu          = sy-langu
      TABLES
        textlines      = lt_text
      EXCEPTIONS
        user_cancelled = 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    DATA: lv_text TYPE string.
    LOOP AT lt_text REFERENCE INTO DATA(lr_text).
      lv_text = |{ lv_text }{ lr_text->* }{ cl_abap_char_utilities=>cr_lf }|.
    ENDLOOP.

    "DATA(lo_e) = zcl_vdb_002_embedding=>new( p_bid ).
    DATA(lo_e) = zcl_vdb_002_embedding_full=>new( p_bid ).
    DATA(lv_id_q) = lo_e->embed_and_save( lv_text )-id.
    COMMIT WORK AND WAIT.
    DATA(lx_a) = lo_e->answer( lv_text ).
*--------------------------------------------------------------------*
    DATA(lo_lib) = zcl_vdb_002_lib=>new( p_bid ).
    DATA(lt_q) = lo_lib->query_by_id( lv_id_q ).
    DATA(lt_a) = lo_lib->query( lx_a ).

    DATA(lt_all) = lt_q.
    APPEND LINES OF lt_a TO lt_all.
    SORT lt_all BY id.
    DELETE ADJACENT DUPLICATES FROM lt_all.

    DATA: lv_prompt TYPE string.
    DATA: lt_used TYPE zcl_vdb_002_lib=>tt_vector.
    DATA(lv_text_answer) = lo_e->rag(
                             EXPORTING iv_ = lv_text
                                       it_ = lt_all
                             IMPORTING ev_prompt = lv_prompt
                                       et_used   = lt_used
    ).

    cl_demo_output=>write_text( lv_text_answer ).
    cl_demo_output=>write( '------------' ).
    cl_demo_output=>write( lv_prompt ).
    cl_demo_output=>write( '------------' ).
    cl_demo_output=>write( lt_used ).
    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD upload.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  sscrfields-functxt_01 = TEXT-f01.
  sscrfields-functxt_02 = TEXT-f02.
  sscrfields-functxt_03 = TEXT-f03.
  sscrfields-functxt_04 = TEXT-f04.

  SELECT SINGLE id
    FROM zvdb_002_vector
    WHERE bid = @p_bid AND
          id IS NOT NULL
    INTO @q.

AT SELECTION-SCREEN.
  DATA(lo_) = NEW lcl_( ).

  CASE sy-ucomm.
    WHEN 'ONLI'.
      lo_->q( q ).
    WHEN 'FC01'.
      lo_->query( q ).
    WHEN 'FC02'.
      lo_->qquery( q ).
    WHEN 'FC03'.
      lo_->embed( ).
    WHEN 'FC04'.
      lo_->answer( ).
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_h1 = '' AND screen-group1 = 'R01'.
      screen-input = 0.
    ENDIF.
    IF p_h2 = '' AND screen-group1 = 'R02'.
      screen-input = 0.
    ENDIF.
    IF p_h3 = '' AND screen-group1 = 'R03'.
      screen-input = 0.
    ENDIF.
    IF p_h4 = '' AND screen-group1 = 'R04'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
