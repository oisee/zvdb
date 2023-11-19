*&---------------------------------------------------------------------*
*& Report Zvdb_002_EMBED_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvdb_002_demo_02_reindex MESSAGE-ID zvdb_002.

TABLES: sscrfields.

*PARAMETERS: p_go AS CHECKBOX.
PARAMETERS: p_bid TYPE zvdb_002_vector-bid.
PARAMETERS: p_q   TYPE i DEFAULT '127'.
PARAMETERS: p_q3  TYPE i DEFAULT '7'.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
SELECTION-SCREEN FUNCTION KEY 4.

INITIALIZATION.
  sscrfields-functxt_01 = TEXT-f01.
  sscrfields-functxt_02 = TEXT-f02.
  sscrfields-functxt_03 = TEXT-f03.
  sscrfields-functxt_04 = TEXT-f04.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM compare.
      "RETURN.
      "PERFORM go.
    WHEN 'FC01'.
      RETURN.
      "PERFORM reembed. "will reembed everything - time and money
    WHEN 'FC02'.
      PERFORM reindex.
    WHEN 'FC03'.
      PERFORM rehash.
    WHEN 'FC04'.
      PERFORM compare.
    WHEN OTHERS.
  ENDCASE.

FORM go.
  DATA: lt_text TYPE string_t.

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

  DATA(lo_e) = zcl_vdb_002_embedding=>new( p_bid ).
  DATA(lv_v) = lo_e->embed( lv_text ).

  cl_demo_output=>display( lv_v ).

  "q = lo_e->embed_and_save( lv_text )-id.
  "COMMIT WORK AND WAIT.

ENDFORM.

FORM reembed.
  DATA(lo_e) = zcl_vdb_002_embedding=>new( p_bid ).

  SELECT *
    FROM zvdb_002_vector
    INTO TABLE @DATA(lt_v)
    WHERE bid = @p_bid AND
          id IS NOT NULL.

  DATA(lo_progress) = zcl_col_000_progress=>new( iv_total = lines( lt_v ) iv_step  = 100
  ).

  LOOP AT lt_v REFERENCE INTO DATA(lr_v).
    lo_progress->next( ).
    lr_v->q1b = lo_e->embed( lr_v->p ).
  ENDLOOP.

  MODIFY zvdb_002_vector FROM TABLE lt_v.
  COMMIT WORK AND WAIT.

ENDFORM.
FORM reindex.
  DATA:lx_ TYPE zcl_vdb_002_lib=>tv.
  DELETE FROM zvdb_002_vector WHERE q1b = lx_.
  COMMIT WORK AND WAIT.
  DATA(lo_l) = zcl_vdb_002_lib=>new( p_bid ).
  DATA(lo_s) = zcl_vdb_002_stopwatch=>new( ).
  lo_s->reset( ).
  DO 5 TIMES.
    lo_l->reindex_all( p_bid ).
    lo_s->next( ).
  ENDDO.
  COMMIT WORK AND WAIT.
  cl_demo_output=>display( lo_s->get_stats( ) ).
ENDFORM.
FORM rehash.
  DATA(lo_l) = zcl_vdb_002_lib=>new( p_bid ).

  DATA(lo_s) = zcl_vdb_002_stopwatch=>new( ).
  lo_l->regen_recal_hash_and_reindex( p_bid ).
  " lo_l->reindex_all( p_bid ).
  COMMIT WORK AND WAIT.
  lo_s->next( ).

  cl_demo_output=>write( lo_s->get_stats( ) ).
  cl_demo_output=>display( ).


ENDFORM.

FORM compare.
  TYPES: ty_q16 TYPE i.
  TYPES: tt_q16 TYPE STANDARD TABLE OF ty_q16 WITH DEFAULT KEY.
  TYPES: BEGIN OF ts_,
           n   TYPE i,
           vf  TYPE zcl_vdb_002_lib=>tt_embedding,
           q1b TYPE zcl_vdb_002_lib=>tv,
           q8b TYPE tt_q16,
           q3b TYPE tt_q16,
           rf  TYPE f,
           r1b TYPE i,
           r8b TYPE i,
           r3b TYPE i,
           p   TYPE string,
         END OF ts_.
  TYPES: tt_ TYPE STANDARD TABLE OF ts_ WITH DEFAULT KEY.

  TYPES: BEGIN OF ts_alv,
           n   TYPE i,
           rf  TYPE f,
           r1b TYPE i,
           r8b TYPE i,
           r3b TYPE i,
           p   TYPE string,
         END OF ts_alv.
  TYPES: tt_alv TYPE STANDARD TABLE OF ts_alv WITH DEFAULT KEY.

  DATA(lo_random_int) = cl_abap_random_int=>create(
    seed  = 42
    min   = 0
    max   = 256
  ).
  DATA(lo_random_f)   = cl_abap_random_float=>create(
    seed  = 42
  ).

*--------------------------------------------------------------------*
  SELECT * UP TO 50 ROWS
    FROM zvdb_002_vector
    INTO TABLE @DATA(lt_v)
    WHERE hh1 IS NOT NULL
    .

*--------------------------------------------------------------------*
  DATA(lo_l) = zcl_vdb_002_lib=>new( p_bid ).

  DATA: lt_    TYPE tt_.
  DATA: lt_alv TYPE tt_alv.


  DATA(lo_sdk) = zcl_oai_01_sdk_lite=>new(
  ).

  LOOP AT lt_v REFERENCE INTO DATA(lr_v).
    APPEND INITIAL LINE TO lt_ REFERENCE INTO DATA(lr_).
    DATA(lo_e_out) = lo_sdk->embeddings( zcl_oai_01_embed_in=>new( VALUE #( input = lr_v->p ) ) ).
    lr_->vf = lo_e_out->get_vector( ).
    lr_->p  = lr_v->p.
  ENDLOOP.

*  DO 50 TIMES.
*    APPEND INITIAL LINE TO lt_ REFERENCE INTO lr_.
*    lr_->n = sy-index.
*    DO 1536 TIMES.
*      APPEND INITIAL LINE TO lr_->vf REFERENCE INTO DATA(lr_v).
*      lr_v->* = ( lo_random_f->get_next( ) * 2 ) - 1.
*      APPEND INITIAL LINE TO lr_->q8b REFERENCE INTO DATA(lr_q8b).
*      lr_q8b->* = lr_v->* * p_q.
*      APPEND INITIAL LINE TO lr_->q3b REFERENCE INTO DATA(lr_q3b).
*      lr_q3b->* = lr_v->* * p_q3.
*    ENDDO.
*    lr_->q1b = lo_l->quantize( lr_->vf  ).
*  ENDDO.

  LOOP AT lt_ REFERENCE INTO lr_.
    lr_->n = sy-tabix.

    DO 1536 TIMES.
      DATA(lv_vf) = lr_->vf[ sy-index ].
*      APPEND INITIAL LINE TO lr_->vf REFERENCE INTO DATA(lr_v).
*      lr_v->* = ( lo_random_f->get_next( ) * 2 ) - 1.
      APPEND INITIAL LINE TO lr_->q8b REFERENCE INTO DATA(lr_q8b).
      lr_q8b->* = lv_vf * p_q.
      APPEND INITIAL LINE TO lr_->q3b REFERENCE INTO DATA(lr_q3b).
      lr_q3b->* = lv_vf * p_q3.
    ENDDO.
    lr_->q1b = lo_l->quantize( lr_->vf  ).

  ENDLOOP.


  DATA(ls_) = lt_[ 1 ].

  LOOP AT lt_ REFERENCE INTO lr_.
    lr_->r1b = lo_l->dp_1536(
                 ix_0 = ls_-q1b
                 ix_1 = lr_->q1b
               ).
    DATA(lv_i) = 1.
    DO 1535 TIMES.
      lr_->r8b = lr_->r8b + ( ( ls_-q8b[ lv_i ] * lr_->q8b[ lv_i ] ) / p_q )  .
      lr_->r3b = lr_->r3b + ( ( ls_-q3b[ lv_i ] * lr_->q3b[ lv_i ] ) / p_q3 )  .
      lr_->rf = lr_->rf + ( ls_-vf[ lv_i ] * lr_->vf[ lv_i ] ).
      ADD 1 TO lv_i.
    ENDDO.
  ENDLOOP.
*--------------------------------------------------------------------*
  lt_alv = CORRESPONDING #( lt_ ).

  "sort lt_alv BY r1b DESCENDING.
  SORT lt_alv BY r1b DESCENDING.

  LOOP AT lt_alv REFERENCE INTO DATA(lr_alv).
    lr_alv->n = sy-tabix.
  ENDLOOP.

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

      " Set column properties
*vf
*q1b
*q8b
*rf
*r1b
*r8b
*      lo_columns->get_column( 'Q1B' )->set_technical( 'X' ).
*      lo_columns->get_column( 'Q8B' )->set_technical( 'X' ).
*      lo_columns->get_column( 'VF' )->set_technical( 'X' ).
*      lo_columns->get_column( 'RF' )->set_long_text( 'Fl.P Rank' ).
*      lo_columns->get_column( 'R1B' )->set_long_text( '1bit Q rank' ).
*      lo_columns->get_column( 'R8B' )->set_long_text( '16bit Q rank' ).

      " Additional settings like layout, variant, filter, sort can be set here

      " Display the ALV
      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv_msg).
      " Handle exceptions
  ENDTRY.



*    APPEND INITIAL LINE TO lt_ REFERENCE INTO DATA(lr_).
*    DO 1536 TIMES.
*      APPEND INITIAL LINE TO lr_->vf REFERENCE INTO DATA(lr_v).
*      lr_v->* = lo_random_f->get_next( ).
*      APPEND INITIAL LINE TO lr_->q8b REFERENCE INTO DATA(lr_q8b).
*      lr_q8b->* = lr_v->* * 65535.
*    ENDDO.
*    lr_->q1b = lo_l->quantize( lr_->vf  ).
*  ENDDO.



ENDFORM.
