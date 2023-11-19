*&---------------------------------------------------------------------*
*& Report ZVDB_001_EMBED_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvdb_001_embed_01.

TABLES: sscrfields.

*PARAMETERS: p_go AS CHECKBOX.
PARAMETERS: p_bid TYPE zvdb_001_vector-bid.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
*SELECTION-SCREEN FUNCTION KEY 4.

INITIALIZATION.
  sscrfields-functxt_01 = TEXT-f01.
  sscrfields-functxt_02 = TEXT-f02.
  sscrfields-functxt_03 = TEXT-f03.
*  sscrfields-functxt_03 = TEXT-f04.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM go.
    WHEN 'FC01'.
      PERFORM reembed.
    WHEN 'FC02'.
      PERFORM reindex.
    WHEN 'FC03'.
      PERFORM rehash.
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

  DATA(lo_e) = zcl_vdb_001_embedding=>new( p_bid ).
  "q = lo_e->embed_and_save( lv_text )-id.
  COMMIT WORK AND WAIT.

ENDFORM.

FORM reembed.
  DATA(lo_e) = zcl_vdb_001_embedding=>new( p_bid ).

  SELECT *
    FROM zvdb_001_vector
    INTO TABLE @DATA(lt_v)
    WHERE bid = @p_bid AND
          id IS NOT NULL.

  DATA(lo_progress) = zcl_col_000_progress=>new( iv_total = lines( lt_v ) iv_step  = 100
  ).

  LOOP AT lt_v REFERENCE INTO DATA(lr_v).
    lo_progress->next( ).
    lr_v->q1b = lo_e->embed( lr_v->p ).
  ENDLOOP.

  MODIFY zvdb_001_vector FROM TABLE lt_v.
  COMMIT WORK AND WAIT.

ENDFORM.
FORM reindex.
  DATA:lx_ TYPE zcl_vdb_001_lib=>tv.
  DELETE FROM zvdb_001_vector WHERE q1b = lx_.
  COMMIT WORK AND WAIT.
  DATA(lo_l) = zcl_vdb_001_lib=>new( p_bid ).
  DATA(lo_s) = zcl_vdb_stopwatch=>new( ).
  lo_s->reset( ).
  DO 5 TIMES.
    lo_l->reindex_all( iv_ = p_bid
                       is_ = VALUE #( i1 = ' ' i2 = ' '
                                      i3 = ' ' i4 = ' '
                                      i5 = ' ' ) ).
    lo_s->next( ).
  ENDDO.
  COMMIT WORK AND WAIT.
  cl_demo_output=>display( lo_s->get_stats( ) ).
ENDFORM.
FORM rehash.
  DATA(lo_l) = zcl_vdb_001_lib=>new( p_bid ).

  DATA(lo_s) = zcl_vdb_stopwatch=>new( ).
  lo_l->regen_recal_hash_and_reindex( p_bid ).
" lo_l->reindex_all( p_bid ).
  COMMIT WORK AND WAIT.
  lo_s->next( ).
  DATA(ls_) = lo_s->get_stats( ).

*  SELECT COUNT( id ) as c
*    FROM zvdb_001_vector
*    INTO @DATA(lv_c)
*    WHERE pid = @p_id.
*  DATA(ls_b) = lo_s->get_stats_for_batch( ).

  cl_demo_output=>write( ls_ ).
  cl_demo_output=>display( ).


ENDFORM.
