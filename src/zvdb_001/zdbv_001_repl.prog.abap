*&---------------------------------------------------------------------*
*& Report zdbv_001_repl
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdbv_001_repl.

PARAMETERS: p_bid_fr TYPE zvdb_001_vector-bid.
PARAMETERS: p_bid_to TYPE zvdb_001_vector-bid.
PARAMETERS: p_times  TYPE i.

START-OF-SELECTION.

  IF p_bid_to IS NOT INITIAL.
    DELETE FROM zvdb_001_vector WHERE bid = p_bid_to.
    COMMIT WORK AND WAIT.
  ENDIF.


  DATA: lt_f TYPE STANDARD TABLE OF zvdb_001_vector.
  DATA: lt_t TYPE STANDARD TABLE OF zvdb_001_vector.

  SELECT *
    FROM zvdb_001_vector
    INTO CORRESPONDING FIELDS OF TABLE @lt_f
    WHERE bid = @p_bid_fr.

*  SORT lt_f BY p.
*  DELETE ADJACENT DUPLICATES FROM lt_f COMPARING p.

  IF lt_f IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lo_l) = zcl_vdb_001_lib=>new( p_bid_to ).

  DO p_times TIMES.
    APPEND LINES OF lt_f TO lt_t.
  ENDDO.

  LOOP AT lt_t REFERENCE INTO DATA(lr_).
    IF lr_->q1b IS INITIAL OR condense( lr_->p ) = ''.
      lr_->bid = '@@@'.
      CONTINUE.
    ENDIF.
    lr_->bid = p_bid_to.
    lr_->id = lo_l->guid( ).
  ENDLOOP.

  DELETE lt_t WHERE bid = '@@@'.
*  SORT lt_t BY p.
*  DELETE ADJACENT DUPLICATES FROM lt_t COMPARING p.

  MODIFY zvdb_001_vector FROM TABLE lt_t.

  COMMIT WORK AND WAIT.
