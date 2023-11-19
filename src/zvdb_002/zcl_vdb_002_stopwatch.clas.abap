CLASS zcl_vdb_002_stopwatch DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_,
        i     TYPE i,
        d     TYPE i,
        start TYPE i,
        end   TYPE i,
        p     TYPE string,
      END OF ts_ .
    TYPES:
      tt_ TYPE STANDARD TABLE OF ts_ WITH KEY i .
    TYPES:
      BEGIN OF ts_stats,
        average   TYPE i,
        average_f TYPE f,
        median    TYPE i,
        max       TYPE i,
        min       TYPE i,
        total     TYPE i,
        lines     TYPE i,
        batch     TYPE i,
        seconds   TYPE i,
        seconds_f TYPE f,
      END OF ts_stats .

    DATA mt_ TYPE tt_ .

    CLASS-METHODS new
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_vdb_002_stopwatch .
    METHODS next
      IMPORTING
        !iv_ TYPE string OPTIONAL .
    METHODS reset .
    METHODS get_log
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS get_stats
      RETURNING
        VALUE(rs_) TYPE ts_stats .
    METHODS get_stats_for_batch
      IMPORTING
        !iv_       TYPE i
      RETURNING
        VALUE(rs_) TYPE ts_stats .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_start TYPE i.
    DATA: mv_end   TYPE i.
    DATA: mv_i TYPE i VALUE 1.
    DATA: mv_batch_sum TYPE i.
ENDCLASS.



CLASS ZCL_VDB_002_STOPWATCH IMPLEMENTATION.


  METHOD get_log.
    rt_ = mt_.
  ENDMETHOD.


  METHOD get_stats.
    IF mt_ IS INITIAL.
      RETURN.
    ENDIF.
    rs_-lines = lines( mt_ ).
    DATA(lt_) = mt_.
    SORT lt_ BY d ASCENDING.
    rs_-median = VALUE #( lt_[ ceil( rs_-lines / 2 ) ]-d OPTIONAL ).
    rs_-min = rs_-median.
    rs_-max = rs_-median.
    LOOP AT mt_ REFERENCE INTO DATA(lr_).
      ADD lr_->d TO rs_-total.
      IF  rs_-min > lr_->d.
        rs_-min = lr_->d.
      ENDIF.
      IF  rs_-max < lr_->d.
        rs_-max = lr_->d.
      ENDIF.
    ENDLOOP.
    rs_-average_f = rs_-total / rs_-lines.
    rs_-average = rs_-total / rs_-lines.
    rs_-seconds_f = rs_-average_f / 1000000.
    rs_-seconds = rs_-average_f / 1000000.
  ENDMETHOD.


  METHOD get_stats_for_batch.
    IF mt_ IS INITIAL OR
       iv_ = 0.
      RETURN.
    ENDIF.
    rs_-lines = lines( mt_ ).
    rs_-batch = iv_.
    DATA(lt_) = mt_.
    SORT lt_ BY d ASCENDING.
    rs_-median = VALUE #( lt_[ ceil( rs_-lines / 2 ) ]-d OPTIONAL ).
    rs_-min = rs_-median.
    rs_-max = rs_-median.
    LOOP AT mt_ REFERENCE INTO DATA(lr_).
      ADD lr_->d TO rs_-total.
      IF  rs_-min > lr_->d .
        rs_-min = lr_->d .
      ENDIF.
      IF  rs_-max < lr_->d.
        rs_-max = lr_->d.
      ENDIF.
    ENDLOOP.
    rs_-average_f = rs_-total / ( rs_-lines * iv_ ).
    rs_-average = rs_-total / ( rs_-lines * iv_ ).
    rs_-seconds_f = rs_-average_f / 1000000.
    rs_-seconds = rs_-average_f / 1000000.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_vdb_002_stopwatch( ).
    ro_->reset( ).
  ENDMETHOD.


  METHOD next.
    GET RUN TIME FIELD mv_end.
    APPEND VALUE #( i     = mv_i
                    start = mv_start
                    end   = mv_end
                    d     = ( mv_end - mv_start )
                    p     = iv_
    ) TO mt_.
    ADD 1 TO mv_i.
    mv_start = mv_end.
  ENDMETHOD.


  METHOD reset.
    GET RUN TIME FIELD mv_start.
  ENDMETHOD.
ENDCLASS.
