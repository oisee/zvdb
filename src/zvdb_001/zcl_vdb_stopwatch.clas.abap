class ZCL_VDB_STOPWATCH definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF ts_,
             i     TYPE i,
             d     TYPE i,
             start TYPE i,
             end   TYPE i,
             p     TYPE string,
           END OF ts_ .
  types:
    tt_ TYPE STANDARD TABLE OF ts_ WITH KEY i .
  types:
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

  data MT_ type TT_ .

  class-methods NEW
    returning
      value(RO_) type ref to ZCL_VDB_STOPWATCH .
  methods NEXT
    importing
      !IV_ type STRING optional .
  methods RESET .
  methods GET_LOG
    returning
      value(RT_) type TT_ .
  methods GET_STATS
    returning
      value(RS_) type TS_STATS .
  methods GET_STATS_FOR_BATCH
    importing
      !IV_ type I
    returning
      value(RS_) type TS_STATS .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_start TYPE i.
    DATA: mv_end   TYPE i.
    DATA: mv_i TYPE i VALUE 1.
    data: mv_batch_sum TYPE i.
ENDCLASS.



CLASS ZCL_VDB_STOPWATCH IMPLEMENTATION.


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
    ro_ = NEW zcl_vdb_stopwatch( ).
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
