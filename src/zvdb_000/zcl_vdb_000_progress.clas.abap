class ZCL_VDB_000_PROGRESS definition
  public
  final
  create public .

public section.

  data MV_TOTAL type I .
  data MV_COUNTER type I .
  data MV_STEP type I .
  data MV_TEXT type STRING .
  constants MC_MAX_STEP type I value 500 ##NO_TEXT.
  constants MC_DEFAULT_STEP type I value 20 ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_TOTAL type I
      !IV_STEP type I default 500
      !IV_TEXT type STRING default '' .
  class-methods NEW
    importing
      !IV_TOTAL type I
      !IV_STEP type I default 500
      !IV_TEXT type STRING default ''
    returning
      value(RO_) type ref to ZCL_VDB_000_PROGRESS .
  class-methods NEW_FOR_TABLE
    importing
      !IT_ type ANY TABLE
      !IV_TEXT type STRING optional
    returning
      value(RO_) type ref to ZCL_VDB_000_PROGRESS .
  methods NEXT
    exporting
      !EV_SHOWN type SAP_BOOL
    returning
      value(RV_) type I .
  PROTECTED SECTION.
private section.

  data MV_INDEX type I .
  data MV_TIMES type I .
  data MV_SLICE_SIZE type I .
  data MV_LAST_SLICE type I .
ENDCLASS.



CLASS ZCL_VDB_000_PROGRESS IMPLEMENTATION.


  METHOD constructor.

    mv_text  = iv_text.
    mv_total = iv_total.
    mv_step  = COND #( WHEN iv_step = 0           THEN 1
                       WHEN iv_step > mc_max_step THEN mc_max_step
                       ELSE iv_step
    ).

  ENDMETHOD.


  METHOD new.
    ro_ = NEW #(
      iv_total = iv_total
      iv_step  = iv_step
      iv_text  = iv_text
    ).
  ENDMETHOD.


  METHOD new_for_table.

    DATA: lo_           TYPE REF TO cl_abap_typedescr,
          lv_head   TYPE string,
          lv_tail   TYPE string.

* Create a new instance of CL_ABAP_TABLEDESCR for the table you want to get the type name of
    lo_ = cl_abap_typedescr=>describe_by_data( it_ ).

* Get the type name of the table
    SPLIT lo_->absolute_name AT '=' INTO lv_head lv_tail. "\TYPE=MARA_TT"

    ro_ = NEW #(
      iv_total = lines( it_ )
      iv_step  = ( lines( it_ ) / mc_default_step ) " iv_total / 20 => update progress every 5%
      iv_text  = COND string( WHEN iv_text IS INITIAL THEN lv_tail
                              ELSE iv_text )
    ).

  ENDMETHOD.


  METHOD next.
    clear ev_shown.
    DATA(lv_mod) = mv_counter MOD ( 1 + mv_total / mv_step ) .
    ADD 1 TO mv_counter.
    IF lv_mod = 0.
      cl_progress_indicator=>progress_indicate(
      i_text      = |{ mv_text }:{ mv_counter }/{ mv_total }|
      i_total     = mv_total
      i_processed = mv_counter
      i_output_immediately = abap_true ).
      ev_shown = 'X'.
    ENDIF.

    rv_ = mv_counter.
  ENDMETHOD.
ENDCLASS.
