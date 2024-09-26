CLASS zcl_vdb_002_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_index_bypass,
        i1 TYPE sap_bool, "heuristic hyperplane 8
        i2 TYPE sap_bool, "heuristic hyperplane 16
        i3 TYPE sap_bool, "random hyperplane 8
        i4 TYPE sap_bool, "random hyperplane 16
        i5 TYPE sap_bool, "fast fold 16
      END OF ts_index_bypass .
    TYPES ts_dp TYPE i .
    TYPES:
      tt_dp TYPE STANDARD TABLE OF ts_dp WITH DEFAULT KEY .
    TYPES ts_set TYPE zvdb_002_set .
    TYPES:
      tt_set TYPE STANDARD TABLE OF ts_set WITH KEY id .
    TYPES:
      ty_coordinate TYPE p LENGTH 10 DECIMALS 9 .     "ts_embedding TYPE zif_peng_azoai_sdk_typinternal=>ty_coordinate .
    TYPES:
      BEGIN OF ty_embedding,
        object    TYPE string,
        index     TYPE i,
        embedding TYPE STANDARD TABLE OF ty_coordinate WITH DEFAULT KEY,
      END OF ty_embedding .
    TYPES tt_embedding TYPE ty_embedding-embedding .
    TYPES ts_vector TYPE zvdb_002_vector .
    TYPES:
      tt_vector TYPE STANDARD TABLE OF ts_vector WITH KEY id .
    TYPES:
      ty_openai_embedding_vector(192)     TYPE x .
    TYPES tv TYPE ty_openai_embedding_vector .              "b1536
    TYPES:
      ty_x256(256) TYPE x .
    TYPES:
      ty_x65536(65536) TYPE x .
    TYPES:
      ty_b8(1)  TYPE x .
    TYPES:
      ty_b16(2) TYPE x .
    TYPES:
      ty_b24(3) TYPE x .
    TYPES:
      ty_b48(6) TYPE x .
    TYPES:
      ty_12(12) TYPE x .
    TYPES:
      ty_24(24) TYPE x .
    TYPES:
      ty_48(48) TYPE x .
    TYPES:
      tt_b8  TYPE STANDARD TABLE OF ty_b8  WITH DEFAULT KEY .
    TYPES:
      tt_b16 TYPE STANDARD TABLE OF ty_b16 WITH DEFAULT KEY .
    TYPES:
      tt_b24 TYPE STANDARD TABLE OF ty_b24 WITH DEFAULT KEY .
    TYPES:
      ttr_8  TYPE RANGE OF ty_b8 .          "for hamming_8
    TYPES:
      ttr_16 TYPE RANGE OF ty_b16 .         "for hamming_16
    TYPES:
      ttr_24 TYPE RANGE OF ty_b24 .         "for hamming_24
    TYPES:
      BEGIN OF ts_hh_hr,
        hh TYPE ty_b24,
        hr TYPE ty_b24,
      END OF ts_hh_hr .
    TYPES:
      BEGIN OF ts_hash_fast_fold,
             hff2  TYPE ty_b16,
             hff3  TYPE ty_b24,
             hff6  TYPE ty_b48,
             hff12 TYPE ty_12,
             hff24 TYPE ty_24,
             hff48 TYPE ty_48,
      END OF ts_hash_fast_fold .
    TYPES:
      BEGIN OF ts_hash,
        hh1 TYPE zvdb_002_vector-hh1,
        hh2 TYPE zvdb_002_vector-hh2,
        hr1 TYPE zvdb_002_vector-hr1,
        hr2 TYPE zvdb_002_vector-hr2,
*       hff2 TYPE ty_b16,
*       hff3 TYPE ty_b24,
*       hff6 TYPE ty_b48,
      END OF ts_hash .
    TYPES:
*    TYPES:
*      BEGIN OF ts_query,
*        id   TYPE zvdb_002_vector-id,
*        rank TYPE i,
*        xor  TYPE i,
*        "       jc          TYPE f,
*        "       or_and_diff TYPE i, " or - rank = xor -> just use xor
*        p    TYPE zvdb_002_vector-p,
*      END OF ts_query .
*    TYPES:
*      tt_query TYPE STANDARD TABLE OF ts_query WITH KEY id .
*    TYPES:
*      BEGIN OF ts_q,
*        id   TYPE zvdb_002_vector-id,
*        rank TYPE i,
*        q1b  TYPE zvdb_002_vector-q1b,
*        ip   TYPE zvdb_002_vector-ip,
*      END OF ts_q .
*    TYPES:
*      tt_q TYPE STANDARD TABLE OF ts_q WITH KEY id .
      BEGIN OF ts_v,
        id  TYPE zvdb_002_vector-id,
        q1b TYPE zvdb_002_vector-q1b,
      END OF ts_v .
    TYPES:
      tt_v TYPE HASHED TABLE OF ts_v WITH UNIQUE KEY id .
    TYPES:
      ttr_tv TYPE RANGE OF tv .

    DATA mv_hp_start TYPE i .
    DATA mv_hp_end TYPE i .
    DATA mv_hr_start TYPE i .
    DATA mv_hr_end TYPE i .
    DATA ms_ib TYPE ts_index_bypass .
    DATA mv_bid TYPE zvdb_002_vector-bid .
    CLASS-DATA gt_dp_8 TYPE tt_dp .
    CLASS-DATA gt_dp_16 TYPE tt_dp .
    CLASS-DATA gt_dp_u16 TYPE tt_dp .
    CLASS-DATA g TYPE tt_dp .
    DATA mt_set TYPE tt_set .
    DATA mv_rank_h TYPE int2 .
    DATA mv_rank_r TYPE int2 .
    CLASS-DATA gx8 TYPE ty_x256 .
    CLASS-DATA gx16 TYPE ty_x65536 .
    CLASS-DATA gt_hamming_8_d1 TYPE tt_b8 .       " 8  of 256 - 32.0 groups
    CLASS-DATA gt_hamming_8_d2 TYPE tt_b8 .       " 36 of 256 - ~7.1 groups
*   CLASS-DATA gt_hamming_8_d3 TYPE tt_b8.        " 92 of 256 - ~2.7 groups - groups are too big, does not make sense.
    CLASS-DATA gt_hamming_8 TYPE tt_b8 .          "d2
    CLASS-DATA gt_hamming_16_d1 TYPE tt_b16 .     "   16 of 65536 - 4096.0 groups
    CLASS-DATA gt_hamming_16_d2 TYPE tt_b16 .     "  136 of 65536 - ~481.9 groups
    CLASS-DATA gt_hamming_16_d3 TYPE tt_b16 .     "  696 of 65536 -  ~94.2 groups
*   CLASS-DATA gt_hamming_16_d4 TYPE tt_b16 .     " 2516 of 65536 -  ~26.1 groups "groups are too big, does not make sense
    CLASS-DATA gt_hamming_16    TYPE tt_b16 .        "d3 (?)
    CLASS-DATA gt_hamming_24_d1 TYPE tt_b24 .     "   16 of 65536 - 4096.0 groups
    CLASS-DATA gt_hamming_24_d2 TYPE tt_b24 .     "  136 of 65536 - ~481.9 groups
    CLASS-DATA gt_hamming_24    TYPE tt_b24 .        "d3 (?)
    CLASS-DATA go_random_int TYPE REF TO cl_abap_random_int .
    CLASS-DATA go_random_f TYPE REF TO cl_abap_random_float .
    DATA mv_ignore_settings TYPE sap_bool .
    DATA mt_v TYPE tt_v .
    DATA mt_hh TYPE tt_vector .
    DATA mt_hr TYPE tt_vector .
*    DATA mt_hp TYPE tt_q .
*    DATA mt_hash_log TYPE tt_vector .
    DATA mt_hash_log_h TYPE tt_vector .
    DATA mt_hash_log_r TYPE tt_vector .

    CLASS-METHODS class_constructor .
    CLASS-METHODS guid
      RETURNING
        VALUE(rv_) TYPE guid .
    CLASS-METHODS init_bit_counter_tab_16bit
      RETURNING
        VALUE(rx_) TYPE ty_x65536 .
    CLASS-METHODS init_bit_counter_tab_8bit
      RETURNING
        VALUE(rx_) TYPE ty_x256 .
    CLASS-METHODS init_hamming_24bit
      RETURNING
        VALUE(rt_) TYPE tt_b24 .
    CLASS-METHODS init_hamming_24bit_d1
      RETURNING
        VALUE(rt_) TYPE tt_b24 .
    CLASS-METHODS init_hamming_24bit_d2
      RETURNING
        VALUE(rt_) TYPE tt_b24 .
    CLASS-METHODS init_hamming_24bit_d3
      RETURNING
        VALUE(rt_) TYPE tt_b24 .
    CLASS-METHODS init_hamming_24bit_d4
      RETURNING
        VALUE(rt_) TYPE tt_b24 .
    CLASS-METHODS init_hamming_16bit
      RETURNING
        VALUE(rt_) TYPE tt_b16 .
    CLASS-METHODS init_hamming_16bit_d1
      RETURNING
        VALUE(rt_) TYPE tt_b16 .
    CLASS-METHODS init_hamming_16bit_d2
      RETURNING
        VALUE(rt_) TYPE tt_b16 .
    CLASS-METHODS init_hamming_16bit_d3
      RETURNING
        VALUE(rt_) TYPE tt_b16 .
    CLASS-METHODS init_hamming_16bit_d4
      RETURNING
        VALUE(rt_) TYPE tt_b16 .
    CLASS-METHODS init_hamming_8bit
      RETURNING
        VALUE(rt_) TYPE tt_b16 .
    CLASS-METHODS init_hamming_8bit_d1
      RETURNING
        VALUE(rt_) TYPE tt_b8 .
    CLASS-METHODS init_hamming_8bit_d2
      RETURNING
        VALUE(rt_) TYPE tt_b8 .
    CLASS-METHODS init_hamming_8bit_d3
      RETURNING
        VALUE(rt_) TYPE tt_b8 .
    CLASS-METHODS init_signed_counter_tab_16bit
      RETURNING
        VALUE(rt_) TYPE tt_dp .
    CLASS-METHODS init_signed_counter_tab_8bit
      RETURNING
        VALUE(rt_) TYPE tt_dp .
    CLASS-METHODS init_usigned_counter_tab_16bit
      RETURNING
        VALUE(rt_) TYPE tt_dp .
    CLASS-METHODS new
      IMPORTING
        !iv_       TYPE zvdb_002_vector-bid
        !is_       TYPE ts_index_bypass OPTIONAL
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_vdb_002_lib .
    METHODS bf
      IMPORTING
        !ix_       TYPE tv
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS bf_far
      IMPORTING
        !ix_       TYPE tv
        !itr_      TYPE ttr_tv OPTIONAL
        !it_bf     TYPE tt_vector
      RETURNING
        VALUE(rs_) TYPE ts_vector .
    METHODS bf_far_mid
      IMPORTING
        !ix_       TYPE tv
        !itr_      TYPE ttr_tv OPTIONAL
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS bf_in_tt
      IMPORTING
        !ix_ TYPE tv
      CHANGING
        !ct_ TYPE tt_vector .
    METHODS bf_mid
      IMPORTING
        !ix_       TYPE tv
        !itr_      TYPE ttr_tv OPTIONAL
        !it_bf     TYPE tt_vector
      RETURNING
        VALUE(rs_) TYPE ts_vector .
    METHODS bf_mid_far
      IMPORTING
        !ix_       TYPE tv
        !itr_      TYPE ttr_tv OPTIONAL
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS bf_vs_tt
      IMPORTING
        !ix_       TYPE tv
        !it_       TYPE tt_vector
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS cache_samples .
    METHODS clear_cache .
    METHODS clear_hash_log .
    METHODS constructor
      IMPORTING
        !iv_ TYPE zvdb_002_vector-bid
        !is_ TYPE ts_index_bypass OPTIONAL
          PREFERRED PARAMETER iv_ .
    METHODS create_vector
      IMPORTING
        !ix_        TYPE tv
        !iv_payload TYPE string
      RETURNING
        VALUE(rs_)  TYPE ts_vector .
    METHODS bit_count
      IMPORTING
        !ix_0      TYPE xsequence
        !ix_1      TYPE xsequence
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS dp
      IMPORTING
        !ix_0      TYPE xsequence
        !ix_1      TYPE xsequence
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS dp_1536
      IMPORTING
        !ix_0      TYPE zvdb_002e_raw192
        !ix_1      TYPE zvdb_002e_raw192
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS dp_24
      IMPORTING
        !ix_0      TYPE ty_b24
        !ix_1      TYPE ty_b24
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS dp_16
      IMPORTING
        !ix_0      TYPE ty_b16
        !ix_1      TYPE ty_b16
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS dp_8
      IMPORTING
        !ix_0      TYPE ty_b8
        !ix_1      TYPE ty_b8
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS get_hh
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS get_hr
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS get_index_bypass
      RETURNING
        VALUE(rs_) TYPE ts_index_bypass .
    METHODS get_set
      RETURNING
        VALUE(rt_) TYPE tt_set .
    METHODS hamming_24
      IMPORTING
        !ix_        TYPE ty_b24
      RETURNING
        VALUE(rtr_) TYPE ttr_24 .
    METHODS hamming_16
      IMPORTING
        !ix_        TYPE ty_b16
      RETURNING
        VALUE(rtr_) TYPE ttr_16 .
    METHODS hamming_8
      IMPORTING
        !ix_        TYPE ty_b8
      RETURNING
        VALUE(rtr_) TYPE ttr_8 .

    METHODS hash_fast_fold
      IMPORTING
        !ix_       TYPE tv
      RETURNING
        VALUE(rs_) TYPE ts_hash_fast_fold .
    METHODS hash
      IMPORTING
        !ix_       TYPE tv
      RETURNING
        VALUE(rs_) TYPE ts_hh_hr .
    METHODS hash_s
      IMPORTING
        !ix_       TYPE tv
      RETURNING
        VALUE(rs_) TYPE ts_hash .
    METHODS init_bake_hp .
    METHODS init_save_hp .
    METHODS qf_in_tt
      IMPORTING
        !is_ TYPE ts_vector
      CHANGING
        !ct_ TYPE tt_vector .
    METHODS qquery
      IMPORTING
        !is_       TYPE ts_vector
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS qquery_by_id
      IMPORTING
        !iv_       TYPE ts_vector-id
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS quantize
      IMPORTING
        !it_       TYPE tt_embedding
      RETURNING
        VALUE(rx_) TYPE tv .
    METHODS query
      IMPORTING
        !ix_       TYPE ts_vector-q1b
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS query_by_id
      IMPORTING
        !iv_       TYPE ts_vector-id
      RETURNING
        VALUE(rt_) TYPE tt_vector .
    METHODS random_v
      RETURNING
        VALUE(rx_) TYPE tv .
    METHODS random_vector
      IMPORTING
        !iv_       TYPE ts_vector-ip DEFAULT '0'
      RETURNING
        VALUE(rs_) TYPE ts_vector .
    METHODS rank_all_to_autocalibrate_a
      IMPORTING
        !iv_       TYPE f DEFAULT '0.6'
      RETURNING
        VALUE(rt_) TYPE tt_set .
    METHODS rank_all_to_autocalibrate_m
      IMPORTING
        !iv_       TYPE f DEFAULT '0.6'
      RETURNING
        VALUE(rt_) TYPE tt_set .
    METHODS read_vector
      IMPORTING
        !iv_       TYPE zvdb_002_vector-id
      RETURNING
        VALUE(rs_) TYPE zvdb_002_vector .
    METHODS recalibrate_hash
      IMPORTING
        !iv_         TYPE string DEFAULT 'MEDIUM'
        !iv_treshold TYPE f DEFAULT '0.6'
      RETURNING
        VALUE(rt_)   TYPE tt_set .
    METHODS recalibrate_hash_and_reindex
      IMPORTING
        !iv_         TYPE string DEFAULT 'MEDIUM'
        !iv_treshold TYPE f DEFAULT '0.6'
        !iv_bid      TYPE zvdb_002_vector-bid
        !is_         TYPE ts_index_bypass OPTIONAL
      RETURNING
        VALUE(rt_)   TYPE tt_set .
    METHODS regen_hash .
    METHODS regen_recal_hash_and_reindex
      IMPORTING
        !iv_         TYPE string DEFAULT 'MEDIUM'
        !iv_treshold TYPE f DEFAULT '0.6'
        !iv_bid      TYPE zvdb_002_vector-bid
        !is_         TYPE ts_index_bypass OPTIONAL
      RETURNING
        VALUE(rt_)   TYPE tt_set .
    METHODS reindex_all
      IMPORTING
        !iv_ TYPE zvdb_002_vector-bid
        !is_ TYPE ts_index_bypass OPTIONAL
          PREFERRED PARAMETER iv_ .
    METHODS reset_hp .
    METHODS save_vector
      IMPORTING
        !is_    TYPE ts_vector
        !iv_bid TYPE ts_vector-bid OPTIONAL
          PREFERRED PARAMETER is_ .
    METHODS ttv_to_range
      IMPORTING
        !it_        TYPE tt_vector
      RETURNING
        VALUE(rtr_) TYPE ttr_tv .
    METHODS ttv_to_range_exclude
      IMPORTING
        !it_        TYPE tt_vector
      RETURNING
        VALUE(rtr_) TYPE ttr_tv .
    METHODS quantize_from_string
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rx_) TYPE tv .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      gx_8_rra(8) TYPE x VALUE '8040201008040201' ##NO_TEXT.            "8 bit rotation right look up table
    CONSTANTS:
      gx_16_rra(32) TYPE x VALUE '8000400020001000080004000200010000800040002000100008000400020001' ##NO_TEXT.          "16 bit rotation right look up table

    METHODS dp_1536_bak
      IMPORTING
        !ix_0      TYPE zvdb_002e_raw192
        !ix_1      TYPE zvdb_002e_raw192
      RETURNING
        VALUE(rv_) TYPE i .
*    CONSTANTS:
*      gx_24_rra(72) TYPE x VALUE '800000400000200000100000080000040000020000010000008000004000002000001000000800000400000200000100000080000040000020000010000008000004000002000001'. "24 bit rotation right look up table
    CLASS-METHODS init_class .
    METHODS init_instance
      IMPORTING
        !iv_ TYPE zvdb_002_vector-bid
        !is_ TYPE ts_index_bypass OPTIONAL
          PREFERRED PARAMETER iv_ .
ENDCLASS.



CLASS ZCL_VDB_002_LIB IMPLEMENTATION.


  METHOD bf.
    cache_samples( ).
    LOOP AT mt_v REFERENCE INTO DATA(lr_v).
      APPEND INITIAL LINE TO rt_ REFERENCE INTO DATA(lr_).
      lr_->id   = lr_v->id.
      lr_->rank = dp_1536( ix_0 = ix_
                               ix_1 = lr_v->q1b
                  ).
      lr_->q1b = lr_v->q1b.
    ENDLOOP.
    SORT rt_ BY rank DESCENDING. "dot product value in range +1536 (vectors are equal) to -1536 (opposite)
  ENDMETHOD.


  METHOD bf_far.
    DATA(lv_len) = lines( it_bf ).
    rs_ = it_bf[ lv_len ].
    IF rs_-q1b IN itr_ AND
       rs_-q1b NE ix_.
      RETURN.
    ENDIF.
    " fetching from furthest to closest
    DATA(lv_i) = lv_len.
    DO lv_len TIMES.
      rs_ = it_bf[ lv_i ].
      IF rs_-q1b IN itr_ AND
         rs_-q1b NE ix_.
        RETURN.
      ENDIF.
      SUBTRACT 1 FROM lv_i.
    ENDDO.
    " ok... then just any random...
    DATA(lv_r) = CONV i( go_random_f->get_next( ) * lv_len ).
    rs_ =  it_bf[ lv_r ].
  ENDMETHOD.


  METHOD bf_far_mid.
    DATA(lt_bf) = bf( ix_ ).
    DATA(ltr_) = itr_.
    APPEND bf_far( ix_ = ix_ it_bf = lt_bf itr_ = ltr_ ) TO rt_.
    APPEND LINES OF ttv_to_range_exclude( rt_ ) TO ltr_.
    APPEND bf_mid( ix_ = ix_ it_bf = lt_bf itr_ = ltr_ ) TO rt_.
  ENDMETHOD.


  METHOD bf_in_tt.
    LOOP AT ct_ REFERENCE INTO DATA(lr_).
      lr_->rank = dp_1536( ix_0 = ix_
                           ix_1 = lr_->q1b
                  ).
    ENDLOOP.
    SORT ct_ BY rank DESCENDING. "dot product value in range +1536 (vectors are equal) to -1536 (opposite)
  ENDMETHOD.


  METHOD bf_mid.
    DATA(lv_mid) = ceil( lines( it_bf ) / 2 ).
    rs_ = it_bf[ lv_mid ] . "mid
    IF rs_-q1b IN itr_ AND
       rs_-q1b NE ix_.
      RETURN.
    ENDIF.
    " fetching from middle -2 to furthest
    DATA(lv_i) = COND i( WHEN lv_mid > 2 THEN lv_mid - 2
                         WHEN lv_mid > 1 THEN lv_mid - 1
                         ELSE lv_mid
                 ).
    DO lv_mid TIMES.
      rs_ = it_bf[ lv_i ].
      IF rs_-q1b IN itr_ AND
         rs_-q1b NE ix_.
        RETURN.
      ENDIF.
      ADD 1 TO lv_i.
    ENDDO.
    lv_i = 1.
    DO lv_mid TIMES.
      rs_ = it_bf[ lv_i ].
      IF rs_-q1b IN itr_ AND
         rs_-q1b NE ix_.
        RETURN.
      ENDIF.
      ADD 1 TO lv_i.
    ENDDO.
    " ok... then just any random...
    DATA(lv_len) = lines( it_bf ).
    DATA(lv_r) = go_random_f->get_next( ) * lv_len.
    rs_ =  it_bf[ lv_r ].
  ENDMETHOD.


  METHOD bf_mid_far.
    DATA(lt_bf) = bf( ix_ ).
    DATA(ltr_) = itr_.
    APPEND bf_mid( ix_ = ix_ it_bf = lt_bf itr_ = ltr_ ) TO rt_.
    APPEND LINES OF ttv_to_range_exclude( rt_ ) TO ltr_.
    APPEND bf_far( ix_ = ix_ it_bf = lt_bf itr_ = ltr_ ) TO rt_.
  ENDMETHOD.


  METHOD bf_vs_tt.
    rt_ = it_.
    LOOP AT rt_ REFERENCE INTO DATA(lr_).
      lr_->rank = dp_1536( ix_0 = ix_
                           ix_1 = lr_->q1b
                  ).
    ENDLOOP.
    SORT rt_ BY rank DESCENDING. "dot product value in range +1536 (vectors are equal) to -1536 (opposite)
  ENDMETHOD.


  METHOD bit_count.
    DATA(lx_dp) = ix_0 BIT-XOR ix_1.

    DATA:lv_s TYPE i. "0.
    DATA:lv_i TYPE i. "0.
    DATA:lv_dp TYPE i. "0.
    DATA(lv_len) = xstrlen( ix_0 ).

*using X (8 bit counter), instead of hash table - using binary string as value table
    DO lv_len TIMES.
      lv_dp = lx_dp+lv_i(1).
      lv_s  = gx8+lv_dp(1).
      ADD lv_s TO rv_.
      ADD 1 TO lv_i.
    ENDDO.
  ENDMETHOD.


  METHOD cache_samples.
    IF mt_v IS NOT INITIAL.
      RETURN.
    ENDIF.
    SELECT id, q1b UP TO 5000 ROWS
      FROM zvdb_002_vector
      INTO TABLE @mt_v
      WHERE bid = @mv_bid AND
            id IS NOT NULL
        AND r = ''. "no random planes for sampling
  ENDMETHOD.


  METHOD class_constructor.
    init_class( ).
  ENDMETHOD.


  METHOD clear_cache.
    CLEAR mt_v.
  ENDMETHOD.


  METHOD clear_hash_log.
    CLEAR mt_hash_log_h.
    CLEAR mt_hash_log_r.
  ENDMETHOD.


  METHOD constructor.

    init_instance( iv_ = iv_
                   is_ = is_ ).

  ENDMETHOD.


  METHOD create_vector.
    rs_-bid = mv_bid.
    rs_-id = guid( ).
    rs_-p = iv_payload.
    rs_-q1b = ix_.

    DATA(ls_) = hash_s( ix_ ).

    rs_-hh1 = ls_-hh1.
    rs_-hh2 = ls_-hh2.
    rs_-hr1 = ls_-hr1.
    rs_-hr2 = ls_-hr2.

  ENDMETHOD.


  METHOD dp.
    DATA(lx_dp) = ix_0 BIT-XOR ix_1.

    DATA:lv_s TYPE i. "0.
    DATA:lv_i TYPE i. "0.
    DATA:lv_dp TYPE i. "0.
    DATA(lv_len) = xstrlen( ix_0 ) DIV 2.

*using XX (16 bit counter), instead of hash table - using binary string as value table
    DO lv_len TIMES.
      lv_dp = lx_dp+lv_i(2).
      lv_s  = gt_dp_16[ lv_dp + 1 ]. " gx16+lv_dp(1).
      ADD lv_s TO rv_.
      ADD 2 TO lv_i.
    ENDDO.

    DATA(lv_mod) = xstrlen( ix_0 ) MOD 2.
    IF lv_mod NE 0.
      lv_dp = lx_dp+lv_i(1).
      lv_s  = gt_dp_8[ lv_dp + 1 ].
      ADD lv_s TO rv_.
    ENDIF.

  ENDMETHOD.


  METHOD dp_1536. "one ms faster =)))
    DATA(d) = ix_0 BIT-XOR ix_1.
    rv_ = rv_ + g[ d+01(2) + 1 ] + g[ d+02(2) + 1 ] + g[ d+03(2) + 1 ] + g[ d+04(2) + 1 ] + g[ d+05(2) + 1 ] + g[ d+06(2) + 1 ] + g[ d+07(2) + 1 ] + g[ d+08(2) + 1 ] + g[ d+09(2) + 1 ] + g[ d+10(2) + 1 ] .
    rv_ = rv_ + g[ d+11(2) + 1 ] + g[ d+12(2) + 1 ] + g[ d+13(2) + 1 ] + g[ d+14(2) + 1 ] + g[ d+15(2) + 1 ] + g[ d+16(2) + 1 ] + g[ d+17(2) + 1 ] + g[ d+18(2) + 1 ] + g[ d+19(2) + 1 ] + g[ d+20(2) + 1 ] .
    rv_ = rv_ + g[ d+21(2) + 1 ] + g[ d+22(2) + 1 ] + g[ d+23(2) + 1 ] + g[ d+24(2) + 1 ] + g[ d+25(2) + 1 ] + g[ d+26(2) + 1 ] + g[ d+27(2) + 1 ] + g[ d+28(2) + 1 ] + g[ d+29(2) + 1 ] + g[ d+30(2) + 1 ] .
    rv_ = rv_ + g[ d+31(2) + 1 ] + g[ d+32(2) + 1 ] + g[ d+33(2) + 1 ] + g[ d+34(2) + 1 ] + g[ d+35(2) + 1 ] + g[ d+36(2) + 1 ] + g[ d+37(2) + 1 ] + g[ d+38(2) + 1 ] + g[ d+39(2) + 1 ] + g[ d+40(2) + 1 ] .
    rv_ = rv_ + g[ d+41(2) + 1 ] + g[ d+42(2) + 1 ] + g[ d+43(2) + 1 ] + g[ d+44(2) + 1 ] + g[ d+45(2) + 1 ] + g[ d+46(2) + 1 ] + g[ d+47(2) + 1 ] + g[ d+48(2) + 1 ] + g[ d+49(2) + 1 ] + g[ d+50(2) + 1 ] .
    rv_ = rv_ + g[ d+51(2) + 1 ] + g[ d+52(2) + 1 ] + g[ d+53(2) + 1 ] + g[ d+54(2) + 1 ] + g[ d+55(2) + 1 ] + g[ d+56(2) + 1 ] + g[ d+57(2) + 1 ] + g[ d+58(2) + 1 ] + g[ d+59(2) + 1 ] + g[ d+60(2) + 1 ] .
    rv_ = rv_ + g[ d+61(2) + 1 ] + g[ d+62(2) + 1 ] + g[ d+63(2) + 1 ] + g[ d+64(2) + 1 ] + g[ d+65(2) + 1 ] + g[ d+66(2) + 1 ] + g[ d+67(2) + 1 ] + g[ d+68(2) + 1 ] + g[ d+69(2) + 1 ] + g[ d+70(2) + 1 ] .
    rv_ = rv_ + g[ d+71(2) + 1 ] + g[ d+72(2) + 1 ] + g[ d+73(2) + 1 ] + g[ d+74(2) + 1 ] + g[ d+75(2) + 1 ] + g[ d+76(2) + 1 ] + g[ d+77(2) + 1 ] + g[ d+78(2) + 1 ] + g[ d+79(2) + 1 ] + g[ d+80(2) + 1 ] .
    rv_ = rv_ + g[ d+81(2) + 1 ] + g[ d+82(2) + 1 ] + g[ d+83(2) + 1 ] + g[ d+84(2) + 1 ] + g[ d+85(2) + 1 ] + g[ d+86(2) + 1 ] + g[ d+87(2) + 1 ] + g[ d+88(2) + 1 ] + g[ d+89(2) + 1 ] + g[ d+90(2) + 1 ] .
    rv_ = rv_ + g[ d+91(2) + 1 ] + g[ d+92(2) + 1 ] + g[ d+93(2) + 1 ] + g[ d+94(2) + 1 ] + g[ d+95(2) + 1 ] + g[ d+96(2) + 1 ] .

  ENDMETHOD.


  METHOD dp_1536_bak.
    DATA(lx_dp) = ix_0 BIT-XOR ix_1.
    DATA:lv_s TYPE i. "0.
    DATA:lv_i TYPE i. "0.
    DATA:lv_dp TYPE i. "0.
* using XX (16 bit counter), instead of hash table - using binary string as value table
    DO 96 TIMES.
      lv_dp = lx_dp+lv_i(2).
      lv_s  = gt_dp_16[ lv_dp + 1 ]. " gx16+lv_dp(1).
      ADD lv_s TO rv_.
      ADD 2 TO lv_i.
    ENDDO.

*    DATA:lv_dp1 TYPE i. "0.
*    DATA:lv_dp2 TYPE i. "0.
*    DATA:lv_dp3 TYPE i. "0.
*    DATA:lv_dp4 TYPE i. "0.
**using XX (16 bit counter), instead of hash table - using binary string as value table
*    DO 24 TIMES.
*      lv_dp1 = lx_dp+lv_i(2).
*      ADD 2 TO lv_i.
*      lv_dp2 = lx_dp+lv_i(2).
*      ADD 2 TO lv_i.
*      lv_dp3 = lx_dp+lv_i(2).
*      ADD 2 TO lv_i.
*      lv_dp4 = lx_dp+lv_i(2).
*      ADD 2 TO lv_i.
*      lv_s  = gt_dp_16[ lv_dp1 + 1 ] + gt_dp_16[ lv_dp2 + 1 ] + gt_dp_16[ lv_dp3 + 1 ] + gt_dp_16[ lv_dp4 + 1 ]  . " gx16+lv_dp(1).
*      ADD lv_s TO rv_.
*      "ADD 8 TO lv_i.
*    ENDDO.
*
*    RETURN.


**using XX (16 bit counter), instead of hash table - using binary string as value table
*    DO iv_len TIMES.
*      lv_dp = lx_dp+lv_i(2).
*      lv_s  = gx16+lv_dp(1).
*      ADD lv_s TO rv_.
*      ADD 2 TO lv_i.
*    ENDDO.

**using XX (16 bit counter), instead of hash table - using binary string as value table
*    DO 24 TIMES.
*      lv_dp = lx_dp+lv_i(2).
*      lv_s = gxi2_+lv_dp(1).
*      ADD lv_s TO rv_.
*      ADD 2 TO lv_i.
*      lv_dp = lx_dp+lv_i(2).
*      lv_s = gxi2_+lv_dp(1).
*      ADD lv_s TO rv_.
*      ADD 2 TO lv_i.
*      lv_dp = lx_dp+lv_i(2).
*      lv_s = gxi2_+lv_dp(1).
*      ADD lv_s TO rv_.
*      ADD 2 TO lv_i.
*      lv_dp = lx_dp+lv_i(2).
*      lv_s = gxi2_+lv_dp(1).
*      ADD lv_s TO rv_.
*      ADD 2 TO lv_i.
*    ENDDO.

**using X (8 bit counter), instead of hash table - using binary string as value table
*    DO 192 TIMES.
*      lv_dp = lx_dp+lv_i(1).
*      lv_s = gxi_+lv_dp(1).
*      ADD lv_s TO rv_.
*      ADD 1 TO lv_i.
*    ENDDO.

**using XX (16 bit counter)
*    DO 96 TIMES.
*      lv_i = ( sy-index - 1 ) * 2.
*      lv_s = gt_bc_2[ xx = lx_dp+lv_i(2) ]-s.
*      ADD lv_s TO rv_.
*    ENDDO.
*using X (8 bit counter)
*    DO 192 TIMES.
*      lv_i = sy-index - 1 .
*      lv_s = gt_bc[ x = lx_dp+lv_i(1) ]-s.
*      ADD lv_s TO rv_.
*    ENDDO.

  ENDMETHOD.


  METHOD dp_16.
    DATA(lx_dp) = ix_0 BIT-XOR ix_1.
    rv_ = gt_dp_16[ lx_dp + 1 ].
  ENDMETHOD.


  METHOD dp_24.
    rv_ = dp_16( ix_0 = ix_0+0(2)
                 ix_1 = ix_1+0(2)
                ).
    rv_ = rv_ + dp_8( ix_0 = ix_0+2(1)
                      ix_1 = ix_1+2(1)
                    ).
  ENDMETHOD.


  METHOD dp_8.
    DATA(lx_dp) = ix_0 BIT-XOR ix_1.
    rv_ = gt_dp_8[ lx_dp + 1 ].

  ENDMETHOD.


  METHOD get_hh.

    SELECT *
      FROM zvdb_002_vhs
      INTO CORRESPONDING FIELDS OF TABLE @rt_
      WHERE bid = @mv_bid AND
            ip > 0 AND r = ''.

    SORT rt_ BY ip.

  ENDMETHOD.


  METHOD get_hr.

    SELECT *
      FROM zvdb_002_vhs
      INTO CORRESPONDING FIELDS OF TABLE @rt_
      WHERE bid = @mv_bid AND
            ip > 0 AND r = 'X'.

    SORT rt_ BY ip.

  ENDMETHOD.


  METHOD get_index_bypass.
    SELECT COUNT( * )
      INTO @DATA(lv_count)
      FROM zvdb_002_vector
      WHERE bid = @mv_bid.

    IF lv_count > 30000.
      rs_-i1 = 'X'.
      rs_-i3 = 'X'.
    ENDIF.


    IF lv_count > 60000.
      rs_-i1 = 'X'.
      rs_-i2 = 'X'.
      rs_-i3 = 'X'.
    ENDIF.


  ENDMETHOD.


  METHOD get_set.
    SELECT *
      FROM zvdb_002_set
      INTO TABLE rt_
      WHERE bid = mv_bid AND
            id IN ('HR','HH').
    IF mv_ignore_settings  = 'X' OR
       lines( rt_ ) NE 2.
      mv_rank_h = 512. " 1536/3                         " empirical was +476.
      mv_rank_r = 0.   " middle between -1536 and +1536 " empirical was -008.
      RETURN.
    ELSE.
      mv_rank_h = rt_[ id = 'HH' ]-rank.
      mv_rank_r = rt_[ id = 'HR' ]-rank.
    ENDIF.
  ENDMETHOD.


  METHOD guid.
    TRY.
        rv_ = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lx_).
        rv_ = '0'.
    ENDTRY.
  ENDMETHOD.


  METHOD hamming_16.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ix_ ) TO rtr_.
    LOOP AT gt_hamming_16 REFERENCE INTO DATA(lr_).
      DATA(lx_) = ix_ BIT-XOR lr_->*.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lx_ ) TO rtr_.
    ENDLOOP.
  ENDMETHOD.


  METHOD hamming_24.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ix_ ) TO rtr_.
    LOOP AT gt_hamming_24 REFERENCE INTO DATA(lr_).
      DATA(lx_) = ix_ BIT-XOR lr_->*.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lx_ ) TO rtr_.
    ENDLOOP.
  ENDMETHOD.


  METHOD hamming_8.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ix_ ) TO rtr_.
    LOOP AT gt_hamming_8 REFERENCE INTO DATA(lr_).
      DATA(lx_) = ix_ BIT-XOR lr_->*.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lx_ ) TO rtr_.
    ENDLOOP.
  ENDMETHOD.


  METHOD hash.
    "dp (and)
    LOOP AT mt_hh FROM mv_hp_start TO mv_hp_end REFERENCE INTO DATA(lr_hh).
      DATA(lv_hh) = dp_1536( ix_0 = ix_ ix_1 = lr_hh->q1b ).
      IF lv_hh > mv_rank_h. "
        SET BIT lr_hh->ip OF rs_-hh TO 1.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_hr FROM mv_hr_start TO mv_hr_end REFERENCE INTO DATA(lr_hr).
      DATA(lv_hr) = dp_1536( ix_0 = ix_ ix_1 = lr_hr->q1b ).
      IF lv_hr > mv_rank_r. "
        SET BIT lr_hr->ip OF rs_-hr TO 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD hash_fast_fold.
    DATA(lx_2) = ix_+0(96)      BIT-XOR ix_+96(96).
    rs_-hff48 = lx_2+0(48)      BIT-XOR lx_2+48(48).
    rs_-hff24 = rs_-hff48+0(24) BIT-XOR rs_-hff48+24(24).
    rs_-hff12 = rs_-hff24+0(12) BIT-XOR rs_-hff24+12(12).
    rs_-hff6  = rs_-hff12+0(6)  BIT-XOR rs_-hff12+6(6).
    rs_-hff3  = rs_-hff6+0(3)   BIT-XOR rs_-hff6+3(3).
    rs_-hff2  = rs_-hff6+0(2)   BIT-XOR rs_-hff6+2(2) BIT-XOR rs_-hff6+4(2).
  ENDMETHOD.


  METHOD hash_s.
    DATA(ls_) = hash( ix_ ).
    rs_-hh1 = ls_-hh+0(1).
    rs_-hh2 = ls_-hh+1(2).
    rs_-hr1 = ls_-hr+0(1).
    rs_-hr2 = ls_-hr+1(2).
*    DATA(ls_ff) = hash_fast_fold( ix_ ).
**   rs_-hff6 = ls_ff-hff6.
**   rs_-hff3 = ls_ff-hff3.
*    rs_-hff2 = ls_ff-hff2.
  ENDMETHOD.


  METHOD init_bake_hp.
    CLEAR: mt_hh.
    " 8 heuristic hyper planes (seed is random)
    DATA(lt_hh_a) = bf_mid_far( random_vector( )-q1b ).
    WHILE lines( lt_hh_a ) < 8.
      DATA(ltr_filter_a) = ttv_to_range_exclude(  lt_hh_a ).
      APPEND LINES OF bf_mid_far( ix_  = lt_hh_a[ sy-index ]-q1b
                                  itr_ = ltr_filter_a )
        TO lt_hh_a.
    ENDWHILE.


    ltr_filter_a = ttv_to_range_exclude(  lt_hh_a ).
    " 16 heuristic hyper planes (seed is random)
    DATA(lt_hh_b) = bf_mid_far( ix_  = random_vector( )-q1b
                                itr_ = ltr_filter_a ).
    WHILE lines( lt_hh_b ) < 16.
      DATA(ltr_filter_b) = ttv_to_range_exclude(  lt_hh_b ).
      APPEND LINES OF ltr_filter_a TO ltr_filter_b.
      APPEND LINES OF bf_mid_far( ix_  = lt_hh_b[ sy-index ]-q1b
                                  itr_ = ltr_filter_b )
        TO lt_hh_b.
    ENDWHILE.

    mt_hh = lt_hh_a.
    APPEND LINES OF lt_hh_b TO mt_hh.

    LOOP AT mt_hh REFERENCE INTO DATA(lr_hh). "assign hash index
      lr_hh->ip = sy-tabix.
    ENDLOOP.
*--------------------------------------------------------------------*
*   24 random hyper planes for RANDOM HASH 8 + 16 bit
    CLEAR: mt_hr.
    DO 24 TIMES.
      APPEND random_vector( CONV #( sy-index ) ) TO mt_hr.
    ENDDO.

  ENDMETHOD.


  METHOD init_bit_counter_tab_16bit.
*--------------------------------------------------------------------*
    DATA: lx_(2) TYPE x.
    DATA:lv_i TYPE i. "0.
    DO 65536 TIMES.
      lx_ = lv_i. "byte 00..FF -> index and the input parameter
      DATA(lv_v) = 0. "value for index
      DO 16 TIMES.
        GET BIT sy-index OF lx_ INTO DATA(lv_b).
        IF lv_b = 0.
          ADD 1 TO lv_v.
        ELSE.
          SUBTRACT 1 FROM lv_v.
        ENDIF.
      ENDDO.
      "APPEND lv_v TO rt_.
      rx_+lv_i(1) = lv_v.
      ADD 1 TO lv_i.
    ENDDO.
*    DATA: lxx_(2) TYPE x.
*    DATA: lv_i16 TYPE i.
*    DATA: lv_i0 TYPE i.
*    DATA: lv_i1 TYPE i.
*    DATA: lv_s TYPE i.
*    DO 65536 TIMES.
*      lxx_  = lv_i16.
*      lv_i0 = lxx_+0(1).
*      lv_i1 = lxx_+1(1).
*      lv_s = CONV i( ix_8+lv_i0(1) ) + CONV i( ix_8+lv_i1(1) ).
*      rx_+lv_i16(1) = lv_s.
*      ADD 1 TO lv_i16.
*    ENDDO.
  ENDMETHOD.


  METHOD init_bit_counter_tab_8bit.

    rx_+0(64)   = '00010102010202030102020302030304010202030203030402030304030404050102020302030304020303040304040502030304030404050304040504050506'.
    rx_+64(64)  = '01020203020303040203030403040405020303040304040503040405040505060203030403040405030404050405050603040405040505060405050605060607'.
    rx_+128(64) = '01020203020303040203030403040405020303040304040503040405040505060203030403040405030404050405050603040405040505060405050605060607'.
    rx_+192(64) = '02030304030404050304040504050506030404050405050604050506050606070304040504050506040505060506060704050506050606070506060706070708'.

*    DATA(lx_8) = rx_.
*    CLEAR rx_.
*
*    DATA: lx_ TYPE ts_bc-x.
*    DATA: lv_i TYPE i.
*    DO 256 TIMES.
*      DATA(ls_) = gt_bc[ x = lx_ ].
*      rx_+lv_i(1) = ls_-s.
*      ADD 1 TO lx_.
*      ADD 1 TO lv_i.
*    ENDDO.
*
*    IF lx_8 NE rx_.
*      BREAK-POINT.
*    ENDIF.
*
*    cl_demo_output=>write( lx_8 ).
*    cl_demo_output=>write( rx_ ).
*    cl_demo_output=>display( ).

  ENDMETHOD.


  METHOD init_class.
    gx8           = init_bit_counter_tab_8bit( ).
    "gx16          = init_bit_counter_tab_16bit( ).
    gt_dp_8       = init_signed_counter_tab_8bit( ).
    gt_dp_16      = init_signed_counter_tab_16bit( ).
    g = gt_dp_16.
*   gt_dp_u16     = init_usigned_counter_tab_16bit( ).

    gt_hamming_8  = init_hamming_8bit( ).
    gt_hamming_16 = init_hamming_16bit( ).
*   gt_hamming_24 = init_hamming_24bit( ).

    go_random_int = cl_abap_random_int=>create(
      seed  = 42
      min   = 0
      max   = 256
    ).

    go_random_f   = cl_abap_random_float=>create(
      seed  = 42
    ).

  ENDMETHOD.


  METHOD init_hamming_16bit.
    rt_ = init_hamming_16bit_d3(  ).
  ENDMETHOD.


  METHOD init_hamming_16bit_d1.
    DATA: lx_ TYPE ty_b16.
    DO 16 TIMES.
      lx_ = '0000'.
      SET BIT sy-index OF lx_ TO 1.
      APPEND lx_ TO rt_.
    ENDDO.
    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_16bit_d2.
    DATA: lx_1 TYPE ty_b16.
    DATA(lv_i1) = 1.
    DO 16 TIMES.
      lx_1 = '0000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 16 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        APPEND lx_2 TO rt_.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_16bit_d3.
    DATA: lx_1 TYPE ty_b16.
    DATA(lv_i1) = 1.
    DO 16 TIMES.
      lx_1 = '0000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 16 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        DATA(lv_i3) = 1.
        DO 16 TIMES.
          DATA(lx_3) = lx_2.
          SET BIT lv_i3 OF lx_3 TO 1.
          APPEND lx_3 TO rt_.
          ADD 1 TO lv_i3.
        ENDDO.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_16bit_d4.
    DATA: lx_1 TYPE ty_b16.
    DATA(lv_i1) = 1.
    DO 16 TIMES.
      lx_1 = '0000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 16 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        DATA(lv_i3) = 1.
        DO 16 TIMES.
          DATA(lx_3) = lx_2.
          SET BIT lv_i3 OF lx_3 TO 1.
          DATA(lv_i4) = 1.
          DO 16 TIMES.
            DATA(lx_4) = lx_3.
            SET BIT lv_i4 OF lx_4 TO 1.
            APPEND lx_4 TO rt_.
            ADD 1 TO lv_i4.
          ENDDO.
          ADD 1 TO lv_i3.
        ENDDO.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_24bit.
    rt_ = init_hamming_24bit_d3(  ).
  ENDMETHOD.


  METHOD init_hamming_24bit_d1.
    DATA: lx_ TYPE ty_b24.
    DO 24 TIMES.
      lx_ = '000000'.
      SET BIT sy-index OF lx_ TO 1.
      APPEND lx_ TO rt_.
    ENDDO.
    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_24bit_d2.
    DATA: lx_1 TYPE ty_b24.
    DATA(lv_i1) = 1.
    DO 24 TIMES.
      lx_1 = '000000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 24 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        APPEND lx_2 TO rt_.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_24bit_d3.
    DATA: lx_1 TYPE ty_b24.
    DATA(lv_i1) = 1.
    DO 24 TIMES.
      lx_1 = '000000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 24 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        DATA(lv_i3) = 1.
        DO 24 TIMES.
          DATA(lx_3) = lx_2.
          SET BIT lv_i3 OF lx_3 TO 1.
          APPEND lx_3 TO rt_.
          ADD 1 TO lv_i3.
        ENDDO.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_24bit_d4.
    DATA: lx_1 TYPE ty_b24.
    DATA(lv_i1) = 1.
    DO 12 TIMES.
      lx_1 = '000000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 24 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        DATA(lv_i3) = 1.
        DO 24 TIMES.
          DATA(lx_3) = lx_2.
          SET BIT lv_i3 OF lx_3 TO 1.
          DATA(lv_i4) = 1.
          DO 24 TIMES.
            DATA(lx_4) = lx_3.
            SET BIT lv_i4 OF lx_4 TO 1.
            APPEND lx_4 TO rt_.
            ADD 1 TO lv_i4.
          ENDDO.
          ADD 1 TO lv_i3.
        ENDDO.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 2 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_8bit.
    rt_ = init_hamming_8bit_d2( ).
  ENDMETHOD.


  METHOD init_hamming_8bit_d1.
    DATA: lx_ TYPE ty_b8.
    DO 8 TIMES.
      lx_ = '00'.
      SET BIT sy-index OF lx_ TO 1.
      APPEND lx_ TO rt_.
    ENDDO.
    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.
  ENDMETHOD.


  METHOD init_hamming_8bit_d2.
    DATA: lx_1 TYPE ty_b8.
    DATA(lv_i1) = 1.
    DO 8 TIMES.
      lx_1 = '00'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 8 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        APPEND lx_2 TO rt_.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_hamming_8bit_d3.
    DATA: lx_1 TYPE ty_b8.
    DATA(lv_i1) = 1.
    DO 8 TIMES.
      lx_1 = '0000'.
      SET BIT lv_i1 OF lx_1 TO 1.
      DATA(lv_i2) = 1.
      DO 8 TIMES.
        DATA(lx_2) = lx_1.
        SET BIT lv_i2 OF lx_2 TO 1.
        DATA(lv_i3) = 1.
        DO 8 TIMES.
          DATA(lx_3) = lx_2.
          SET BIT lv_i3 OF lx_3 TO 1.
          APPEND lx_3 TO rt_.
          ADD 1 TO lv_i3.
        ENDDO.
        ADD 1 TO lv_i2.
      ENDDO.
      ADD 1 TO lv_i1.
    ENDDO.

    SORT rt_ BY table_line.
    DELETE ADJACENT DUPLICATES FROM rt_.

  ENDMETHOD.


  METHOD init_instance.
    mv_bid  = iv_.
    mt_hh   = get_hh( ).
    mt_hr   = get_hr( ).
    mt_set  = get_set( ).
    IF is_ IS NOT SUPPLIED.
      ms_ib = get_index_bypass( ).
    ELSE.
      ms_ib = is_.
    ENDIF.
    mv_hp_start = 1.
    mv_hp_end   = 24.
    IF ms_ib-i1 = 'X'.
      mv_hp_start = 9.
    ENDIF.
    IF ms_ib-i2 = 'X'.
      mv_hp_end = 8.
    ENDIF.
    mv_hr_start = 1.
    mv_hr_end   = 24.
    IF ms_ib-i3 = 'X'.
      mv_hr_start = 9.
    ENDIF.
    IF ms_ib-i4 = 'X'.
      mv_hr_end = 8.
    ENDIF.
  ENDMETHOD.


  METHOD init_save_hp.

    DATA: ltr_id TYPE RANGE OF zvdb_002_vector-id.
    ltr_id = VALUE #( FOR ls_hh1 IN mt_hh
                        ( sign = 'I' option = 'EQ' low = ls_hh1-id )
                     ).

    SELECT *
      FROM zvdb_002_vector
      INTO TABLE @DATA(lt_v)
      WHERE bid = @mv_bid AND
            id IN @ltr_id.

    LOOP AT mt_hh REFERENCE INTO DATA(lr_hh).
      DATA(lr_v) = REF #( lt_v[ id = lr_hh->id ] OPTIONAL ).
      IF lr_v IS BOUND.
        lr_hh->bid = mv_bid.
        lr_hh->p = lr_v->p.
      ENDIF.
    ENDLOOP.


    MODIFY zvdb_002_vhs FROM TABLE mt_hh.
    MODIFY zvdb_002_vhs FROM TABLE mt_hr.

  ENDMETHOD.


  METHOD init_signed_counter_tab_16bit.
*--------------------------------------------------------------------*
    DATA: lx_(2) TYPE x.
    DATA:lv_i TYPE i. "0.
    DO 65536 TIMES.
      lx_ = lv_i. "byte 00..FF -> index and the input parameter
      DATA(lv_v) = 0. "value for index
      DO 16 TIMES.
        GET BIT sy-index OF lx_ INTO DATA(lv_b).
        IF lv_b = 0.
          ADD 1 TO lv_v.
        ELSE.
          SUBTRACT 1 FROM lv_v.
        ENDIF.
      ENDDO.
      APPEND lv_v TO rt_.
      ADD 1 TO lv_i.
    ENDDO.
  ENDMETHOD.


  METHOD init_signed_counter_tab_8bit.
    DATA: lx_(1) TYPE x.
    DATA:lv_i TYPE i. "0.
    DO 256 TIMES.
      lx_ = lv_i. "byte 00..FF -> index and the input parameter
      DATA(lv_v) = 0. "value for index
      DO 8 TIMES.
        GET BIT sy-index OF lx_ INTO DATA(lv_b).
        IF lv_b = 0.
          ADD 1 TO lv_v.
        ELSE.
          SUBTRACT 1 FROM lv_v.
        ENDIF.
      ENDDO.
      APPEND lv_v TO rt_.
      ADD 1 TO lv_i.
    ENDDO.

  ENDMETHOD.


  METHOD init_usigned_counter_tab_16bit.
*--------------------------------------------------------------------*
    DATA: lx_(2) TYPE x.
    DATA:lv_i TYPE i. "0.
    DO 65536 TIMES.
      lx_ = lv_i. "byte 00..FF -> index and the input parameter
      DATA(lv_v) = 0. "value for index
      DO 16 TIMES.
        GET BIT sy-index OF lx_ INTO DATA(lv_b).
        IF lv_b = 1.
          ADD 1 TO lv_v.
        ENDIF.
      ENDDO.
      APPEND lv_v TO rt_.
      ADD 1 TO lv_i.
    ENDDO.
  ENDMETHOD.


  METHOD new.

    ro_ = NEW zcl_vdb_002_lib(
      iv_ = iv_
      is_ = is_
    ).

  ENDMETHOD.


  METHOD qf_in_tt.
    LOOP AT ct_ REFERENCE INTO DATA(lr_).
      DATA(lx_dp_h1) = is_-hh1  BIT-XOR lr_->hh1.
      DATA(lx_dp_h2) = is_-hh2  BIT-XOR lr_->hh2.
      DATA(lx_dp_r1) = is_-hr1  BIT-XOR lr_->hr1.
      DATA(lx_dp_r2) = is_-hr2  BIT-XOR lr_->hr2.
      lr_->rank2 = gt_dp_8[ lx_dp_h1 + 1 ] + gt_dp_16[ lx_dp_h2 + 1 ] + gt_dp_8[ lx_dp_r1 + 1 ] + gt_dp_16[ lx_dp_r1 + 1 ].
    ENDLOOP.
    SORT ct_ BY rank2 DESCENDING. "dot product of hashes -> value in range +48 (vectors are equal) to -48 (opposite)
  ENDMETHOD.


  METHOD qquery.
    " fetching using indices

    IF ms_ib-i1 NE 'X'.
      DATA(ltr_hh1) = hamming_8(  is_-hh1 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hh1 IN @ltr_hh1.

    ENDIF.
    IF ms_ib-i2 NE 'X'.
      DATA(ltr_hh2) = hamming_16( is_-hh2 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hh2 IN @ltr_hh2.
    ENDIF.
    IF ms_ib-i3 NE 'X'.
      DATA(ltr_hr1) = hamming_8(  is_-hr1 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hr1 IN @ltr_hr1.
    ENDIF.
    IF ms_ib-i4 NE 'X'.
      DATA(ltr_hr2) = hamming_16( is_-hr2 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hr2 IN @ltr_hr2.
    ENDIF.

*    IF ms_ib-i5 NE 'X'.
*      DATA(ltr_hff2) = hamming_16( is_-hff2 ).
*      SELECT *
*        FROM zvdb_002_vector
*        APPENDING TABLE @rt_
*        WHERE bid = @mv_bid AND
*              hff2 IN @ltr_hff2.
*    ENDIF.
    SORT rt_ BY id.
    DELETE ADJACENT DUPLICATES FROM rt_ COMPARING id.
    qf_in_tt( EXPORTING is_ = is_
              CHANGING  ct_ = rt_
    ).

  ENDMETHOD.


  METHOD qquery_by_id.
    DATA(ls_v) = read_vector( iv_ ).
    IF ls_v IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rt_ = qquery( ls_v ).
  ENDMETHOD.


  METHOD quantize.

    LOOP AT it_ FROM 1 TO 1536 REFERENCE INTO DATA(lr_).
      IF lr_->* < 0.
        SET BIT sy-tabix OF rx_ TO 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD quantize_from_string.
    TYPES: tt_f TYPE TABLE OF f WITH DEFAULT KEY.
    DATA:lt_s TYPE string_table.
    DATA:lt_f TYPE tt_f.
    DATA:lt_v TYPE tt_embedding.
    SPLIT iv_ AT space INTO TABLE lt_s.
    lt_f = CORRESPONDING #( lt_s ).
    lt_v = CORRESPONDING #( lt_f ).
    rx_ = me->quantize( lt_v ).

  ENDMETHOD.


  METHOD query.
    DATA(ls_h) = hash_s( ix_ ).

    " fetching using indices
    IF ms_ib-i1 NE 'X'.
      DATA(ltr_hh1) = hamming_8(  ls_h-hh1 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hh1 IN @ltr_hh1.

    ENDIF.
    IF ms_ib-i2 NE 'X'.
      DATA(ltr_hh2) = hamming_16( ls_h-hh2 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hh2 IN @ltr_hh2.
    ENDIF.
    IF ms_ib-i3 NE 'X'.
      DATA(ltr_hr1) = hamming_8(  ls_h-hr1 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hr1 IN @ltr_hr1.
    ENDIF.
    IF ms_ib-i4 NE 'X'.
      DATA(ltr_hr2) = hamming_16( ls_h-hr2 ).
      SELECT *
        FROM zvdb_002_vector
        APPENDING TABLE @rt_
        WHERE bid = @mv_bid AND
              hr2 IN @ltr_hr2.
    ENDIF.

*    IF ms_ib-i5 NE 'X'.
*      DATA(ltr_hff2) = hamming_16( ls_h-hff2 ).
*      SELECT *
*        FROM zvdb_002_vector
*        APPENDING TABLE @rt_
*        WHERE bid = @mv_bid AND
*              hff2 IN @ltr_hff2.
*    ENDIF.

    SORT rt_ BY id.
    DELETE ADJACENT DUPLICATES FROM rt_ COMPARING id.
    bf_in_tt( EXPORTING ix_ = ix_
              CHANGING  ct_ = rt_
    ).

  ENDMETHOD.


  METHOD query_by_id.
    DATA(ls_v) = read_vector( iv_ ).
    IF ls_v IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rt_ = query( ls_v-q1b ).
  ENDMETHOD.


  METHOD random_v.

    DATA:lv_i TYPE i. "0.
    DATA: lv_r TYPE i.
    DO 192 TIMES.
      lv_r = go_random_int->get_next( ).
      rx_+lv_i(1) = lv_r.
      ADD 1 TO lv_i.
    ENDDO.

  ENDMETHOD.


  METHOD random_vector.
    rs_-bid  = mv_bid.
    rs_-id   = guid( ).
    rs_-q1b  = random_v( ).
    rs_-rank = -1.
    rs_-r    = 'X'.
    IF  iv_ IS NOT INITIAL.
      rs_-ip = iv_.
      rs_-p  = |Random hyper plane #{ iv_ }|.
    ENDIF.
  ENDMETHOD.


  METHOD rank_all_to_autocalibrate_a.
    DATA: ls_h LIKE LINE OF mt_hash_log_h.
    DATA: ls_r LIKE LINE OF mt_hash_log_r.
    LOOP AT mt_v REFERENCE INTO DATA(lr_v).
      LOOP AT mt_hh REFERENCE INTO DATA(lr_hh).
        ls_h = CORRESPONDING #( lr_hh->* ).
        ls_h-rank = dp_1536(  ix_0 = lr_v->q1b ix_1 = lr_hh->q1b ).
        APPEND ls_h TO mt_hash_log_h.
      ENDLOOP.

      LOOP AT mt_hr REFERENCE INTO DATA(lr_hr).
        ls_r = CORRESPONDING #( lr_hr->* ).
        ls_r-rank = dp_1536(  ix_0 = lr_v->q1b ix_1 = lr_hr->q1b ).
        APPEND ls_r TO mt_hash_log_r.
      ENDLOOP.
    ENDLOOP.

    DATA: lv_rank_sum_h TYPE int8.
    DATA: lv_rank_h    TYPE f.
    DATA: lv_count_h    TYPE i.

    DATA: lv_rank_sum_r TYPE int8.
    DATA: lv_rank_r    TYPE f.
    DATA: lv_count_r    TYPE i.

    LOOP AT mt_hash_log_h REFERENCE INTO DATA(lr_h).
      ADD lr_h->rank TO lv_rank_sum_h.
      ADD 1 TO lv_count_h.
    ENDLOOP.
    LOOP AT mt_hash_log_r REFERENCE INTO DATA(lr_r).
      ADD lr_r->rank TO lv_rank_sum_r.
      ADD 1 TO lv_count_r.
    ENDLOOP.

    lv_rank_h = ( lv_rank_sum_h / lv_count_h ) * 2 * iv_.
    lv_rank_r = ( lv_rank_sum_r / lv_count_r  ) * 2 * iv_.

    APPEND VALUE #( bid = mv_bid id = 'HH' rank = lv_rank_h ) TO rt_.
    APPEND VALUE #( bid = mv_bid id = 'HR' rank = lv_rank_r ) TO rt_.
  ENDMETHOD.


  METHOD rank_all_to_autocalibrate_m.
    DATA: ls_h LIKE LINE OF mt_hash_log_h.
    DATA: ls_r LIKE LINE OF mt_hash_log_r.
    LOOP AT mt_v REFERENCE INTO DATA(lr_v).
      LOOP AT mt_hh REFERENCE INTO DATA(lr_hh).
        ls_h = CORRESPONDING #( lr_hh->* ).
        ls_h-rank = dp_1536(  ix_0 = lr_v->q1b ix_1 = lr_hh->q1b ).
        APPEND ls_h TO mt_hash_log_h.
      ENDLOOP.

      LOOP AT mt_hr REFERENCE INTO DATA(lr_hr).
        ls_r = CORRESPONDING #( lr_hr->* ).
        ls_r-rank = dp_1536(  ix_0 = lr_v->q1b ix_1 = lr_hr->q1b ).
        APPEND ls_r TO mt_hash_log_r.
      ENDLOOP.
    ENDLOOP.

    DATA: lv_rank_sum_h TYPE int8.
    DATA: lv_rank_h    TYPE f.
    DATA: lv_count_h    TYPE i.

    DATA: lv_rank_sum_r TYPE int8.
    DATA: lv_rank_r    TYPE f.
    DATA: lv_count_r    TYPE i.
*--------------------------------------------------------------------*
    lv_count_h = lines( mt_hash_log_h ).
    SORT mt_hash_log_h BY rank DESCENDING.
    lv_rank_h = mt_hash_log_h[ ( lv_count_h * iv_ ) ]-rank.

    lv_count_r = lines( mt_hash_log_r ).
    SORT mt_hash_log_r BY rank DESCENDING.
    lv_rank_r = mt_hash_log_r[ ( lv_count_r * iv_ ) ]-rank.

    APPEND VALUE #( bid = mv_bid id = 'HH' rank = lv_rank_h ) TO rt_.
    APPEND VALUE #( bid = mv_bid id = 'HR' rank = lv_rank_r ) TO rt_.
  ENDMETHOD.


  METHOD read_vector.

    SELECT SINGLE *
      FROM zvdb_002_vector
      INTO @rs_
      WHERE bid = @mv_bid AND
            id  = @iv_.

  ENDMETHOD.


  METHOD recalibrate_hash.
*    reset_hp( ).
*    COMMIT WORK AND WAIT.
*
*    init_bake_hp( ).
*    init_save_hp( ).
*    COMMIT WORK AND WAIT.
*    clear_cache( ).
*    clear_hash_log( ).
*--------------------------------------------------------------------*
    mt_hh = get_hh( ).
    mt_hr = get_hr( ).
    cache_samples( ). "side effect: mt_v loaded (cache)
    IF to_upper( iv_ ) = 'AVERAGE'.
      rt_ = rank_all_to_autocalibrate_a( iv_treshold ).
    ELSE.
      rt_ = rank_all_to_autocalibrate_m( iv_treshold ).
    ENDIF.

    MODIFY zvdb_002_set FROM TABLE rt_.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD recalibrate_hash_and_reindex.
    init_class( ).
    init_instance( iv_ = iv_bid
                   is_ = is_ ).
    rt_ = recalibrate_hash(
        iv_         = iv_
        iv_treshold = iv_treshold
    ).
*--------------------------------------------------------------------*
    init_instance( iv_ = iv_bid
                   is_ = is_ ).
    reindex_all( iv_ = iv_bid
                 is_ = is_ ).

  ENDMETHOD.


  METHOD regen_hash.
    reset_hp( ).
    COMMIT WORK AND WAIT.

    init_bake_hp( ).
    init_save_hp( ).
    COMMIT WORK AND WAIT.
    clear_cache( ).
    clear_hash_log( ).
*--------------------------------------------------------------------*
  ENDMETHOD.


  METHOD regen_recal_hash_and_reindex.
    init_class( ).
    init_instance( iv_ = iv_bid
                   is_ = is_ ).
    regen_hash( ).
    rt_ = recalibrate_hash(
        iv_         = iv_
        iv_treshold = iv_treshold
    ).
*--------------------------------------------------------------------*
    init_instance( iv_ = iv_bid
                   is_ = is_ ).
    reindex_all( iv_ = iv_bid
                 is_ = is_ ).

  ENDMETHOD.


  METHOD reindex_all.
    clear_cache( ).
    clear_hash_log( ).
    init_instance( iv_ = iv_
                   is_ = is_ ).

    SELECT *
      FROM zvdb_002_vector
      INTO TABLE @DATA(lt_v)
      WHERE bid = @mv_bid AND
            id IS NOT NULL.

    LOOP AT lt_v REFERENCE INTO DATA(lr_v).
      DATA(ls_h) = hash_s( lr_v->q1b ).
      lr_v->hh1  = ls_h-hh1.
      lr_v->hh2  = ls_h-hh2.
      lr_v->hr1  = ls_h-hr1.
      lr_v->hr2  = ls_h-hr2.
*     lr_v->hff2 = ls_h-hff2.
*     lr_v->hff3 = ls_h-hff3.
    ENDLOOP.
    MODIFY zvdb_002_vector FROM TABLE lt_v.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD reset_hp.

    DELETE FROM zvdb_002_vhs WHERE bid = @mv_bid AND id IS NOT NULL.

  ENDMETHOD.


  METHOD save_vector.
    DATA(ls_) = is_.
    IF iv_bid IS SUPPLIED.
      ls_-bid = iv_bid. "if provided explicitly - use provided bucket id
    ELSE.
      ls_-bid = mv_bid. " if not - use current bucket id set up during instantiation on the instance level/
    ENDIF.
    MODIFY zvdb_002_vector FROM @ls_.

  ENDMETHOD.


  METHOD ttv_to_range.
    rtr_ = VALUE #( FOR ls_ IN it_
        ( sign = 'I' option = 'EQ' low = ls_-q1b )
    ).

  ENDMETHOD.


  METHOD ttv_to_range_exclude.
    rtr_ = VALUE #( FOR ls_ IN it_
        ( sign = 'E' option = 'EQ' low = ls_-q1b )
    ).

  ENDMETHOD.
ENDCLASS.
