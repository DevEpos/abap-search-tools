"! <p class="shorttext synchronized" lang="en">Resource for Enhanced Where-Used List</p>
CLASS zcl_sat_adt_res_whereused DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post
        REDEFINITION.
  PROTECTED SECTION.
    DATA mv_object_name TYPE seu_objkey.
    DATA ms_object_type TYPE wbobjtype.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_whereused IMPLEMENTATION.

  METHOD post.

*** TODO: Retrieve parameters of target object
    " mv_object = 'I_PRODUCT'
    " ms_object_type = VALUE #( objtype_tr = 'STOB' subtype_wb = 'DO' )
    TRY.
        cl_ris_adt_res_references=>get_references(
          EXPORTING
            i_object_type                 = ms_object_type
            i_object_name                 = mv_object_name
            it_scope_referencing_objtypes = VALUE #(
                                                      ( objtype_tr = 'CLAS'  subtype_wb = 'OC' )
                                                      ( objtype_tr = 'PROG'  subtype_wb = 'P' ) )
          IMPORTING
            e_result                      = DATA(ls_where_used_list) ).
        DATA(lo_content_handler) = cl_adt_rest_st_handler=>create_instance(
            st_name      = cl_ris_adt_res_references=>co_transformation-result
            content_type = cl_ris_adt_res_references=>co_transformation_content_type-result
        ).
        lo_content_handler = cl_adt_rest_comp_cnt_handler=>create(
            request         = request
            content_handler = lo_content_handler
        ). " wrapper validates content-type

        response->set_body_data( content_handler = lo_content_handler data = ls_where_used_list ).

        IF ls_where_used_list-results_possibly_incomplete = abap_true.
          response->set_status( status = cl_rest_status_code=>gc_success_partial_content ).
        ENDIF.
      CATCH cx_ris_exception.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
