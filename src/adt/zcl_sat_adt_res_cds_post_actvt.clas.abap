"! <p class="shorttext synchronized">Resource for CDS Post Activation</p>
CLASS zcl_sat_adt_res_cds_post_actvt DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_post_actvt IMPLEMENTATION.
  METHOD post.
    DATA single_ddlname TYPE ddlname.
    DATA ddlname_string_list TYPE string.
    DATA ddlname_range TYPE zif_sat_ty_global=>ty_t_ddlname_range.

    request->get_uri_attribute( EXPORTING name  = zif_sat_c_adt_utils=>c_cds_post_activation_param-ddlname
                                IMPORTING value = single_ddlname ).

    request->get_body_data(
      EXPORTING content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
      IMPORTING data            = ddlname_string_list ).

    IF    ( single_ddlname IS INITIAL     AND ddlname_string_list IS INITIAL )
       OR ( single_ddlname IS NOT INITIAL AND ddlname_string_list IS NOT INITIAL ).

      MESSAGE e001(00) WITH |Either fill 'ddlname' in URI or pass a string of ddlnames| | separated  by ';' in the payload|.

      RAISE EXCEPTION TYPE zcx_sat_adt_rest_error
        EXPORTING textid = cx_adt_rest=>create_textid_from_msg_params( ).
    ENDIF.

    IF ddlname_string_list IS NOT INITIAL.
      SPLIT ddlname_string_list AT ';' INTO TABLE DATA(ddlnames).
      ddlname_range = VALUE #( FOR ddlname IN ddlnames ( sign = 'I' option = 'EQ' low = to_upper( ddlname ) ) ).
    ELSE.
      ddlname_range = VALUE #( ( sign = 'I' option = 'EQ' low = to_upper( single_ddlname ) ) ).
    ENDIF.

    " update cds meta information for cds views that were activated
    NEW zcl_sat_cds_v2_meta_updater( ddlname_range = ddlname_range )->update_index( ).
  ENDMETHOD.
ENDCLASS.
