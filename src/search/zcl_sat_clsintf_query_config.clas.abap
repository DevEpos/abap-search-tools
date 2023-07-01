"! <p class="shorttext synchronized">Configuration for Class/Interface Query</p>
CLASS zcl_sat_clsintf_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.

    METHODS zif_sat_object_search_config~get_type          REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        folder    TYPE string VALUE 'ABAP:IMG_FOLDER',
        abap_lang TYPE string VALUE 'ABAP:IMG_SOURCE_CODE',
        friend    TYPE string VALUE 'ABAP:IMG_FRIEND',
        interface TYPE string VALUE 'ABAP:IMG_INTERFACE',
        super     TYPE string VALUE 'ABAP:IMG_SUPER_TYPE',
        method    TYPE string VALUE 'ABAP:IMG_METHOD',
        attribute TYPE string VALUE 'ABAP:IMG_ATTRIBUTE',
      END OF c_image_keys.

    METHODS get_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sat_clsintf_query_config IMPLEMENTATION.
  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter(
        ( get_user_filt_conf( ) )
        ( get_package_filt_conf( ) )
        ( get_rel_state_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( name           = c_general_options-type
          img_key        = c_general_image_keys-type_folder
          img_encoded    = get_general_image( c_general_image_keys-type_folder )
          caching        = abap_true
          content_assist = VALUE #(
              assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_type
              proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                           img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) )
        ( name           = c_class_intf_search_option-flag
          img_key        = c_general_image_keys-checked_box
          img_encoded    = get_general_image( c_general_image_keys-checked_box )
          caching        = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_flag ) )
        ( name           = c_class_intf_search_option-category
          img_key        = c_image_keys-folder
          img_encoded    = get_image( c_image_keys-folder )
          caching        = abap_true
          content_assist = VALUE #(
              assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_category
              proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                           img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) )
        ( name           = c_class_intf_search_option-method
          img_key        = c_image_keys-method
          img_encoded    = get_image( c_image_keys-method )
          allowed_length = 61
          patterns       = abap_true )
        ( name           = c_class_intf_search_option-interface
          img_key        = c_image_keys-interface
          img_encoded    = get_image( c_image_keys-interface )
          allowed_length = 30
          patterns       = abap_true )
        ( name           = c_class_intf_search_option-attribute
          img_key        = c_image_keys-attribute
          img_encoded    = get_image( c_image_keys-attribute )
          allowed_length = 30
          key_value      = abap_true
          patterns       = abap_true )
        ( name           = c_class_intf_search_option-friend
          img_key        = c_image_keys-friend
          img_encoded    = get_image( c_image_keys-friend )
          allowed_length = 30
          patterns       = abap_true )
        ( name           = c_class_intf_search_option-super_type
          img_key        = c_image_keys-super
          img_encoded    = get_image( c_image_keys-super )
          allowed_length = 30
          patterns       = abap_true ) ).

    ms_search_type = VALUE #(
        label  = 'Class/Interface'
        name   = zif_sat_c_object_search=>c_search_type-class_interface
        inputs = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                            label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                          ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                            label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                            filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-class_interface.
  ENDMETHOD.

  METHOD get_image.
    CASE iv_image_key.

      WHEN c_image_keys-abap_lang.
        result = `R0lGODlhEAAQAPf/ABcyXXt0T311ToF3TYZ5S4p8S2d9qY9+SWqAq5WBR22DrZqDRZqERZ+GQ6CGRHOJsnSKs6SIQqWJQqqLQHqPt66MP66NP7GPPrSQPYCVu4GWu4ecwIqewpGetY2ixJGlx9SyaNSyadWyaa+3ydq9fODIj8jO3d7o997o+ODq+OLs9+Xt+efv+O` &&
                 `rw+ezy+u/z+vL1+/P2+/T2+/T2/PT3+/X3/Pb4+/b4/Pf5/Pj5+/j5/Pj6/Pn6/P///////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA` &&
                 `AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA` &&
                 `AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA` &&
                 `AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA` &&
                 `AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAD8ALAAAAAAQABAAAAirAH/8wECwYEGBCAVi4MGwIY8QGBIKvKBDB44cOXbgICHigkQLNz5wyAABgY0SJEBUSDghBo2XM2jIqPFyQsIIMD542KCBwgMFBmBISNjghdGjSB0kZOCiqdOn` &&
                 `CxImaNEBgIkWLUYA6NAiQcIDLExYZcHCKgAWBxIWWCHWxIoVZlcUSEhARVsVKsyqIJBwQIq2AFJU7ZBiQEIBJxIrVoxCQMIAkCNLhiyxsuWAADs=`.

      WHEN c_image_keys-attribute.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAEbSURBVDhPY/z//z8DJYAJSpMN4C44o29FklNMLh5jBNEoBij6u4HZMMDIxcXw//t3BgY0b97fuAtuAG4vMD` &&
                 `IyCFWUMQjVVEEFsAOsBrA/e8PALacI4bCwMHCJSoLFsAGcLmCPiWD4feQ4w79bdxg4kmPBLsIGsBrA6u7MwMjOxvD/02eGP2fPg8OCxdIMKosKMA0A2sQW5Admsnq4MLBFhEDYDrYMzOLiYDYywDCAy9ODgZGZmeH31p0MXyxcwBgEmMTFGFikJMFsZIBhAKeFOcP/` &&
                 `P38Yfk6aCRVhYPi1ZiMDIzcXA5ebK1QEATDTASsr2AX/f/wAi4MBMCbAAGgwCOBPB79/o2oGAZBGqGZ0QL2kTC7Amg6IBwwMAHM6YAiWs+qtAAAAAElFTkSuQmCC`.

      WHEN c_image_keys-folder.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAVFBMVEUAAAD48Mj46LDo0Ijw2JD44Jj44KD42Ij40HjgwHj42JD40IDYsHC8hTLDiza0fzKlbCStciu8fzLDhTatbCSeZietci+eXx2eXyCPUhn////46JgjdYhrAAAAAXRSTlMAQObYZgAAAFlJRE` &&
                 `FUeJydz0kSgCAMRNEoKojiBMaB+9/TEFPo0vLtfu8a4JeBvDucJDztY6xU2/uEB1dkjoelzHYexoZgBjBba7EWioZJa9QiNWwGO8ENBxpxN8CafTt2AcOvBf0oDmf4AAAAAElFTkSuQmCC`.

      WHEN c_image_keys-friend.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACYUlEQVR4nM2S31MSURTHffCxh/6JHvsfeuCh1MapMbFJCwFLa3oxZyBsGjORSvkxkakYQQlL8sNJS6VCFhWacSwde21MSHQBd9nLsrDLLrvbro0zkYwPPXVmvnPu+d5zPvfhnpqa/y6WXeqm2NSt4L` &&
                 `L7Or3k6hBWoBtE1Nv1CXZ3yI4dhOG+2sikajY61YnF140Yte/b44iZZAGBkO21YbDi6czDr9VB2HvzRFVA2KFwrs12Z9ns9G5ZFIe/TUqAA4lnWgR+Ee/Dr5QzR4bnRuUNEbcKJTeeY3TCnQKhJwT2Xk9Kg9l5PSnV7K4HIdZHsfBkO5gfa2mqACzYLkd2Ns3YPqSj` &&
                 `M29+K+3SUNLriPNO8dCX8s6mBRP7YxWAGevFfDHtSme8WjLxqK3040FzKePX5jjCj6S9PbnvukbRk5fSUA9JZdzpWesFugLgG64TOMKzLwkL9ua2B+UUgzhRqWbRSTTx+AqFvtMQZQAd9PhN9cJfgHMgn7TiXPYhwYOBHA+GAA8sOAdGcB48Ff1hwOGGHI8P5PJ7Ju` &&
                 `Az1hUqAAFLw9zW6r08l71Ptg5ZWZ3dzKyu64vxrf6ClKW6yfCszIv38a+9hYC5/mMlwNwom59oxhlssEhleinNhJE9fXuMP9n6QjjVNc53j5vKks+ihsIHuxybNp5vOPKVQVuLPepVkSw2SPG4luZxzR/S0pIfC6jzCza5p+oiwX1nakOOq/YlSJn9uXG3VEwNMBzQ` &&
                 `M1Sqn9n7pmOXoHY85GjzwCOXqm/iYYReKmURlzK86FAIi45rQkgU7FJ+DjkUZ48d/Nf4BdpeseqJ1iPyAAAAAElFTkSuQmCC`.

      WHEN c_image_keys-interface.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAB3UlEQVR4nGNgQANBDrNlQpwXFIe7LNwQ4bbgVrjLgkOhTguqQx3mS6CrxQBhzvPigRq+JPkv/16Wtvl/VdbW/wXx6//H+yz9HuY8/1eo84JknJpDnec1Rrgu/FmTvfX/pObDGLg8ffN/oAG/wxwXBG` &&
                 `NqdphvEOo0/39D/k6w4o/vv6Pgm1degcUrM7b8D3NZ+BHDO2HOC5anhaz801G59z8IL5l59j8MnD7y6P/ktsP/YXLxvkt/hDktaEZz/oJPuTFr/9fkbAXjnrp9cAP2br0FFwfhnOg1/8OdF+5GNQDo/JzI1f8L49eBcXPxDrgBO9Zfg4uDcFb4qv9AF79B98KTBN8l` &&
                 `/1ODloFxRcZGuAGbVlyCi4NwnPdiYGDOf47mhfkbotwXAv23GIyLk9fDDVi/7CJcHIQj3Rb8CXFasBzFgBDHef7AgPmV5L8UbMu7N1/hBnz98vP/qcMPwOJJfkuBzp//N8RlvgNmIgKaGuG28GdG6Mr/tblbUXBFxqb/aSEr/ocD00kYuu2ItDCVJ9xp/lFgePxJCl` &&
                 `j2Pyti1f9cYIiDAjcpYDnI33+Ati8BqcNqAAg4ODSwhDjPSw9zmX8DFDMgDNT0JcxlwZFQxwXeODXichEoY5GkaUAAAP0vdVygsaP9AAAAAElFTkSuQmCC`.

      WHEN c_image_keys-method.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAJbSURBVDhPxZNPSBRRHMe/772ZXWdczNRqKRdTtjKwP5bWEgpB0dHoUkR46hBCxz15Cgo8GHSLSi95KSm1Tn` &&
                 `UpygzKhUKIwLCoiBRX/DM7szOzO/Neb2YH7eCtQ5/D/B5f5vt9M7/3e/hXSFTXqc92na5itN8XotPzRUKhxKaM5MouH8jfmnoevbbORsC1k0rKEXcJJRe69jRorcktdIuuwnQ9zM4bmPySt1zPm/AKrC9/+5UZucCiilSmaWh7Tfzi5e60vj/ZSFK1+7BV2406LYka` &&
                 `3cPBlB77veKkHVFqNaZ+PI5slYDgs1WF3RjuzeptyQwM28H86gqWLUOui+hO9yC97RA4vqozv9aaYx2NH4rvfs4FXho84pT2H95VqzGoaNuZwfmjV/Dy8wwmZz+hqe4A2lPH0N54Qu6m4XhzfbUSp/2BLyAM8LjorNerqMr0UHw4PYYnV0fl7jbOHenB6PR4qC+sGU` &&
                 `jEFHBfdISCJAzgQiTKPkfJE6E4NDmCB+/H8CL7FI9y42C00utlswi3zOFzoYWCJAwghJiLhiv/uRCKAcNvRnDq5lnceX0fQXjAiuVhqeBC5tmhIKkEANMLBZsb9vrpbErJ48ibrnSRXCRVAnyIgbwpWx8RY2q0qkBJNC6yzBdsi3tisCL8NUg7st33tLh6qaUhoSuU` &&
                 `oVqtll1nUJiCNXcVJe7h21KhaLnu+OLg297ItjFI1pmWZ1WO35y3nL1xRVEI9QlnZZheEatOCXOLhpzE8oSw1L5i7nspsm1+FxQ5F4LzDBfQgoYRkI/y6K5vdhf+N8AfNmv0xpyFHKIAAAAASUVORK5CYII=`.

      WHEN c_image_keys-super.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABYElEQVR4nGNgoCcISEgQ8I2Kne/g4MBCsmaQJp+o2N1AA/6DDMFQcGy5x9sjS53/H1/p9QibAVGpOdOB+D8CZ5ejKABp/vf3/X8QnV/Wch+MS1s+wuQzCioU0vKrbdILqh6D2Cm5DTIoBqzvUvq2eY` &&
                 `Le+w3d6h8KKtoVQDi/rPkxshqQWG55x32sftzcoyayvl1BYX2/ggBMDOQKZDUZFf0KmRUTsBuALQwwDZiukFo5E7sB2MIgr6z5O7KahIr5CglVi7AbQEwYJFQsV4iuWkV+GDg07GeJLlsrg6kbB0A2QKjMQUa20nGKXKXjI5lyh26BAgcBfHoxDFCodFwRODX2d++O` &&
                 `2v8eE8N/ggwhwoDm37AAlSi1+1uyIvd/5er8/5mL0v9Ll9k/JWhAblmLBSxAZUod9uk3e/x36Q38r9Po9lu8xH4WQQOQgWihpYpUhf1OsRLbH5Ll9qtBYQISBwDP0MXt5h1aTgAAAABJRU5ErkJggg==`.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
