INTERFACE zif_sat_c_operator
  PUBLIC.
  CONSTANTS equals TYPE voperator VALUE '=' ##NO_TEXT.
  CONSTANTS greater_than TYPE voperator VALUE '>' ##NO_TEXT.
  CONSTANTS greater_or_equal_to TYPE voperator VALUE '>=' ##NO_TEXT.
  CONSTANTS lesser_than TYPE voperator VALUE '<' ##NO_TEXT.
  CONSTANTS lesser_or_equal_to TYPE voperator VALUE '<=' ##NO_TEXT.
  CONSTANTS not_equals TYPE voperator VALUE '<>' ##NO_TEXT.
  CONSTANTS like TYPE voperator VALUE 'LIKE' ##NO_TEXT.
  CONSTANTS not_like TYPE voperator VALUE 'NOT LIKE' ##NO_TEXT.
  CONSTANTS between TYPE voperator VALUE 'BETWEEN' ##NO_TEXT.
ENDINTERFACE.
