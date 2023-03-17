include(CTest)

set (lexer_error_regex   "LEXER ERROR")
set (lexer_pass_regex    "LEXER PASS")
set (parser_error_regex  "PARSER ERROR")
set (parser_pass_regex   "PARSER PASS")
set (codegen_error_regex "INTERPRETER ERROR")
set (codegen_pass_regex  "INTERPRETER PASS")

# create the testing file and list of tests
create_test_sourcelist (Tests
  assignment.kvn
  if_assignment.kvn
  bool.kvn
  conditional.kvn
  empty_module.kvn
  extern_test.kvn
  double_literal.kvn
  factorial.kvn
  for_loop.kvn
  fibonacci.kvn
  fizzbuzz.kvn
  hello_world.kvn
  id_func.kvn
  int_return.kvn
  many_args.kvn
  only_main.kvn
  precedence.kvn
  rand.kvn
  ret_main.kvn
  simple_cond.kvn
  string.kvn
  unary_operator.kvn
  valid_program1.kvn
  valid_program2.kvn
  valid_program3.kvn
  var_decl.kvn
  lexer_fail_no_end_string.kvn
  lexer_fail_inv_char.kvn
  lexer_fail_2_points.kvn
  parser_fail_expression.kvn
  parser_fail_no_comma_fn.kvn
  parser_fail_fn_decl.kvn
  parser_fail_assignment.kvn
  codegen_fail_void_return.kvn
  codegen_fail_unknown_function.kvn
  codegen_fail_function_as_var.kvn
  codegen_fail_not_all_codepaths_return.kvn)

set(TestsToRun ${Tests})

foreach (test ${TestsToRun})
  get_filename_component (TName ${test} NAME_WE)
  get_filename_component (TPath ${test} ABSOLUTE)
  add_test(NAME ${TName} COMMAND ${PROJECT_NAME} ${TPath})
  if(${TName} MATCHES "lexer_fail")
    set_property(TEST ${TName} PROPERTY PASS_REGULAR_EXPRESSION ${lexer_error_regex}) 
  elseif(${TName} MATCHES "parser_fail")
    set_property(TEST ${TName} PROPERTY PASS_REGULAR_EXPRESSION ${parser_error_regex}) 
  elseif(${TName} MATCHES "codegen_fail")
    set_property(TEST ${TName} PROPERTY PASS_REGULAR_EXPRESSION ${codegen_error_regex}) 
  endif()
endforeach ()
