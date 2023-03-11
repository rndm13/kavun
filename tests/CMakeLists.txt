include(CTest)

set (lexer_error_regex "[LEXER ERROR]")
set (lexer_pass_regex  "[LEXER PASS]")
set (parser_error_regex "[PARSER ERROR]")
set (parser_pass_regex  "[PARSER PASS]")
set (codegen_error_regex "[INTERPRETER ERROR]")
set (codegen_pass_regex  "[INTERPRETER PASS]")

# create the testing file and list of tests
create_test_sourcelist (PassingTests
  ./bool.kvn
  ./conditional.kvn
  ./empty_module.kvn
  ./extern_test.kvn
  ./factorial.kvn
  ./fibonacci.kvn
  ./fizzbuzz.kvn
  ./hello_world.kvn
  ./id_func.kvn
  ./int_return.kvn
  ./many_args.kvn
  ./only_main.kvn
  ./precedence.kvn
  ./rand.kvn
  ./ret_main.kvn
  ./simple_cond.kvn
  ./string.kvn
  ./unary_operator.kvn
  ./unknown_function.kvn
  ./valid_program1.kvn
  ./valid_program2.kvn
  ./valid_program3.kvn
  ./var_decl.kvn
  ./void_parameter.kvn
  ./void_return.kvn)

set (PassingTestsToRun ${PassingTests})

foreach (test ${PassingTestsToRun})
  get_filename_component (TName ${test} NAME_WE)
  add_test (NAME ${TName} COMMAND ${PROJECT_NAME} ${TName})
  set_property(TEST ${TName} PROPERTY PASS_REGULAR_EXPRESSION "${codegen_pass_regex}")
endforeach ()

create_test_sourcelist (FailingLexerTests
  ./lexer_fail_no_end_string.kvn
  ./lexer_fail_inv_char.kvn
  ./lexer_fail_2_points.kvn)

set (FailingLexerTestsToRun ${FailingLexerTests})

foreach (test ${FailingLexerTestsToRun})
  get_filename_component (TName ${test} NAME_WE)
  add_test (NAME ${TName} COMMAND ${PROJECT_NAME} ${TName})
  set_property(TEST ${TName} PROPERTY PASS_REGULAR_EXPRESSION "${lexer_error_regex}")
endforeach ()

create_test_sourcelist (FailingParserTests
  ./parser_fail_expression.kvn
  ./parser_fail_fn_decl.kvn)

set (FailingParserTestsToRun ${FailingParserTests})

foreach (test ${FailingParserTestsToRun})
  get_filename_component (TName ${test} NAME_WE)
  add_test (NAME ${TName} COMMAND ${PROJECT_NAME} ${TName})
  set_property(TEST ${TName} PROPERTY PASS_REGULAR_EXPRESSION "${parser_error_regex}")
endforeach ()
