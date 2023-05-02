/// \file

#include <fmt/core.h>
#include <fstream>
#include <iostream>
#include <istream>
#include <llvm/Support/raw_ostream.h>
#include <sstream>
#include <codecvt>
#include <locale>

#include "AST.hpp"
#include "code_generator.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SourceMgr.h"

enum Return_values : int {
  SUCCESS = 0,
  LEXER_ERROR = 1,
  PARSER_ERROR = 2,
  INTERPRETER_ERROR = 3,
  UNEXPECTED_ERROR = 4,
};

struct FlagInfo {
  std::optional<std::string> output_file;
  std::optional<std::string> input_file;
  llvm::OptimizationLevel optimization_level = llvm::OptimizationLevel::O2;
};

void print_usage() {
  fmt::print("Kavun programming language:\n"
             "Usage: kavun file.kvn\n");
}

FlagInfo read_flags(const std::span<std::string>& input) {
  FlagInfo result;
  bool specifying_output = false;
  for (const auto& str : input) {
    if (str == "-O0") {
      result.optimization_level = llvm::OptimizationLevel::O0;
    } else if (str == "-O1") {
      result.optimization_level = llvm::OptimizationLevel::O1;
    } else if (str == "-O2") {
      result.optimization_level = llvm::OptimizationLevel::O2;
    } else if (str == "-O3") {
      result.optimization_level = llvm::OptimizationLevel::O3;
    } else if (str == "-o"  || str == "--output") {
      specifying_output = true;
    } else if (specifying_output) {
      result.output_file = str;
      specifying_output = false;
    } else if (str[0] != '-'){
      result.input_file = str;
    } else {
      fmt::print("Unknown flag: {}\n", str);
      print_usage();
      exit(UNEXPECTED_ERROR);
    }
  }
  return result;
}

std::wstring slurp(const std::wistream& input) {
  std::wstringstream ss;
  ss << input.rdbuf();
  return ss.str();
}

Return_values run(FlagInfo fi) {
  std::wstring input_code;
  if (fi.input_file) {
    std::wifstream wif(fi.input_file.value());
    wif.imbue(std::locale());
    input_code = slurp(wif);
  }
  else {
    input_code = slurp(std::wcin);
  }

  try {
    Lexer lexer{};
    auto tokens = lexer.get_tokens(input_code);
    fmt::print(stderr, "[LEXER PASS]\n");

    Parser parser{};
    auto ast = parser.parse(tokens);
    fmt::print(stderr, "[PARSER PASS]\n");

    CodeGenerator code_generator(fi.optimization_level);
    code_generator(ast);

    fmt::print(stderr, "[INTERPRETER PASS]\n");

    if (fi.output_file) {
      std::error_code ec;
      llvm::raw_fd_ostream out(fi.output_file.value(), ec, llvm::sys::fs::FA_Write);
      llvm::WriteBitcodeToFile(*code_generator.the_module, out);
    } else {
      llvm::outs() << *code_generator.the_module << '\n'; 
    }
    return SUCCESS;
  } catch (lexer_exception &e) {
    std::wcerr << "[LEXER ERROR]  " << e.what() << '\n';
    return LEXER_ERROR;
  } catch (parser_exception &e) {
    std::wcerr << "[PARSER ERROR]  " << e.what() << '\n';
    return PARSER_ERROR;
  } catch (interpreter_exception &e) {
    std::wcerr << "[INTERPRETER ERROR]  " << e.what() << '\n';
    return INTERPRETER_ERROR;
  } catch (std::exception &e) {
    fmt::print(stderr, "[UNEXPECTED ERROR]  {}\n", e.what());
    return UNEXPECTED_ERROR;
  }
}

int main(int argc, char *argv[]) {
  std::setlocale(LC_ALL, "en_US.utf-8");
  std::locale::global(std::locale("en_US.utf-8"));
  std::cout.imbue(std::locale());
  std::cerr.imbue(std::locale());
  std::wcout.imbue(std::locale());
  std::wcerr.imbue(std::locale());

  std::vector<std::string> args;
  args.reserve(argc);
  for (auto arg = argv + 1; arg - argv < argc; ++arg) {
    args.emplace_back(*arg);
  }
  auto fi = read_flags(args);
  return run(fi);
}
