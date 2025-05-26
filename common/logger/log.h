// SPDX-License-Identifier: LicenseRef-PATO-ESEO
// SPDX-Licence-CopyrightText: PATO ESEO License (see LICENSE.md)

#ifndef __LOG_H__
#define __LOG_H__

#include <stdio.h>
#include <stdarg.h>
#include <time.h>

// Number of indentation steps to cycle through (adjust as needed)
#define LOG_INDENT_STEPS 4

// Colored log level strings
#define LOG_INFO  "\033[1;37m[info]\033[0m"
#define LOG_WARN  "\033[1;33m[warn]\033[0m"
#define LOG_ERROR "\033[1;31m[error]\033[0m"
#define LOG_DEBUG "\033[1;34m[debug]\033[0m"

// Formatting characters
#define ASCII_TAB        "\t"   // Horizontal Tab
#define ASCII_NL         "\n"   // New Line (LF)
#define ASCII_CR         "\r"   // Carriage Return (CR)
#define ASCII_SPACE      " "    // Space
#define ASCII_BACKSPACE  "\b"   // Backspace
#define ASCII_FORMFEED   "\f"   // Form Feed
#define ASCII_VTAB       "\v"   // Vertical Tab

// Punctuation and symbols
#define ASCII_DOT        "."ASCII_SPACE    // Period
#define ASCII_COMMA      ","ASCII_SPACE    // Comma
#define ASCII_COLON      ASCII_SPACE":"ASCII_SPACE    // Colon
#define ASCII_SEMICOLON  ";"ASCII_SPACE    // Semicolon
#define ASCII_DASH       "-"    // Hyphen or dash
#define ASCII_UNDERSCORE "_"    // Underscore
#define ASCII_EQUALS     "="    // Equal sign
#define ASCII_PLUS       "+"    // Plus sign
#define ASCII_MINUS      "-"    // Minus (alias of DASH)
#define ASCII_STAR       "*"    // Asterisk
#define ASCII_SLASH      "/"    // Forward slash
#define ASCII_BACKSLASH  "\\"   // Backslash
#define ASCII_PIPE       "|"    // Pipe
#define ASCII_AMP        "&"    // Ampersand
#define ASCII_HASH       "#"    // Hash or pound
#define ASCII_AT         "@"    // At sign
#define ASCII_DOLLAR     "$"    // Dollar sign
#define ASCII_PERCENT    "%"    // Percent sign
#define ASCII_TILDE      "~"    // Tilde
#define ASCII_CARET      "^"    // Caret
#define ASCII_BACKTICK   "`"    // Backtick
#define ASCII_QUOTE      "\""   // Double quote
#define ASCII_SQUOTE     "'"    // Single quote
#define ASCII_EXCL       "!"    // Exclamation mark
#define ASCII_QMARK      ASCII_SPACE"?"ASCII_SPACE    // Question mark
#define ASCII_LT         "<"    // Less-than
#define ASCII_GT         ">"    // Greater-than
#define ASCII_LPAREN     "("    // Left parenthesis
#define ASCII_RPAREN     ")"    // Right parenthesis
#define ASCII_LBRACE     "{"    // Left brace
#define ASCII_RBRACE     "}"    // Right brace
#define ASCII_LBRACKET   "["    // Left bracket
#define ASCII_RBRACKET   "]"    // Right bracket

// Composite patterns (semantic formatting)
#define ASCII_L1ARROW    "<-"ASCII_SPACE   // Pointer or direction
#define ASCII_R1ARROW    "->"ASCII_SPACE   // Pointer or direction
#define ASCII_L2ARROW	 "<="ASCII_SPACE  // Association or mapping
#define ASCII_R2ARROW	 "=>"ASCII_SPACE  // Association or mapping
#define ASCII_SEPARATOR  "|"  // Column separator
#define ASCII_ASSIGN     ":=" // Assignment (pseudo syntax)
#define ASCII_KVSEP      ":"   // Key-Value separator
#define ASCII_PAIRSEP    ","ASCII_SPACE   // List separator
#define ASCII_RANGE      ".."   // Range or span
#define ASCII_EMPHASIS   "***"  // Emphasis (markdown-like)

// Logging macros
#define LOG_INFO_MSG(tag, fmt, ...)  log_print(LOG_INFO, tag, fmt, ##__VA_ARGS__)
#define LOG_WARN_MSG(tag, fmt, ...)  log_print(LOG_WARN, tag, fmt, ##__VA_ARGS__)
#define LOG_ERROR_MSG(tag, fmt, ...) log_print(LOG_ERROR, tag, fmt, ##__VA_ARGS__)
#ifdef DEBUG // Defined in CMakeLists.txt
#define LOG_DEBUG_MSG(tag, fmt, ...) log_print(LOG_DEBUG, tag, fmt, ##__VA_ARGS__)
#endif

// Print a log message with auto-indentation
void log_print(const char *level, const char *tag, const char *fmt, ...);

#endif /* __LOG_H__ */