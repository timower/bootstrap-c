/**
 * @file Boostrap language parser
 * @author Timothy Werquin <foo@timower.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// Stolen from C.
// TODO: update if needed?
const PREC = {
  COMMA: -10,
  ASSIGNMENT: -2,
  CONDITIONAL: -1,
  DEFAULT: 0,
  LOGICAL_OR: 1,
  LOGICAL_AND: 2,
  INCLUSIVE_OR: 3,
  EXCLUSIVE_OR: 4,
  BITWISE_AND: 5,
  EQUAL: 6,
  RELATIONAL: 7,
  OFFSETOF: 8,
  SHIFT: 9,
  ADD: 10,
  MULTIPLY: 11,
  CAST: 12,
  SIZEOF: 13,
  UNARY: 14,
  CALL: 15,
  FIELD: 16,
  SUBSCRIPT: 17,
  SCOPE: 18,
  TYPE: 19
};

module.exports = grammar({
  name: "bootstrap",

  word: $ => $.identifier,

  extras: $ => [
    $.comment,
    /\s+/,
  ],

  supertypes: $ => [
    $._declaration,
    $.statement,
    $.expression,
  ],

  // TODO: get rid of this conflict using 'typeof'?
  conflicts: $ => [
    [$.expression, $._type_identifier],
    [$._enumerator, $._type_identifier],
  ],

  rules: {
    source_file: $ => repeat($._declaration),

    _declaration: $ => choice(
      $.func_decl,
      $.struct_decl,
      $.enum_decl,
      $.union_decl,
      $.import_decl,
      $.let_decl,
    ),

    let_decl: $ => seq(
      'let',
      $.identifier,
      optional(seq(':', $.type)),
      optional(seq('=', $._assignment)),
      ';'
    ),

    import_decl: $ => seq('import', $.identifier, ';'),

    _sub_struct: $ => seq(
      field('name', $._type_identifier),
      '{',
      repeat(seq(alias($.parameter, $.struct_field), ';')),
      '}',
    ),

    struct_decl: $ => seq(
      'struct',
      $._sub_struct,
      ';',
    ),

    enum_decl: $ => seq(
      'enum',
      field('name', $._type_identifier),
      '{',
      sep($._enumerator, ','),
      optional(','),
      '}',
      ';'
    ),

    union_decl: $ => seq(
      'union',
      field('name', $._type_identifier),
      '{',
      repeat(alias($._sub_struct, $.union_tag)),
      '}',
      ';'
    ),

    func_decl: $ => seq(
      'func',
      $.identifier,
      $.param_list,
      optional(seq('->', field('result', $.type))),
      choice($.block, ';'),
    ),

    block: $ => seq(
      '{',
      repeat($.statement),
      '}'
    ),

    statement: $ => choice(
      $.block,
      $.expression_statement,
      $.return_statement,
      $.switch_statement,
      $.for_statement,
      $.if_statement,
      $.while_statement,
      $.break_statement,
    ),

    for_statement: $ => seq(
      'for',
      '(',
      $.expression_statement,
      $.expression_statement,
      $.expression,
      ')',
      $.block
    ),

    if_statement: $ => prec.right(seq(
      'if',
      '(',
      $.expression,
      ')',
      $.statement,
      optional(seq(
        'else',
        $.statement
      ))
    )),

    while_statement: $ => seq(
      'while',
      '(',
      $.expression,
      ')',
      $.statement,
    ),

    switch_statement: $ => seq(
      'switch',
      '(',
      field('condition', $.expression),
      ')',
      '{',
      repeat(choice($.default_statement, $.case_statement)),
      '}',
    ),

    default_statement: $ => seq(
      'default',
      ':',
      repeat1($.statement),
    ),

    case_statement: $ => seq(
      'case',
      $.expression,
      ':',
      repeat1($.statement),
    ),

    return_statement: $ => seq(
      'return',
      optional($.expression),
      ';'
    ),

    break_statement: $ => seq('break', ';'),

    expression_statement: $ => seq(optional($.expression), ';'),

    param_list: $ => seq(
      '(',
      sep($.parameter, ','),
      optional($.variadic_param),
      ')',
    ),

    variadic_param: $ => seq(',', '...'),

    parameter: $ => seq(
      $.identifier,
      ':',
      $.type,
    ),

    type: $ => prec.right(seq(
      optional('const'),
      choice(
        $.primitive_type,
        $._type_identifier,
        seq($._type_identifier, '::', $._type_identifier),
        seq(choice('struct', 'union', 'enum'), $._type_identifier),
      ),
      repeat(choice('*', $._array_decl))
    )),

    _array_decl: $ => seq('[', optional($._number), ']'),

    _number: $ => /[-+]?[0-9]+/,

    primitive_type: $ => choice(
      ...[8, 16, 32, 64].map(n => `i${n}`),
      ...[8, 16, 32, 64].map(n => `u${n}`),
      'void',
    ),

    expression: $ => choice(
      $.number_literal,
      $.identifier,
      $.string_literal,
      $.char_literal,
      $.null,
      $.struct_expression,
      $.scope_expression,
      $.paren_expression,
      $.index_expression,
      $.call_expression,
      $.member_expression,
      $.update_expression,
      $.unary_expression,
      $.cast_expression,
      $.binary_expression,
      $.let_expression,
      $.conditional_expression,
      $.assignment_expression,
      $.comma_expression,
      $.sizeof_expression,
    ),

    _assignment: $ => choice(
      $.expression,
      $.array_expression,
    ),

    array_expression: $ => seq(
      '{',
      sep($.expression, ','),
      optional(','),
      '}',
    ),

    struct_expression: $ => seq(
      $._type_identifier,
      optional(seq('::', $._type_identifier)),
      '{',
      sep($.field_expression, ','),
      optional(','),
      '}'
    ),

    field_expression: $ =>  seq($.identifier, '=', $.expression),

    scope_expression: $ => prec(PREC.SCOPE, seq($._type_identifier, '::', $._enumerator)),

    paren_expression: $ => seq('(', $.expression, ')'),

    index_expression: $ => prec(PREC.SUBSCRIPT, seq($.expression, '[', $.expression, ']')),

    call_expression: $ => prec(PREC.CALL, seq(
      field('function', $.expression),
      '(',
      sep($.expression, ','),
      ')'
    )),

    member_expression: $ => prec(PREC.FIELD, seq($.expression, choice('->', '.'), $.identifier)),

    update_expression: $ => {
      const argument = field('argument', $.expression);
      const operator = field('operator', choice('--', '++'));
      return prec.right(PREC.UNARY, choice(
        seq(operator, argument),
        seq(argument, operator),
      ));
    },

    unary_expression: $ => prec.left(PREC.UNARY, seq(choice('&', '*', '+', '-', '~', '!'), $.expression)),

    cast_expression: $ => prec(PREC.CAST, seq($.expression, 'as', $.type)),

    binary_expression: $ => {
      const table = [
        ['+', PREC.ADD],
        ['-', PREC.ADD],
        ['*', PREC.MULTIPLY],
        ['/', PREC.MULTIPLY],
        ['%', PREC.MULTIPLY],
        ['||', PREC.LOGICAL_OR],
        ['&&', PREC.LOGICAL_AND],
        ['|', PREC.INCLUSIVE_OR],
        ['^', PREC.EXCLUSIVE_OR],
        ['&', PREC.BITWISE_AND],
        ['==', PREC.EQUAL],
        ['!=', PREC.EQUAL],
        ['>', PREC.RELATIONAL],
        ['>=', PREC.RELATIONAL],
        ['<=', PREC.RELATIONAL],
        ['<', PREC.RELATIONAL],
        ['<<', PREC.SHIFT],
        ['>>', PREC.SHIFT],
      ];

      return choice(...table.map(([operator, precedence]) => {
        return prec.left(precedence, seq(
          field('left', $.expression),
          // @ts-ignore
          field('operator', operator),
          field('right', $.expression),
        ));
      }));
    },

    sizeof_expression: $ => seq(
      'sizeof',
      '(',
      choice($.type, $.expression),
      ')',
    ),

    let_expression: $ => seq(
      'let',
      $.identifier,
      optional(seq(':', $.type)),
      '=',
      $._assignment
    ),

    conditional_expression: $ => prec.right(PREC.CONDITIONAL, seq(
      $.expression,
      '?',
      $.expression,
      ':',
      $.expression
    )),

    assignment_expression: $ => prec.right(PREC.ASSIGNMENT, seq(
      $.expression,
      choice(
         '=',
        '*=',
        '/=',
        '%=',
        '+=',
        '-=',
        '<<=',
        '>>=',
        '&=',
        '^=',
        '|=',
      ),
      $.expression
    )),

    comma_expression: $ => prec.left(PREC.COMMA, seq(
      $.expression,
      ',',
      $.expression
    )),

    string_literal: $ => seq(
      '"',
      repeat(choice(
        alias(token.immediate(prec(1, /[^\\"\n]+/)), $.string_content),
        $.escape_sequence,
      )),
      '"',
    ),

    escape_sequence: _ => token(prec(1, seq(
      '\\',
      choice(
        /[^xuU]/,
        /\d{2,3}/,
        /x[0-9a-fA-F]{1,4}/,
        /u[0-9a-fA-F]{4}/,
        /U[0-9a-fA-F]{8}/,
      ),
    ))),

    null: $ => "null",
    char_literal: $ => seq(
      '\'',
      choice(
        $.escape_sequence,
        token.immediate(/[^\n']/),
      ),
      '\''
    ),

    number_literal: $ => $._number,

    _type_identifier: $ => alias ($.identifier, $.type_identifier),
    _enumerator: $ => alias($.identifier, $.enumerator),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    comment: $ => token(seq('//', /.*/)),
  }
});

function sep(rule, seperator) {
  return optional(sep1(rule, seperator));
}

function sep1(rule, seperator) {
  return seq(rule, repeat(seq(seperator, rule)));
}
