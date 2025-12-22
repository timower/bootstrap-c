import ast;

import sema.state;

func evalConstant(state: SemaState*, expr: ExprAST*) -> ExprAST* {
  switch (expr->kind) {
    case ExprKind::Int as intExpr:
      // Already a constant
      return expr;

    case ExprKind::Scope as scopeExpr:
      // Enum values - already a constant
      return expr;

    case ExprKind::Binary as binary:
      let lhs = evalConstant(state, binary.lhs);
      let rhs = evalConstant(state, binary.rhs);

      if (let lhsInt = lhs->kind as ExprKind::Int*) {
        if (let rhsInt = rhs->kind as ExprKind::Int*) {
          let result: i32 = 0;
          switch (binary.op.kind) {
            case TokenKind::PLUS:
              result = lhsInt->value + rhsInt->value;
            case TokenKind::MINUS:
              result = lhsInt->value - rhsInt->value;
            case TokenKind::STAR:
              result = lhsInt->value * rhsInt->value;
            case TokenKind::SLASH:
              if (rhsInt->value == 0) {
                failSemaExpr(state, expr, "Division by zero in constant expression");
              }
              result = lhsInt->value / rhsInt->value;
            case TokenKind::PERCENT:
              if (rhsInt->value == 0) {
                failSemaExpr(state, expr, "Modulo by zero in constant expression");
              }
              result = lhsInt->value % rhsInt->value;
            case TokenKind::AND:
              result = lhsInt->value & rhsInt->value;
            case TokenKind::PIPE:
              result = lhsInt->value | rhsInt->value;
            case TokenKind::HAT:
              result = lhsInt->value ^ rhsInt->value;
            case TokenKind::LEFT_OP:
              result = lhsInt->value << rhsInt->value;
            case TokenKind::RIGHT_OP:
              result = lhsInt->value >> rhsInt->value;
            default:
              // Not a constant binary expression
              return expr;
          }

          // Create new constant expression with computed value
          let constExpr = newExpr(ExprKind::Int {
            value = result,
            token = binary.op,
          });
          constExpr->location = expr->location;
          constExpr->type = expr->type;
          return constExpr;
        }
      }
      return expr;

    default:
      // Not a constant expression
      failSemaExpr(state, expr, "Not a constant expression");
      return null;
  }
}
