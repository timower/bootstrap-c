import ast;

import sema.state;

func evalConstant(state: SemaState*, expr: ExprAST*) -> ExprAST* {
  switch (expr->kind) {
    case ExprKind::Cast as castExpr:
      let res = evalConstant(state, castExpr.expr);
      res->type = expr->type;
      return res;

    case ExprKind::Int as intExpr:
      return expr;

    case ExprKind::Paren as paren:
      return evalConstant(state, paren.expr);

    case ExprKind::Binary as binary:
      let lhs = evalConstant(state, binary.lhs);
      let rhs = evalConstant(state, binary.rhs);

      let lhsInt = lhs->kind as ExprKind::Int*;
      let rhsInt = rhs->kind as ExprKind::Int*;
      if (lhsInt == null || rhsInt == null) {
        unreachable("Eval expected ints");
      }

      if (isAssign(binary.op)) {
        failSemaExpr(
            state,
            expr,
            "Assign expression not supported in const context");
      }

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

        case TokenKind::LESS:
          result = lhsInt->value < rhsInt->value ? 1 : 0;
        case TokenKind::GREATER:
          result = lhsInt->value > rhsInt->value ? 1 : 0;
        case TokenKind::LE_OP:
          result = lhsInt->value <= rhsInt->value ? 1 : 0;
        case TokenKind::GE_OP:
          result = lhsInt->value >= rhsInt->value ? 1 : 0;

        case TokenKind::EQ_OP:
          result = lhsInt->value == rhsInt->value ? 1 : 0;
        case TokenKind::NE_OP:
          result = lhsInt->value != rhsInt->value ? 1 : 0;

        case TokenKind::AND_OP:
          result = (lhsInt->value != 0) && (rhsInt->value != 0) ? 1 : 0;
        case TokenKind::OR_OP:
          result = (lhsInt->value != 0) || (rhsInt->value != 0) ? 1 : 0;
        case TokenKind::COMMA:
          result = rhsInt->value;

        default:
          unreachable("Not a binop");
      }

      // Create new constant expression with computed value
      let constExpr = newExpr(state->astAlloc, ExprKind::Int {
        value = result,
        token = binary.op,
      });
      constExpr->location = expr->location;
      constExpr->type = expr->type;
      return constExpr;

    default:
      break;
  }

  // Not a constant expression
  failSemaExpr(state, expr, "Not a constant expression");
  return null;
}
