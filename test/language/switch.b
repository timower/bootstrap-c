// RUN: %bootstrap %s | lli | FileCheck %s
// Test comprehensive switch statement functionality
// CHECK: === Integer Switch Tests ===
// CHECK-NEXT: One
// CHECK-NEXT: Two or Three
// CHECK-NEXT: Two or Three
// CHECK-NEXT: Five
// CHECK-NEXT: Other: 10
// CHECK: === Enum Switch Tests ===
// CHECK-NEXT: Task is pending
// CHECK-NEXT: Task is running
// CHECK-NEXT: Task completed successfully
// CHECK-NEXT: Task failed
// CHECK: === Union Switch Tests ===
// CHECK-NEXT: Success: 200 - OK
// CHECK-NEXT: Error: 404 - Not Found
// CHECK-NEXT: Warning level: 2
// CHECK-NEXT: Info message
// CHECK: === Multiple Case Values Tests ===
// CHECK-NEXT: Day 1: Weekday
// CHECK-NEXT: Day 2: Weekday
// CHECK-NEXT: Day 3: Weekday
// CHECK-NEXT: Day 4: Weekday
// CHECK-NEXT: Day 5: Weekday
// CHECK-NEXT: Day 6: Weekend
// CHECK-NEXT: Day 7: Weekend
// CHECK: === Arithmetic Expression Cases ===
// CHECK-NEXT: x equals 10 + 5 = 15
// CHECK: === Nested Switch Tests ===
// CHECK-NEXT: Task 42 is running with high priority
// CHECK: === Fallthrough Tests ===
// CHECK-NEXT: Case 2
// CHECK: === Let Expression Tests ===
// CHECK-NEXT: Got success result in let expression
// CHECK-NEXT: Created
// CHECK: === Character Switch Tests ===
// CHECK-NEXT: 'a' is a vowel
// CHECK-NEXT: 'E' is a vowel
// CHECK-NEXT: '5' is a digit
// CHECK-NEXT: Space character
// CHECK-NEXT: Newline character
// CHECK: All switch tests completed
extern func printf(format: i8*, ...) -> i32;

enum Status {
  PENDING,
  RUNNING,
  COMPLETED,
  FAILED,
}

enum Priority {
  LOW,
  MEDIUM,
  HIGH,
}

union Result {
  Success {
    code: i32;
    message: [i8];
  }
  Error {
    error_code: i32;
    details: [i8];
  }
  Warning {
    level: i32;
  }
  Info {}
}

struct Task {
  id: i32;
  status: Status;
  priority: Priority;
}


// Test basic integer switch
func testIntegerSwitch() {
  printf("=== Integer Switch Tests ===\n");

  let numbers: i32[5] = [ 1, 2, 3, 5, 10 ];
  for (let i = 0; i < 5; i++) {
    let num = numbers[i];
    switch (num) {
      case 1:
        printf("One\n");
        break;
      case 2, 3:
        printf("Two or Three\n");
        break;
      case 5:
        printf("Five\n");
        break;
      default:
        printf("Other: %d\n", num);
    }
  }
}


// Test enum switch
func testEnumSwitch() {
  printf("=== Enum Switch Tests ===\n");

  let statuses: Status[4] = [ Status::PENDING, Status::RUNNING, Status::COMPLETED, Status::FAILED ];
  for (let i = 0; i < 4; i++) {
    let status = statuses[i];
    switch (status) {
      case Status::PENDING:
        printf("Task is pending\n");
        break;
      case Status::RUNNING:
        printf("Task is running\n");
        break;
      case Status::COMPLETED:
        printf("Task completed successfully\n");
        break;
      case Status::FAILED:
        printf("Task failed\n");
        break;
    }
  }
}


// Test union switch with pattern matching
func testUnionSwitch() {
  printf("=== Union Switch Tests ===\n");

  // Test each union variant individually
  let success: Result = Result::Success {
    code = 200,
    message = "OK",
  };
  switch (success) {
    case Result::Success as s:
      printf("Success: %d - %s\n", s.code, s.message);
      break;
    case Result::Error as error:
      printf("Error: %d - %s\n", error.error_code, error.details);
      break;
    case Result::Warning as warning:
      printf("Warning level: %d\n", warning.level);
      break;
    case Result::Info:
      printf("Info message\n");
      break;
  }

  let error: Result = Result::Error {
    error_code = 404,
    details = "Not Found",
  };
  switch (error) {
    case Result::Success as s:
      printf("Success: %d - %s\n", s.code, s.message);
      break;
    case Result::Error as e:
      printf("Error: %d - %s\n", e.error_code, e.details);
      break;
    case Result::Warning as warning:
      printf("Warning level: %d\n", warning.level);
      break;
    case Result::Info:
      printf("Info message\n");
      break;
  }

  let warning: Result = Result::Warning {
    level = 2,
  };
  switch (warning) {
    case Result::Success as s:
      printf("Success: %d - %s\n", s.code, s.message);
      break;
    case Result::Error as e:
      printf("Error: %d - %s\n", e.error_code, e.details);
      break;
    case Result::Warning as w:
      printf("Warning level: %d\n", w.level);
      break;
    case Result::Info:
      printf("Info message\n");
      break;
  }

  let info: Result = Result::Info {};
  switch (info) {
    case Result::Success as s:
      printf("Success: %d - %s\n", s.code, s.message);
      break;
    case Result::Error as e:
      printf("Error: %d - %s\n", e.error_code, e.details);
      break;
    case Result::Warning as w:
      printf("Warning level: %d\n", w.level);
      break;
    case Result::Info:
      printf("Info message\n");
      break;
  }
}


// Test switch with multiple case values
func testMultipleCaseValues() {
  printf("=== Multiple Case Values Tests ===\n");

  for (let day = 1; day <= 7; day++) {
    switch (day) {
      case 1, 2, 3, 4, 5:
        printf("Day %d: Weekday\n", day);
        break;
      case 6, 7:
        printf("Day %d: Weekend\n", day);
        break;
    }
  }
}


// Test switch with arithmetic expressions
func testArithmeticCases() {
  printf("=== Arithmetic Expression Cases ===\n");

  let x = 15;
  switch (x) {
    case (10 + 5):
      printf("x equals 10 + 5 = 15\n");
      break;
    case (20 - 3):
      printf("x equals 20 - 3 = 17\n");
      break;
    case (4 * 4):
      printf("x equals 4 * 4 = 16\n");
      break;
    default:
      printf("x = %d doesn't match any arithmetic expression\n", x);
  }
}


// Test nested switch statements
func testNestedSwitch() {
  printf("=== Nested Switch Tests ===\n");

  let task = Task {
    id = 42,
    status = Status::RUNNING,
    priority = Priority::HIGH,
  };

  switch (task.status) {
    case Status::PENDING:
      printf("Task %d is pending\n", task.id);
      break;
    case Status::RUNNING:
      printf("Task %d is running with ", task.id);
      switch (task.priority) {
        case Priority::LOW:
          printf("low priority\n");
          break;
        case Priority::MEDIUM:
          printf("medium priority\n");
          break;
        case Priority::HIGH:
          printf("high priority\n");
          break;
      }
      break;
    case Status::COMPLETED, Status::FAILED:
      printf("Task %d is finished\n", task.id);
      break;
  }
}


// Test switch without break statements (fallthrough)
func testFallthrough() {
  printf("=== Fallthrough Tests ===\n");

  let value = 2;
  switch (value) {
    case 1:
      printf("Case 1\n");
    case 2:
      printf("Case 2\n");
    case 3:
      printf("Case 3\n");
      break;
    default:
      printf("Default case\n");
  }
}


// Test switch with let expressions
func testLetExpressions() {
  printf("=== Let Expression Tests ===\n");

  let result: Result = Result::Success {
    code = 201,
    message = "Created",
  };

  // Test union cast with let expression
  if (let success = result as Result::Success*) {
    printf("Got success result in let expression\n");
    switch (success->code) {
      case 200:
        printf("OK\n");
        break;
      case 201:
        printf("Created\n");
        break;
      case 202:
        printf("Accepted\n");
        break;
      default:
        printf("Other success code: %d\n", success->code);
    }
  }
}


// Test character switch
func testCharacterSwitch() {
  printf("=== Character Switch Tests ===\n");

  let chars: i32[5] = [ 'a', 'E', '5', ' ', '\n' ];
  for (let i = 0; i < 5; i++) {
    let ch = chars[i];
    switch (ch) {
      case 'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U':
        printf("'%c' is a vowel\n", ch);
        break;
      case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
        printf("'%c' is a digit\n", ch);
        break;
      case ' ':
        printf("Space character\n");
        break;
      case '\n':
        printf("Newline character\n");
        break;
      default:
        printf("'%c' is other\n", ch);
    }
  }
}

func main() -> i32 {
  testIntegerSwitch();
  testEnumSwitch();
  testUnionSwitch();
  testMultipleCaseValues();
  testArithmeticCases();
  testNestedSwitch();
  testFallthrough();
  testLetExpressions();
  testCharacterSwitch();

  printf("All switch tests completed\n");
  return 0;
}
