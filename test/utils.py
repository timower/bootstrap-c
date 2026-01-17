import re

# group 1: BB name, group 2: Index, group 3: Count (opt)
BB_REGEX = re.compile(r"BB: (\S*)  Index=(\d+)(?:  Count=(\d+))?")
# group 1: from Index, group 2: to Index, group 3: Count
EDGE_REGEX = re.compile(r"Edge \d+: (\d+)-->(\d+).*Count=(\d+)")

UNREACHABLE_REGEX = re.compile(r" *unreachable| *call void @unreachable\(")
SETJMP_REGEX = re.compile(r".*call i32 @setjmp\(")
LABEL_REGEX = re.compile(r"([^ ]+):")
FUNC_REGEX = re.compile(r"define .* @(.*)\(")

BRANCH_REGEX = re.compile(r" *br i1 \S*, label %(\S*), label %(\S*)")
SWITCH_REGEX = re.compile(r" *switch .*label %(\S*) \[")
CASE_REGEX = re.compile(r".*, label %(\S*)")


def get_unreachables(lines):
    unreachables = {}

    currentFunc = None
    currentLabel = None
    for line in lines:
        if match := LABEL_REGEX.match(line):
            currentLabel = match.group(1)
        if match := FUNC_REGEX.match(line):
            currentFunc = match.group(1)
            unreachables[currentFunc] = set()
        if UNREACHABLE_REGEX.match(line) or SETJMP_REGEX.match(line):
            unreachables[currentFunc].add(currentLabel)

    return unreachables
