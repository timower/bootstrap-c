#!/usr/bin/env python3
"""
Parse coverage.txt and output functions with their coverage percentages.
Coverage is defined as non-zero BB counts / total BBs.
"""

from dataclasses import dataclass

import sys
import re

# group 1: BB name, group 2: Index, group 3: Count (opt)
BB_REGEX = re.compile(r"BB: (\S*)  Index=(\d+)(?:  Count=(\d+))?")
# group 1: from Index, group 2: to Index, group 3: Count
EDGE_REGEX = re.compile(r"Edge \d+: (\d+)-->(\d+).*Count=(\d+)")


@dataclass
class FuncCounts:
    name: str
    covered_bbs: int
    total_bbs: int
    covered_edges: int
    total_edges: int

    def get_coverage(self):
        assert self.total_edges != 0
        return (self.covered_edges / self.total_edges) * 100


def split_into_function_groups(lines):
    """Split lines into groups, each starting with 'pgo-view-raw-counts: '."""
    groups = []
    current_group = []

    for line in lines:
        if line.startswith("pgo-view-raw-counts: "):
            if current_group:
                groups.append(current_group)
            current_group = [line]
        else:
            current_group.append(line)

    if current_group:
        groups.append(current_group)

    return groups


def parse_function_group(group, unreachables):
    """
    Parse a single function group.
    Returns (function_name, coverage_percentage, non_zero_bbs, total_bbs) or None.
    """

    assert group[0].startswith("pgo-view-raw-counts: ")
    func_name = group[0].split("pgo-view-raw-counts: ", 1)[1]
    unreachableBBs = unreachables[func_name]

    assert group[1].startswith("Dump Function")

    assert group[2].startswith("Number of Basic Blocks:")
    num_bbs = int(group[2].split(": ", 1)[1])

    idx = 3
    # Parse BB lines
    bb_counts = []
    unreachable_idxs = set()
    while idx < len(group):
        line = group[idx]
        if not line.startswith("BB: "):
            break
        idx += 1

        match = BB_REGEX.match(line)
        assert match is not None, f"No match: {line}"

        bb_name = match.group(1)
        bb_idx = int(match.group(2))

        if bb_name == "FakeNode":
            assert bb_idx == 0
            continue

        count = int(match.group(3))

        if bb_name in unreachableBBs:
            unreachable_idxs.add(bb_idx)
            continue

        bb_counts.append(count)

    # -1 for the FakeNode we skipped
    assert len(bb_counts) == num_bbs - 1 - len(unreachableBBs), (
        f"unmatched counts {num_bbs} {len(bb_counts)} {len(unreachableBBs)}"
    )

    non_zero_bbs = sum(1 for c in bb_counts if c > 0)
    total_bbs = len(bb_counts)
    assert total_bbs >= 1, "No basic blocks in function?"

    # Parse edges
    assert group[idx].startswith("Number of Edges:"), (
        f"Expected edges at {idx}: {group[idx]}"
    )
    # num_edges = int(group[idx].split(": ", 1)[1])
    idx += 1
    edge_counts = []
    while idx < len(group):
        line = group[idx]
        idx += 1
        if not line.startswith("Edge "):
            break

        match = EDGE_REGEX.match(line)
        if match is None:
            assert "-*c" in line, f"No match not removed: {line}"
            continue

        frm = int(match.group(1))
        to = int(match.group(2))
        count = int(match.group(3))

        # Skip fakenode wich has index 0
        if frm == 0:
            continue
        if frm in unreachable_idxs or to in unreachable_idxs:
            continue

        edge_counts.append(count)

    non_zero_edges = sum(1 for c in edge_counts if c > 0)
    total_edges = len(edge_counts)

    return FuncCounts(
        func_name,
        non_zero_bbs,
        total_bbs,
        non_zero_edges,
        total_edges,
    )


def parse_coverage_file(filepath, unreachables):
    """
    Parse the coverage file.
    Returns a list of (function_name, coverage_percentage) tuples.
    """
    with open(filepath, "r") as f:
        lines = [line.strip() for line in f.readlines()]

    groups = split_into_function_groups(lines)
    functions = []

    for group in groups:
        result = parse_function_group(group, unreachables)
        if result:
            functions.append(result)

    return functions


def parse_unreachables(file):
    with open(file, "r") as f:
        lines = f.readlines()

    unreachable = re.compile(r" *unreachable| *call void @unreachable\(")
    label = re.compile(r"([^ ]+):")
    func = re.compile(r"define .* @(.*)\(")

    unreachables = {}

    currentFunc = None
    currentLabel = None
    for line in lines:
        if match := label.match(line):
            currentLabel = match.group(1)
        if match := func.match(line):
            currentFunc = match.group(1)
            unreachables[currentFunc] = set()
        if unreachable.match(line):
            unreachables[currentFunc].add(currentLabel)

    return unreachables


def main():
    if len(sys.argv) != 3:
        print("Usage: python parse_coverage.py <coverage.txt> <bootstrap.ll>")
        sys.exit(1)

    coverage_file = sys.argv[1]
    ir_file = sys.argv[2]

    try:
        unreachables = parse_unreachables(ir_file)
        functions = parse_coverage_file(coverage_file, unreachables)

        # Sort by coverage percentage (descending)
        functions.sort(
            key=lambda x: 100 * (x.total_bbs - x.covered_bbs)
            + (x.total_edges - x.covered_edges)
        )

        header = f"{'Function':<32} {'BBs':<7} {'Edges':<7} {'Coverage':<6}"

        print(header)
        print("-" * len(header))

        for func in functions:
            print(
                f"{func.name:<30} {func.covered_bbs:>3}/{func.total_bbs:<3} {func.covered_edges:>3}/{func.total_edges:<3} {func.get_coverage():>7.1f}%    "
            )

        # Calculate and print total coverage at the end
        total_non_zero = sum(func.covered_bbs for func in functions)
        total_blocks = sum(func.total_bbs for func in functions)
        overall_coverage = (
            (total_non_zero / total_blocks * 100) if total_blocks > 0 else 0
        )

        # Calculate and print total coverage at the end
        total_covered_edges = sum(func.covered_edges for func in functions)
        total_edges = sum(func.total_edges for func in functions)
        edge_coverage = (
            (total_covered_edges / total_edges * 100) if total_edges > 0 else 0
        )
        print("-" * len(header))
        print(
            f"BB Coverage: {overall_coverage:.2f}% ({total_non_zero}/{total_blocks} blocks)"
        )
        print(
            f"Edge Coverage: {edge_coverage:.2f}% ({total_covered_edges}/{total_edges} edges)"
        )

        # Exit with error if coverage is below 90%
        if round(overall_coverage, 1) < 90.0:
            print(f"Error: Coverage {overall_coverage:.2f}% is below required 90%")
            sys.exit(1)

    except FileNotFoundError:
        print(f"Error: File '{coverage_file}' not found")
        sys.exit(1)
    except Exception as e:
        print(f"Error parsing file: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
