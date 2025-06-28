#!/usr/bin/env python3
"""
Parse coverage.txt and output functions with their coverage percentages.
Coverage is defined as non-zero BB counts / total BBs.
"""

import sys


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


def parse_function_group(group):
    """
    Parse a single function group.
    Returns (function_name, coverage_percentage, non_zero_bbs, total_bbs) or None.
    """

    assert group[0].startswith("pgo-view-raw-counts: ")
    func_name = group[0].split("pgo-view-raw-counts: ", 1)[1]

    assert group[1].startswith("Dump Function")

    assert group[2].startswith("Number of Basic Blocks:")
    num_bbs = int(group[2].split(": ", 1)[1])

    # Parse BB lines
    bb_counts = []
    for line in group[3:]:
        if not line.startswith("BB: "):
            break

        if "FakeNode" in line:
            continue

        if "Count=" in line:
            count_part = line.split("Count=", 1)[1].strip()
            count = int(count_part)
            bb_counts.append(count)
        else:
            bb_counts.append(0)

    # -1 for the FakeNode we skipped
    assert len(bb_counts) == num_bbs - 1, f"unmatched counts {num_bbs} {bb_counts}"

    non_zero_bbs = sum(1 for c in bb_counts if c > 0)
    total_bbs = len(bb_counts)
    assert total_bbs >= 1, "No basic blocks in function?"

    coverage_pct = (non_zero_bbs / total_bbs) * 100
    return (func_name, coverage_pct, non_zero_bbs, total_bbs)


def parse_coverage_file(filepath):
    """
    Parse the coverage file.
    Returns a list of (function_name, coverage_percentage) tuples.
    """
    with open(filepath, "r") as f:
        lines = [line.strip() for line in f.readlines()]

    groups = split_into_function_groups(lines)
    functions = []

    for group in groups:
        result = parse_function_group(group)
        if result:
            functions.append(result)

    return functions


def main():
    if len(sys.argv) != 2:
        print("Usage: python parse_coverage.py <coverage.txt>")
        sys.exit(1)

    filepath = sys.argv[1]

    try:
        functions = parse_coverage_file(filepath)

        # Sort by coverage percentage (descending)
        functions.sort(key=lambda x: x[1], reverse=True)

        print(f"{'Function':<30} {'Coverage':<10} {'Non-zero/Total':<15}")
        print("-" * 55)

        for func_name, coverage_pct, non_zero, total in functions:
            print(f"{func_name:<30} {coverage_pct:>7.1f}%    {non_zero:>3}/{total:<3}")

        # Calculate and print total coverage at the end
        total_non_zero = sum(non_zero for _, _, non_zero, _ in functions)
        total_blocks = sum(total for _, _, _, total in functions)
        overall_coverage = (
            (total_non_zero / total_blocks * 100) if total_blocks > 0 else 0
        )

        print("-" * 55)
        print(
            f"Total Coverage: {overall_coverage:.1f}% ({total_non_zero}/{total_blocks} blocks)"
        )

        # Exit with error if coverage is below 90%
        if overall_coverage < 90.0:
            print(f"Error: Coverage {overall_coverage:.1f}% is below required 90%")
            sys.exit(1)

    except FileNotFoundError:
        print(f"Error: File '{filepath}' not found")
        sys.exit(1)
    except Exception as e:
        print(f"Error parsing file: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
