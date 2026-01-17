#!/usr/bin/env python3
"""
Mutates a given ll file, and runs lit test.
If tests don't fail, a branch isn't tested.
"""

from subprocess import run

import multiprocessing
import argparse
import shutil
import sys
import re
import os

from utils import get_unreachables, FUNC_REGEX, BRANCH_REGEX, SWITCH_REGEX, CASE_REGEX


def split_file(file):
    with open(file, "r") as f:
        nsplits = sum(1 for line in f.readlines() if line.startswith("define"))

    res = run(["llvm-split", "--help"], capture_output=True)
    assert res.returncode == 0, "llvm-split doesn't work"

    cmd = [
        "llvm-split",
        "-j",
        str(nsplits),
        file,
        "-o",
        "build/split/",
    ]

    if "round-robin" in res.stdout.decode():
        cmd.append("--round-robin")

    print(f"Making {nsplits} splits:", " ".join(cmd))

    os.makedirs("build/split", exist_ok=True)
    run(cmd, check=True)
    return [f"build/split/{i}" for i in range(nsplits)]


def dis_file(file):
    out = f"{file}.ll"
    run(["llvm-dis", file, "-o", out], check=True)
    return out


def get_branches(file):
    with open(file, "r") as f:
        lines = f.readlines()
    unreachables = get_unreachables(lines)

    branches = []
    cur_unreachables = set()

    bb_regex = re.compile(r"\.(src_.*\.b)\.(\d+)")

    def branch_filter(label):
        if label in cur_unreachables:
            return True

        if "src_emit.b" in label:
            return True

        if m := bb_regex.search(label):
            file = m.group(1).replace("_", "/")
            line = int(m.group(2))
            with open(file, "r") as f:
                source = f.readlines()[line - 2]
                if "// opt:" in source:
                    return True

        return False

    def add_labels(start, end, labels):
        for label in labels:
            branches.append(
                (
                    f"{start}.{label}",
                    lines[:start] + [f"  br label %{label}\n"] + lines[end:],
                )
            )

    curSwitch = None
    for idx, line in enumerate(lines):
        if curSwitch is not None:
            if line.strip() == "]":
                labels = [lbl for lbl in curSwitch[1] if not branch_filter(lbl)]

                if len(labels) > 1:
                    add_labels(curSwitch[0], idx + 1, labels)

                curSwitch = None

            if m := CASE_REGEX.match(line):
                curSwitch[1].add(m.group(1))  # type: ignore

        if m := FUNC_REGEX.match(line):
            cur_unreachables = unreachables[m.group(1)]

        if m := BRANCH_REGEX.match(line):
            if any(branch_filter(lbl) for lbl in m.groups()):
                continue
            add_labels(idx, idx + 1, m.groups())

        if m := SWITCH_REGEX.match(line):
            curSwitch = (idx, set(m.groups()))

    return file, branches


def make_content(tpl):
    file, label, new_lines, others, idx = tpl
    input_name = f"{file}.mutated.{label}.ll"
    with open(input_name, "w") as f:
        f.writelines(new_lines)
    return file, input_name, others, idx


def compile(file):
    out = f"{file}.ll.o"
    res = run(["clang", "-O0", "-c", "-x", "ir", "-o", out, file], capture_output=True)
    if res.returncode != 0:
        print(f"Failed to compile {file}")
        print(res.stderr.decode())
        raise
    return out


def build_bin(tpl):
    file, input_name, others, idx = tpl
    bin_name = f"{input_name}.bin"

    args = ["clang", "-fuse-ld=lld", "-O0", "-o", bin_name, input_name] + others
    res = run(args, capture_output=True)
    if res.returncode != 0:
        print(f"Failed to compile {file}")
        print(" ".join(args))
        print(res.stderr.decode())
        raise
    return bin_name, idx


def run_test(tpl):
    bin_name, idx = tpl

    test_dir = f"{bin_name}.test"

    os.makedirs(test_dir, exist_ok=True)
    dest = f"{test_dir}/.lit_test_times.txt"
    if not os.path.exists(dest):
        shutil.copyfile("test/.inverted_test_times.txt", dest)

    test_result = run(
        [
            "lit",
            "--max-failures=1",
            # TODO: "--filter-out=all/self.b",
            "--timeout=5",
            "--threads=1",
            f"-DBOOTSTRAP={bin_name}",
            f"-DTEST_ROOT={test_dir}",
            "test/",
        ],
        capture_output=True,
    )
    if test_result.returncode == 0:
        print(f"Failed: {bin_name}:")
        return False, idx

    # print(test_result.stdout.decode())
    # print(test_result.stderr.decode())
    return True, idx


def progress_imap(pool, map_func, items):
    num_tasks = len(items)
    if num_tasks == 0:
        return

    is_tty = sys.stderr.isatty()
    try:
        for i, result in enumerate(pool.imap_unordered(map_func, items), 1):
            if is_tty:
                sys.stderr.write(f"\r{i:>4}/{num_tasks}|{i / num_tasks * 100:>3.0f}%| ")
                sys.stderr.flush()
            yield result
    finally:
        if is_tty:
            sys.stderr.write("\n")


def invert():
    test_times = {}
    with open("test/.lit_test_times.txt", "r") as f:
        for line in f:
            time, path = line.split(maxsplit=1)
            test_times[path.strip("\n")] = float(time)

    for path in test_times.keys():
        test_times[path] = 5.0 - test_times[path]

    with open("test/.inverted_test_times.txt", "w") as time_file:
        for name, time in test_times.items():
            time_file.write(("%e" % time) + " " + name + "\n")


def main(pool, args):
    invert()

    print("Splitting")
    files = split_file(args.ir_file)
    print("Disassembling")
    ir_files = pool.map(dis_file, files)

    print("Compiling")
    object_files = list(progress_imap(pool, compile, files))

    idx = 0
    branches = []
    for file, group in pool.map(get_branches, ir_files):
        others = [f"{f}.o" for f in ir_files if f != file]
        for label, lines in group:
            branches.append((file, label, lines, others, idx))
            idx += 1

    print(f"Found {len(branches)} branches")
    if args.start is not None:
        branches = branches[args.start :]

    if args.ntests is not None:
        branches = branches[: args.ntests]
    print(f"Testing {len(branches)} branches")

    sanity_tpl = ("build/sanity", "sanity", [], object_files, 0)
    sanity_ok, _ = run_test(build_bin(make_content(sanity_tpl)))
    if sanity_ok:
        print("Sanity failed")
        sys.exit(1)

    print("Making mutations")
    contents = pool.map(make_content, branches)

    num_failures = 0
    num_ran = 0

    print("Building...")
    bins = list(progress_imap(pool, build_bin, contents))

    try:
        print("Running...")
        for ok, idx in progress_imap(pool, run_test, bins):
            if not ok:
                print(f"Failed: {idx}")
                num_failures += 1
            num_ran += 1
    except KeyboardInterrupt:
        print("...Interupted")

    print(f"Found {num_failures}/{num_ran} failures")
    if num_failures != 0:
        sys.exit(1)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("ir_file", type=str, help="coverage.ll")
    parser.add_argument("--start", type=int, default=None)
    parser.add_argument("--ntests", type=int, default=None)
    parser.add_argument("-j", "--jobs", type=int, default=None)
    args = parser.parse_args()

    with multiprocessing.Pool(args.jobs) as p:
        main(p, args)
