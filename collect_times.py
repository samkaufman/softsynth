#!/usr/bin/env python3

import functools
import csv
import argparse
import pathlib
import re
import dataclasses
import os
import subprocess
import concurrent.futures
from typing import Any, Literal

TIMEOUT = 60 * 60 * 1  # 1 hour

arg_parser = argparse.ArgumentParser(
    description=(
        "Run Racket- and Rust-based implementations at many sizes, collecting times."
    )
)
arg_parser.add_argument("-o", "--output", type=pathlib.Path, default=None)
arg_parser.add_argument("--min-size", type=int, default=1)
arg_parser.add_argument("--min-loop-scans", type=int, default=1)
arg_parser.add_argument("max_program_size", type=int)
arg_parser.add_argument("max_loop_scans", type=int)


@dataclasses.dataclass(frozen=True)
class RustBenchmark:
    checkpoints: Literal["naive", "safe", "online", None]
    prog_size: int
    loop_scans: int

    def execute(self) -> dict[str, Any]:
        """Run and return synth. time in milliseconds."""
        args = []
        if self.checkpoints == "naive":
            args.append("--naive-checkpoints")
        if self.checkpoints == "safe":
            args.append("--safe-checkpoints")
        if self.checkpoints == "online":
            args.append("--online-checkpoints")
        args.append(str(self.prog_size))
        args.append(str(self.loop_scans * 3))
        cwd = os.path.join(os.path.dirname(os.path.abspath(__file__)), "rust")
        try:
            output = subprocess.run(
                "cargo run --release -- " + " ".join(args),
                check=True,
                cwd=cwd,
                shell=True,
                capture_output=True,
                timeout=TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            timed_out = True
            runtime_ms = TIMEOUT * 1000
            success = False
        else:
            stdout = output.stdout.decode("utf-8")
            success = "No correct program" not in stdout
            match = re.search(r"Time taken for prog. len \d+: ([\d\.]+)(\w+)", stdout)
            timed_out = False
            runtime_ms = float(match.group(1))
            units = match.group(2)
            if units == "µs":
                runtime_ms /= 1000.0
            elif units == "ms":
                pass
            elif units == "s":
                runtime_ms *= 1000.0
            else:
                raise ValueError(f"Unknown units {units}")
        return {
            "impl": f"rust-{self.checkpoints}"
            if self.checkpoints is not None
            else "rust",
            "prog_size": self.prog_size,
            "loop_scans": self.loop_scans,
            "runtime_ms": runtime_ms,
            "timed_out": timed_out,
            "success": success,
        }


@dataclasses.dataclass(frozen=True)
class RosetteBenchmark:
    prog_size: int
    loop_scans: int

    def execute(self) -> dict[str, Any]:
        """Run and return synth. time in milliseconds."""
        cwd = os.path.join(os.path.dirname(os.path.abspath(__file__)), "rosette")
        try:
            output = subprocess.run(
                f"racket softmax-interpreter.rkt {self.prog_size} {self.loop_scans}",
                check=True,
                cwd=cwd,
                shell=True,
                capture_output=True,
                timeout=TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            timed_out = True
            success = False
            runtime_ms = TIMEOUT * 1000
        else:
            timed_out = False
            stdout = output.stdout.decode("utf-8")
            success = "unsat" not in stdout
            match = re.search(r"Took ([\d\.]+) ms", stdout)
            runtime_ms = float(match.group(1))
        return {
            "impl": "rosette",
            "prog_size": self.prog_size,
            "loop_scans": self.loop_scans,
            "runtime_ms": runtime_ms,
            "timed_out": timed_out,
            "success": success,
        }


import sys


def main():
    args = arg_parser.parse_args()

    benchmarks = []
    for prog_size in range(args.min_size, args.max_program_size + 1):
        for loop_scans in range(args.min_loop_scans, args.max_loop_scans + 1):
            for checkpoints in [None, "naive", "safe", "online"]:
                benchmarks.append(RustBenchmark(checkpoints, prog_size, loop_scans))  # type: ignore
            benchmarks.append(RosetteBenchmark(prog_size, loop_scans))

    fieldnames = [
        "impl",
        "prog_size",
        "loop_scans",
        "runtime_ms",
        "timed_out",
        "success",
    ]
    writer_out = sys.stdout
    if args.output is not None:
        writer_out = open(args.output, "w")
    writer = csv.DictWriter(writer_out, fieldnames=fieldnames)
    writer.writeheader()

    with concurrent.futures.ThreadPoolExecutor(
        max_workers=(os.cpu_count() or 1)
    ) as executor:
        futures = [executor.submit(functools.partial(b.execute)) for b in benchmarks]
        for future in concurrent.futures.as_completed(futures):
            r = future.result()
            writer.writerow(r)
            writer_out.flush()


if __name__ == "__main__":
    main()
