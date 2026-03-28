"""Shared test fixtures."""

import pytest


class Report:
    def __init__(self):
        self.entries = []

    def add(self, title, zero_input, py_output):
        self.entries.append((title, zero_input, py_output))


@pytest.fixture(scope="session")
def report(request):
    r = Report()
    yield r
    # write report after all tests complete
    with open("test_report.md", "w") as f:
        f.write("# test report\n")
        f.write("*zero to python translation examples*\n\n")
        for title, zero_input, py_output in r.entries:
            f.write(f"## {title}\n\n")
            f.write("### zero\n\n")
            f.write(f"```zero\n{zero_input.strip()}\n```\n\n")
            f.write("### python\n\n")
            f.write(f"```python\n{py_output.strip()}\n```\n\n")
