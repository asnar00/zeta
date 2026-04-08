"""Shared test fixtures."""

import pytest


class Report:
    def __init__(self):
        self.languages = {}

    def add(self, title, zero_input, output, language="python"):
        self.languages.setdefault(language, []).append((title, zero_input, output))


def write_report(language, entries):
    with open(f"test_report_{language}.md", "w") as f:
        f.write(f"# test report: zero to {language}\n\n")
        for title, zero_input, output in entries:
            f.write(f"## {title}\n\n")
            f.write("### zero\n\n")
            f.write(f"```zero\n{zero_input.strip()}\n```\n\n")
            f.write(f"### {language}\n\n")
            f.write(f"```{language}\n{output.strip()}\n```\n\n")


@pytest.fixture(scope="session")
def report(request):
    r = Report()
    yield r
    for language, entries in r.languages.items():
        write_report(language, entries)
