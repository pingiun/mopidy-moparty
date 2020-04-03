import distutils.cmd
import subprocess

import setuptools.command.build_py
from setuptools import setup


class BuildPyCommand(setuptools.command.build_py.build_py):
    """Custom build command that runs Elm make."""

    def run(self):
        self.run_command("build_elm")
        setuptools.command.build_py.build_py.run(self)


class BuildElmCommand(distutils.cmd.Command):
    description = "run Elm make on Elm source"
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        subprocess.run(
            [
                "elm",
                "make",
                "--optimize",
                "--output",
                "moparty.js",
                "frontend/Main.elm",
            ]
        )
        compress = subprocess.Popen(
            [
                "uglifyjs",
                "moparty.js",
                "--compress",
                'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe',
            ],
            stdout=subprocess.PIPE,
        )
        mangle = subprocess.Popen(
            [
                "uglifyjs",
                "--mangle",
                "--output=mopidy_moparty/static/moparty.min.js",
            ],
            stdin=compress.stdout,
        )
        mangle.wait()


setup(cmdclass={"build_elm": BuildElmCommand, "build_py": BuildPyCommand})
