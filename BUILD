# Description: orthotope-0.1.0.0
#

load("//tools/build_defs/testing:bzl_library.bzl", "bzl_library")
load(":package_description.bzl", "description")
load(
    "//tools/build_defs/haskell:cabal_package.bzl",
    "cabal_haskell_package",
)

package(default_visibility = ["//visibility:public"])

cabal_haskell_package(
    description = description,
    warnings = True,
)

bzl_library(
    name = "package_description_bzl",
    srcs = ["package_description.bzl"],
    parse_tests = False,
    visibility = ["//visibility:private"],
)
