#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "sandbox" package:
version = "$Version::version"
description = "sandbox"

archive(byte) = "core_ops.cmo"
archive(native) = "core_ops.cmx"
requires = ""

EOF
