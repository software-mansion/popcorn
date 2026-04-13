#!/bin/bash
# Build OTP/BEAM for WebAssembly (wasm32-emscripten).
#
# Usage:
#   scripts/build-beam.sh [OPTIONS] <build-mode>
#
# Build modes:
#   release     production build (-Os -flto)
#   debug       debug build (-O0 -g, assertions enabled)
#
# Options:
#   --with-crypto         include static OpenSSL + crypto/asn1 NIFs
#   --otp-tag <TAG>       OTP git tag to clone (default: OTP-28.3.1)
#   --source <path>       use local OTP source instead of cloning
#   --outdir <dir>        output directory (default: ./out)
#   -j <N>                parallel jobs
#   -h, --help            show this help
set -euo pipefail

# steps
# 1. clone the otp repo into otp/sources/otp-original or use local source from OTP_SOURCE (.env file)
# 1b. copy the repo into otp/sources/otp, set BEAM_DIR to that location
# 2. apply patches from otp/patches (current patches live in /Users/jgonet/Documents/dev/otp-wasm-demo/otp-wasm/patches/0001-emscripten-support.patch) - don't read other files from that project
# 3. run autoconf
# 4. bootstrap if "${BEAM_DIR}/bootstrap/bin/erlc" doesn't exist
# 5. run build-openssl.sh with correct options if we're doing build with openssl
# 6. run configure and xcomp conf
# 7. run make
